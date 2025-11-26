#include "sampler_internal.h"

static inline float onepole_lpf_process(filter_onepole_t* f, float input) {
	f->state += f->coefficient * (input - f->state);
	return f->state;
}

static inline float onepole_hpf_process(filter_onepole_t* f, float input) {
	f->state += f->coefficient * (input - f->state);
	return input - f->state;
}

/* write sample to delay line and advance cursor */
static inline void delay_write(delay_t* d, float sample) {
	d->buffer[d->cursor] = sample;
	d->cursor++;
	if (d->cursor >= d->size) d->cursor = 0;
}

/* Read sample from delay line at integer offset behind cursor */
static inline float delay_read(delay_t* d, ma_uint32 offset) {
	ma_uint32 index = d->cursor >= offset ? d->cursor - offset : d->cursor + d->size - offset;
	return d->buffer[index];
}

/* Read sample from delay line at fractional offset using cubic interpolation.
     Uses 4 neighboring samples to fit a smooth curve, then evaluates at the
     fractional position. Eliminates zipper noise when LFO modulates delay time. */
static inline float buffer_read_cubic(float * buffer, ma_uint32 size, ma_uint32 cursor, float offset) {
	ma_uint32 offset_int = (ma_uint32)offset;
	float frac = offset - offset_int;

	/* Four samples surrounding the fractional position */
	ma_uint32 i1 = cursor >= offset_int ? cursor - offset_int : cursor + size - offset_int;
	ma_uint32 i0 = i1 > 0 ? i1 - 1 : size - 1;
	ma_uint32 i2 = i1 < size - 1 ? i1 + 1 : 0;
	ma_uint32 i3 = i2 < size - 1 ? i2 + 1 : 0;

	float y0 = buffer[i0];
	float y1 = buffer[i1];
	float y2 = buffer[i2];
	float y3 = buffer[i3];

	/* Cubic polynomial coefficients (Catmull-Rom spline) */
	float c0 = y1;
	float c1 = 0.5f * (y2 - y0);
	float c2 = y0 - 2.5f * y1 + 2.0f * y2 - 0.5f * y3;
	float c3 = 0.5f * (y3 - y0) + 1.5f * (y1 - y2);

	/* Evaluate polynomial using Horner's method */
	return ((c3 * frac + c2) * frac + c1) * frac + c0;
}

static inline float delay_read_cubic(delay_t* d, float offset) {
	return buffer_read_cubic(d->buffer, d->size, d->cursor, offset);
}

static inline float allpass_read_cubic(filter_allpass_t* ap, float offset) {
	return buffer_read_cubic(ap->buffer, ap->size, ap->cursor, offset);
}

/* Allpass filter: passes all frequencies but shifts phase.
     Creates diffusion by smearing transients in time. */
static inline float allpass_process(filter_allpass_t* ap, float input) {
	float delayed = ap->buffer[ap->cursor];
	float output = -ap->coefficient * input + delayed * (1.0f + ap->coefficient);
	
	ap->buffer[ap->cursor] = input + delayed * ap->coefficient;
	
	ap->cursor++;
	if (ap->cursor >= ap->size) ap->cursor = 0;

	return output;
}

/* allpass with modulated delay time for chorus effect */
static inline float allpass_process_modulated(filter_allpass_t* ap, float input, float mod_offset) {
	float delayed = allpass_read_cubic(ap, (float)(ap->size -1) + mod_offset);
	float output = -ap->coefficient * input + delayed * (1.0f + ap->coefficient);
	
	ap->buffer[ap->cursor] = input + delayed * ap->coefficient;
	
	ap->cursor++;
	if (ap->cursor >= ap->size) ap->cursor = 0;

	return output;
}

/* Tick LFO and return bipolar value [-1 to +1] */
static inline float lfo_tick(lfo_t* lfo, ma_uint32 sample_rate) {
	float out = sinf(lfo->phase * 2.0f * M_PI);

	lfo->phase += lfo->rate / (float)sample_rate;
	if (lfo->phase >= 1.0f) lfo->phase -= 1.0f;

	return out * lfo->depth;
}

/* Delay-based pitch shifter using dual crossfaded read heads.
 Shift is in semitones, ratio = 2^(semitones/12).

 Pitch shifting works by reading from a delay buffer at a different
 speed than writing. Reading faster = higher pitch, slower = lower.

 Problem: the read head eventually catches up to (or falls behind)
 the write head, causing a discontinuity.

 Solution: two read heads 180° apart. As one approaches the write head
 and would glitch, we crossfade to the other head which is safely distant.
 The crossfade hides the discontinuity. */
static inline float pitchshift_process(pitchshift_t* ps, float input, ma_uint32 sample_rate)
{
	float ratio, read_speed;
	float head1_pos, head2_pos;
	float offset1, offset2;
	float head1_sample, head2_sample;
	float fade, shifted;

	if (ps->shift_semitones == 0.0f || ps->mix == 0.0f) {
		ps->buffer[(ma_uint32)ps->write_pos] = input;
		ps->write_pos += 1.0f;
		if (ps->write_pos >= ps->size) ps->write_pos -= ps->size;
		return input;
	}

	/* convert semitones to speed ratio. +12 semitones = 2x speed = octave up */
	ratio = powf(2.0f, ps->shift_semitones / 12.0f);
	read_speed = ratio;

	/* Two heads half a buffer apart */
	head1_pos = ps->read_pos;
	head2_pos = ps->read_pos + ps->size / 2.0f;
	if (head2_pos >= ps->size) head2_pos -= ps->size;

	/* calculate offset from write position of each head */
	offset1 = ps->write_pos - head1_pos;
	if (offset1 < 0) offset1 += ps->size;
	offset2 = ps->write_pos - head2_pos;
	if (offset2 < 0) offset2 += ps->size;

	head1_sample = buffer_read_cubic(ps->buffer, ps->size, (ma_uint32)ps->write_pos, offset1);
	head2_sample = buffer_read_cubic(ps->buffer, ps->size, (ma_uint32)ps->write_pos, offset2);

	/* triangle crossfade: phase 0-> 0.5 fades head1->head2; phase 0.5 -> 1 fades head2 + head1. */
	fade = ps->phase < 0.5f ? ps->phase * 2.0f : 2.0f - ps->phase * 2.0f;
	shifted = head1_sample * (1.0f - fade) + head2_sample * fade;

	/* write input to buffer */
	ps->buffer[(ma_uint32)ps->write_pos] = input;
	ps->write_pos += 1.0f;
	if (ps->write_pos >= ps->size) ps->write_pos -= ps->size;

	/* read head advanced at shifted speed */
	ps->read_pos += read_speed;
	if (ps->read_pos >= ps->size) ps->read_pos -= ps->size;

	/* phase tracks crossfade cycle */
	ps->phase += read_speed / ps->size;
	if (ps->phase >= 1.0f) ps->phase -= 1.0f;

	/* return shifted signal with dry */
	return input + (shifted - input) * ps->mix;
}

/* simple 2x upsampler: interpolate to create two output samples.
 * called once per input sample, produces two samples at double rate */
static inline void upsample_2x(float input, float* out1, float* out2, float* state) {
	*out1 = (*state + input) * 0.5f; /* interpolated midpoint */
	*out2 = input;
	*state = input;
}

/* Simple 2x downsampler: average then decimate.
   Takes two samples at double rate, outputs one at original rate. */
static inline float downsample_2x(float in1, float in2, float* state) {
	float out = (*state + in1 + in2) * 0.333333f;
	*state = in2;
	return out;
}

/* convert cutoff frequency to one-pole filter coefficient */
static inline float onepole_coefficient(float cutoff_hz, ma_uint32 sample_rate) {
	float x = expf(-2.0f * M_PI * cutoff_hz / (float) sample_rate);
	return 1.0f - x;
}

/* Process one half of the tank figure-8.
 * Signal flows: modulated allpass -> delay -> LPF -> allpass -> delay.
 * Returns output to feed the other half. Tap output taken after first delay. 
 */
static inline float tank_half_process(
		filter_allpass_t* diffusion1,
		delay_t* pre_delay,
		filter_onepole_t* lpf,
		filter_allpass_t* diffusion2,
		delay_t* post_delay,
		float input,
		float damping,
		float mod_offset, 
		float *tap_out)
{
	/* modulated allpass for chorus effect */
	float s = allpass_process_modulated(diffusion1, input, mod_offset);

	/* first delay line -- output tap taken here */
	delay_write(pre_delay, s);
	s = delay_read(pre_delay, pre_delay->size - 1);
	*tap_out = s;

	/* damping LPF simulates high frequency absorption */
	s = onepole_lpf_process(lpf, s);
	s = s * damping;

	/* second allpass for more diffusion */
	s = allpass_process(diffusion2, s);

	/* second delay line */
	delay_write(post_delay, s);
	s = delay_read(post_delay, post_delay->size - 1);

	return s;
}

/* process one sample through a single tank (figure-8 topology).
 * Left half output feeds right half input.
 * Right half output feeds back to left half input on next sample.
 * Decay controls feedback amount.
 */
static float tank_process(
		tank_t* tank,
		float input,
		float decay,
		float damping,
		float mod_offset)
{
	float tap1, tap2;
	float left_in, left_out, right_out;

	/* input with cross-feedback from previous frame */
	left_in = input + tank->feedback_sample * decay;

	left_out = tank_half_process(
			&tank->decay_diffusion1[0],
			&tank->pre_damping_delay[0],
			&tank->damping_lpf[0],
			&tank->decay_diffusion2[0],
			&tank->post_damping_delay[0], 
			left_in,
			damping,
			mod_offset,
			&tap1);

	/* right half uses inverted modulation for stereo decorrelation */
	right_out = tank_half_process(
			&tank->decay_diffusion1[1],
			&tank->pre_damping_delay[1],
			&tank->damping_lpf[1],
			&tank->decay_diffusion2[1],
			&tank->post_damping_delay[1], 
			left_out,
			damping,
			-mod_offset,
			&tap2);

	/* store for next frame's cross-feedback */
	tank->feedback_sample = right_out;

	/* difference of taps creates stereo width */
	return tap1 - tap2;
}

/* process one sample through a complete channel.
 * Predelay -> input LPF -> input diffusion -> 3 parallel tanks. */
static float channel_process(
		channel_t* ch,
		float input,
		float predelay_samples,
		float bandwidth,
		float input_diffusion_mix,
		float decay,
		float damping,
		float mod_offset)
{
	int i;
	float s, diffused, out;

	/* predelay */
	delay_write(&ch->predelay, input);
	s = delay_read(&ch->predelay, (ma_uint32) predelay_samples);

	/* input bandwidth limiting */
	s = onepole_lpf_process(&ch->input_lpf, s);

	/* input diffusion -- blend between raw and diffused */
	diffused = s;
	for (i = 0; i < 8; i++) {
		diffused = allpass_process(&ch->input_diffusion[i], diffused);
	}
	s = s + (diffused - s) * input_diffusion_mix;

	/* sum of 3 parallel tanks with detuned delay times */
	out = 0.0f;
	out += tank_process(&ch->tanks[0], s, decay, damping, mod_offset);
	out += tank_process(&ch->tanks[1], s, decay, damping, mod_offset * 1.07f);
	out += tank_process(&ch->tanks[2], s, decay, damping, mod_offset * 0.93f);

	return out * 0.3333333f; /* average of the three tanks */
}

/* Main reverb processing callback */
static void reverb_process_pcm_frames(
		ma_node* node,
		const float** frames_in,
		ma_uint32* frame_count_in,
		float** frames_out,
		ma_uint32* frame_count_out)
{
	reverb_node_t* reverb = (reverb_node_t*) node;
	ma_uint32 frame_count = *frame_count_in;
	const float* in = frames_in[0];
	float* out = frames_out[0];
	ma_uint32 i;
	ma_uint32 device_rate, sample_rate;
	float predelay_samples;
	float in_l, in_r;
	float up_l1, up_l2, up_r1, up_r2;
	float input_mod, tank_mod;
	float wet_l1, wet_l2, wet_r1, wet_r2;
	float wet_l, wet_r;
	float mid, side;

	get_engine_format_info(NULL, NULL, &device_rate);
	sample_rate = device_rate *2; /* 2x oversampled */
	predelay_samples = reverb->predelay_ms * (float)sample_rate / 1000.0f;

	for (i = 0; i < frame_count; i++) {
		in_l = in[i * 2];
		in_r = in[i * 2 + 1];

		/* upsample to 2x rate */
		upsample_2x(in_l, &up_l1, &up_l2, &reverb->upsample_state[0]);
		upsample_2x(in_r, &up_r1, &up_r2, &reverb->upsample_state[1]);

		/* get the LFO modulation values */
		input_mod = lfo_tick(&reverb->input_lfo, sample_rate);
		tank_mod = lfo_tick(&reverb->tank_lfo, sample_rate);

		/* process two samples at oversampled rate - first pair */
		wet_l1 = channel_process(
				&reverb->channels[0], 
				up_l1 + up_r1 * reverb->cross_feed,
				predelay_samples,
				reverb->bandwidth,
				reverb->input_diffusion_mix,
				reverb->decay,
				reverb->damping,
				tank_mod);
		wet_r1 = channel_process(
				&reverb->channels[1], 
				up_r1 + up_l1 * reverb->cross_feed,
				predelay_samples,
				reverb->bandwidth,
				reverb->input_diffusion_mix,
				reverb->decay,
				reverb->damping,
				tank_mod);

		/* process two samples at oversampled rate -- second pair */
		wet_l2 = channel_process(
				&reverb->channels[0], 
				up_l2 + up_r2 * reverb->cross_feed,
				predelay_samples,
				reverb->bandwidth,
				reverb->input_diffusion_mix,
				reverb->decay,
				reverb->damping,
				tank_mod);
		wet_r2 = channel_process(
				&reverb->channels[1], 
				up_r2 + up_l2 * reverb->cross_feed,
				predelay_samples,
				reverb->bandwidth,
				reverb->input_diffusion_mix,
				reverb->decay,
				reverb->damping,
				tank_mod);

		/* apply shimmer pitch shifters - first pair */
		wet_l1 = pitchshift_process(&reverb->shimmer[0][0], wet_l1, sample_rate);
		wet_l1 = pitchshift_process(&reverb->shimmer[0][1], wet_l1, sample_rate);
		wet_r1 = pitchshift_process(&reverb->shimmer[1][0], wet_r1, sample_rate);
		wet_r1 = pitchshift_process(&reverb->shimmer[1][1], wet_r1, sample_rate);

		/* apply shimmer pitch shifters - second pair */
		wet_l2 = pitchshift_process(&reverb->shimmer[0][0], wet_l2, sample_rate);
		wet_l2 = pitchshift_process(&reverb->shimmer[0][1], wet_l2, sample_rate);
		wet_r2 = pitchshift_process(&reverb->shimmer[1][0], wet_r2, sample_rate);
		wet_r2 = pitchshift_process(&reverb->shimmer[1][1], wet_r2, sample_rate);

		/* downsample to original rate */
		wet_l = downsample_2x(wet_l1, wet_l2, &reverb->downsample_state[0]);
		wet_r = downsample_2x(wet_r1, wet_r2, &reverb->downsample_state[1]);

		/* output filters */
		wet_l = onepole_hpf_process(&reverb->wet_hpf[0], wet_l);
		wet_l = onepole_lpf_process(&reverb->wet_lpf[0], wet_l);
		wet_r = onepole_hpf_process(&reverb->wet_hpf[1], wet_r);
		wet_r = onepole_lpf_process(&reverb->wet_lpf[1], wet_r);

		/* stereo width via mid-side */
		mid = (wet_l + wet_r) * 0.5f;
		side = (wet_l - wet_r) * 0.5f * reverb->width;
		wet_l = mid + side;
		wet_r = mid - side;

		/* mix wet and dry */
		out[i * 2] = in_l * reverb->dry + wet_l * reverb->wet;
		out[i * 2 + 1] = in_r * reverb->dry + wet_r * reverb->wet;
	}  

	*frame_count_out = frame_count;
}
