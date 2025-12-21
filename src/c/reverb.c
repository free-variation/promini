/*
 * reverb.c - True stereo Dattorro plate reverb
 *
 * Based on Jon Dattorro's "Effect Design Part 1" paper (1997)
 * https://ccrma.stanford.edu/~dattorro/EffectDesignPart1.pdf
 *
 * Reference implementation: Valley Audio Plateau (MIT License)
 * https://github.com/ValleyAudio/ValleyRackFree/tree/main/src/Plateau
 *
 * True stereo: two independent channels with cross-feed coupling.
 * Each channel: predelay → input LPF → 4 diffusers → tank (2 halves)
 * Output: multiple taps from tank combined per Dattorro Table 2.
 */

#include "promini.h"

#define MAX_PREDELAY 9600  /* 200ms at 48kHz */
#define SCALED_DELAY(base, size) ((ma_uint32)((base) * (size)))

static const float mod_freq_ratios[4] = {1.0f, 1.5f, 1.2f, 1.8f};

/* ============================================================================
 * Basic DSP primitives
 * ============================================================================ */

/* Write to circular buffer */
static inline void delay_write(float *buffer, ma_uint32 mask, ma_uint32 t, float value) {
	buffer[t & mask] = value;
}

/* Read from circular buffer at offset */
static inline float delay_read(float *buffer, ma_uint32 mask, ma_uint32 t, ma_uint32 offset) {
	return buffer[(t + offset) & mask];
}

/* Allpass filter - Dattorro/Schroeder form */
static inline float allpass_process(float *buffer, ma_uint32 mask, ma_uint32 t,
                                     ma_uint32 delay, float gain, float input) {
	float delayed = delay_read(buffer, mask, t, delay);
	input += delayed * -gain;
	delay_write(buffer, mask, t, input);
	return delayed + input * gain;
}

/* ============================================================================
 * Delay buffer management
 * ============================================================================ */

/* Calculate buffer size (next power of 2) and mask */
static ma_uint32 calc_buffer_size(ma_uint32 delay) {
	ma_uint32 size = 1;
	while (size < delay) {
		size <<= 1;
	}
	return size;
}

/* Allocate delay buffer, returns mask (size - 1) or 0 on failure */
static float *alloc_buffer(ma_uint32 size) {
	float *buf = (float*)ma_malloc(size * sizeof(float), NULL);
	if (buf) {
		memset(buf, 0, size * sizeof(float));
	}
	return buf;
}

/* ============================================================================
 * Pitch shifter for shimmer effect
 * 4-grain overlap-add pitch shifting (see SuperCollider PitchShift for similar approach)
 * ============================================================================ */

/* Read from buffer with cubic interpolation (Catmull-Rom) */
static inline float buffer_read_cubic_ps(float *buffer, ma_uint32 size, float pos) {
	ma_uint32 i0, i1, i2, i3;
	float frac;
	float y0, y1, y2, y3;
	float c0, c1, c2, c3;

	/* Wrap negative positions */
	while (pos < 0.0f) pos += (float)size;
	i1 = (ma_uint32)pos % size;
	i0 = (i1 == 0) ? size - 1 : i1 - 1;
	i2 = (i1 + 1) % size;
	i3 = (i2 + 1) % size;
	frac = pos - (float)(ma_uint32)pos;

	y0 = buffer[i0];
	y1 = buffer[i1];
	y2 = buffer[i2];
	y3 = buffer[i3];

	/* Catmull-Rom spline coefficients */
	c0 = y1;
	c1 = 0.5f * (y2 - y0);
	c2 = y0 - 2.5f * y1 + 2.0f * y2 - 0.5f * y3;
	c3 = 0.5f * (y3 - y0) + 1.5f * (y1 - y2);

	return ((c3 * frac + c2) * frac + c1) * frac + c0;
}

/*
 * Pitch shifter using 4 overlapping grains (based on SuperCollider PitchShift).
 *
 * Four read heads staggered 90° apart. Each reads at shifted speed and is
 * windowed with a triangular ramp. Every framesize/4 samples, one head resets.
 * The overlapping triangular windows sum to constant amplitude.
 */
static float pitchshift_process(reverb_pitchshift_t *ps, float input,
                                 float shift_semitones, float mix,
                                 ma_uint32 sample_rate) {
	float ratio, pchratio1, samp_slope, startpos;
	float value;
	ma_uint32 mask;
	ma_uint32 framesize;
	float read_pos;
	float slope;
	int i;

	mask = ps->size - 1;

	/* Write input to buffer */
	ps->buffer[ps->write_pos & mask] = input;

	/* Bypass if no shift or no mix */
	if (shift_semitones == 0.0f || mix == 0.0f) {
		ps->write_pos++;
		return input;
	}

	/* Frame size determines grain length and crossfade rate */
	framesize = ps->size / 2;
	slope = 2.0f / (float)framesize;

	/* Convert semitones to ratio */
	ratio = SEMITONES_TO_RATIO(shift_semitones);
	pchratio1 = ratio - 1.0f;
	samp_slope = -pchratio1;

	/* Check if counter expired - time to reset one grain */
	if (ps->counter == 0) {
		ps->counter = framesize >> 2;  /* reset counter */
		ps->stage = (ps->stage + 1) & 3;

		/* Starting delay for new grain */
		startpos = (pchratio1 < 0.0f) ? 2.0f : (float)framesize * pchratio1 + 2.0f;

		/* Reset grain based on stage, with staggered ramp phases */
		switch (ps->stage) {
		case 0:
			ps->dsamp_slope[0] = samp_slope;
			ps->dsamp[0] = startpos;
			ps->ramp[0] = 0.0f;
			ps->ramp_slope[0] = slope;
			ps->ramp_slope[2] = -slope;
			break;
		case 1:
			ps->dsamp_slope[1] = samp_slope;
			ps->dsamp[1] = startpos;
			ps->ramp[1] = 0.0f;
			ps->ramp_slope[1] = slope;
			ps->ramp_slope[3] = -slope;
			break;
		case 2:
			ps->dsamp_slope[2] = samp_slope;
			ps->dsamp[2] = startpos;
			ps->ramp[2] = 0.0f;
			ps->ramp_slope[2] = slope;
			ps->ramp_slope[0] = -slope;
			break;
		case 3:
			ps->dsamp_slope[3] = samp_slope;
			ps->dsamp[3] = startpos;
			ps->ramp[3] = 0.0f;
			ps->ramp_slope[3] = slope;
			ps->ramp_slope[1] = -slope;
			break;
		}
	}

	/* Sum contribution from all 4 grains */
	value = 0.0f;
	for (i = 0; i < 4; i++) {
		/* Update delay position */
		ps->dsamp[i] += ps->dsamp_slope[i];

		/* read with cubic interpolation */
		read_pos = (float)ps->write_pos - ps->dsamp[i];
		while (read_pos < 0.0f) read_pos += (float)ps->size;

		/* add windowed sample */
		value += buffer_read_cubic_ps(ps->buffer, ps->size, read_pos) * ps->ramp[i];

		/* Update ramp */
		ps->ramp[i] += ps->ramp_slope[i];
		if (ps->ramp[i] < 0.0f) ps->ramp[i] = 0.0f;
		if (ps->ramp[i] > 1.0f) ps->ramp[i] = 1.0f;
	}

	ps->write_pos++;
	ps->counter--;

	/* Mix original and shifted */
	return input + (value - input) * mix;
}

/* ============================================================================
 * Initialization
 * ============================================================================ */

static void free_tank_half(reverb_tank_half_t *th) {
	if (th->decay_diff1_buf) { ma_free(th->decay_diff1_buf, NULL); th->decay_diff1_buf = NULL; }
	if (th->pre_damp.buffer) { ma_free(th->pre_damp.buffer, NULL); th->pre_damp.buffer = NULL; }
	if (th->decay_diff2_buf) { ma_free(th->decay_diff2_buf, NULL); th->decay_diff2_buf = NULL; }
	if (th->post_damp.buffer) { ma_free(th->post_damp.buffer, NULL); th->post_damp.buffer = NULL; }
}

static void free_channel(reverb_channel_t *ch) {
	int i;
	if (ch->predelay_buf) { ma_free(ch->predelay_buf, NULL); ch->predelay_buf = NULL; }
	for (i = 0; i < 4; i++) {
		if (ch->diffuser_buf[i]) { ma_free(ch->diffuser_buf[i], NULL); ch->diffuser_buf[i] = NULL; }
	}
	free_tank_half(&ch->tank[0]);
	free_tank_half(&ch->tank[1]);
}

static ma_result init_delay_line(reverb_delay_line_t *dl, ma_uint32 main_delay,
                                  ma_uint32 tap1, ma_uint32 tap2, ma_uint32 tap3) {
	ma_uint32 size = calc_buffer_size(main_delay * 2 + 1);  /* 2x for size scaling */
	dl->buffer = alloc_buffer(size);
	if (!dl->buffer) return MA_OUT_OF_MEMORY;
	dl->mask = size - 1;
	dl->main_delay_base = main_delay;
	dl->tap1_base = tap1;
	dl->tap2_base = tap2;
	dl->tap3_base = tap3;
	return MA_SUCCESS;
}

static ma_result init_tank_half(reverb_tank_half_t *th, int half_idx, ma_uint32 sample_rate) {
	ma_uint32 size;
	float scale = (float)sample_rate / 29761.0f;

	/* Dattorro's magic numbers scaled to sample rate */
	/* Half 0 (left side of tank) */
	ma_uint32 decay_diff1_delays[2] = { 672, 908 };
	ma_uint32 pre_damp_delays[2] = { 4453, 4217 };
	ma_uint32 pre_damp_tap1[2] = { 353, 266 };
	ma_uint32 pre_damp_tap2[2] = { 3627, 2974 };
	ma_uint32 pre_damp_tap3[2] = { 1990, 2111 };
	ma_uint32 decay_diff2_delays[2] = { 1800, 2656 };
	ma_uint32 decay_diff2_tap1[2] = { 187, 335 };
	ma_uint32 decay_diff2_tap2[2] = { 1228, 1913 };
	ma_uint32 post_damp_delays[2] = { 3720, 3163 };
	ma_uint32 post_damp_tap1[2] = { 1066, 121 };
	ma_uint32 post_damp_tap2[2] = { 2673, 1996 };

	/* Decay diffuser 1 */
	th->decay_diff1_delay = (ma_uint32)(decay_diff1_delays[half_idx] * scale);
	size = calc_buffer_size(th->decay_diff1_delay * 2 + 64);  /* 2x for size scaling + modulation */
	th->decay_diff1_buf = alloc_buffer(size);
	if (!th->decay_diff1_buf) return MA_OUT_OF_MEMORY;
	th->decay_diff1_mask = size - 1;
	th->decay_diff1_out = 0.0f;

	/* Pre-damping delay with taps */
	if (init_delay_line(&th->pre_damp,
	                    (ma_uint32)(pre_damp_delays[half_idx] * scale),
	                    (ma_uint32)(pre_damp_tap1[half_idx] * scale),
	                    (ma_uint32)(pre_damp_tap2[half_idx] * scale),
	                    (ma_uint32)(pre_damp_tap3[half_idx] * scale)) != MA_SUCCESS) {
		return MA_OUT_OF_MEMORY;
	}

	th->damping_state = 0.0f;

	/* Decay diffuser 2 with taps */
	th->decay_diff2_delay = (ma_uint32)(decay_diff2_delays[half_idx] * scale);
	size = calc_buffer_size(th->decay_diff2_delay * 2 + 1);	/* 2x for size scaling */
	th->decay_diff2_buf = alloc_buffer(size);
	if (!th->decay_diff2_buf) return MA_OUT_OF_MEMORY;
	th->decay_diff2_mask = size - 1;
	th->decay_diff2_tap1_base = SCALED_DELAY(decay_diff2_tap1[half_idx], scale);
	th->decay_diff2_tap2_base = SCALED_DELAY(decay_diff2_tap2[half_idx], scale);

	/* Post-damping delay with taps */
	if (init_delay_line(&th->post_damp,
	                    (ma_uint32)(post_damp_delays[half_idx] * scale),
	                    (ma_uint32)(post_damp_tap1[half_idx] * scale),
	                    (ma_uint32)(post_damp_tap2[half_idx] * scale),
	                    0) != MA_SUCCESS) {
		return MA_OUT_OF_MEMORY;
	}

	return MA_SUCCESS;
}

static ma_result init_channel(reverb_channel_t *ch, ma_uint32 sample_rate) {
	int i;
	ma_uint32 size;
	float scale = (float)sample_rate / 29761.0f;

	/* Input diffuser delays (Dattorro's values) */
	ma_uint32 diffuser_delays[4] = { 142, 107, 379, 277 };

	/* Predelay */
	size = calc_buffer_size(MAX_PREDELAY);
	ch->predelay_buf = alloc_buffer(size);
	if (!ch->predelay_buf) return MA_OUT_OF_MEMORY;
	ch->predelay_mask = size - 1;

	ch->input_lpf_state = 0.0f;

	/* 4 input diffusers */
	for (i = 0; i < 4; i++) {
		ma_uint32 delay = (ma_uint32)(diffuser_delays[i] * scale);
		size = calc_buffer_size(delay *2 + 1);	/* 2x for size scaling */
		ch->diffuser_buf[i] = alloc_buffer(size);
		if (!ch->diffuser_buf[i]) return MA_OUT_OF_MEMORY;
		ch->diffuser_mask[i] = size - 1;
		ch->diffuser_delay[i] = delay;
	}

	/* Tank halves */
	if (init_tank_half(&ch->tank[0], 0, sample_rate) != MA_SUCCESS) return MA_OUT_OF_MEMORY;
	if (init_tank_half(&ch->tank[1], 1, sample_rate) != MA_SUCCESS) return MA_OUT_OF_MEMORY;

	return MA_SUCCESS;
}

/* ============================================================================
 * Processing
 * ============================================================================ */

/* Process one sample through a tank half, return feedback for other half */
static float process_tank_half(reverb_tank_half_t *th, ma_uint32 t, float input,
                                float decay, float decay_diff1_gain,
                                float decay_diff2_gain, float damping, float hp,
                                ma_int32 mod_offset, float smooth_size) {
	float x;
	ma_uint32 mod_delay;

	/* Decay diffuser 1 with modulation */
	mod_delay = SCALED_DELAY(th->decay_diff1_delay, smooth_size) + mod_offset;
	if (mod_delay < 1) mod_delay = 1;
	x = allpass_process(th->decay_diff1_buf, th->decay_diff1_mask, t,
	                    mod_delay, decay_diff1_gain, input);
	th->decay_diff1_out = x;

	/* Pre-damping delay */
	delay_write(th->pre_damp.buffer, th->pre_damp.mask, t, x);
	x = delay_read(th->pre_damp.buffer, th->pre_damp.mask, t,
	               th->pre_damp.mask + 1 -
	               SCALED_DELAY(th->pre_damp.main_delay_base, smooth_size));

	/* Damping LPF */
	ONE_POLE(th->damping_state, x, damping);
	x = th->damping_state;
	x += 1e-25f;  /* prevent denormals */

	/* Tank highpass (bass decay control) */
	if (hp > 0.0f) {
		ONE_POLE(th->hp_state, x, hp);
		x -= th->hp_state;
	}

	/* Soft limiter for stability - only activate for extreme signals */
	if (fabsf(x) > 4.0f) {
		x = tanhf(x) * 4.0f;
	}

	/* Apply decay */
	x *= decay;

	/* Decay diffuser 2 */
	x = allpass_process(th->decay_diff2_buf, th->decay_diff2_mask, t,
	                    SCALED_DELAY(th->decay_diff2_delay, smooth_size),
	                    decay_diff2_gain, x);

	/* Post-damping delay */
	delay_write(th->post_damp.buffer, th->post_damp.mask, t, x);
	x = delay_read(th->post_damp.buffer, th->post_damp.mask, t,
	               th->post_damp.mask + 1 -
	               SCALED_DELAY(th->post_damp.main_delay_base, smooth_size));

	return x;
}

/* Get output taps from tank - Dattorro Table 2 */
static void get_tank_output(reverb_tank_half_t *tank, ma_uint32 t,
                            float smooth_size, float *out_l, float *out_r) {
	/* Left output: taps from tank[1] positive, tank[0] negative */
	*out_l = tank[1].decay_diff1_out;
	*out_l += delay_read(
			tank[1].pre_damp.buffer, tank[1].pre_damp.mask, t,
			tank[1].pre_damp.mask + 1 -
			SCALED_DELAY(tank[1].pre_damp.tap1_base, smooth_size));
	*out_l += delay_read(
			tank[1].pre_damp.buffer, tank[1].pre_damp.mask, t,
			tank[1].pre_damp.mask + 1 -
			SCALED_DELAY(tank[1].pre_damp.tap2_base, smooth_size));
	*out_l -= delay_read(
			tank[1].decay_diff2_buf, tank[1].decay_diff2_mask, t,
			tank[1].decay_diff2_mask + 1 -
			SCALED_DELAY(tank[1].decay_diff2_tap2_base, smooth_size));
	*out_l += delay_read(
			tank[1].post_damp.buffer, tank[1].post_damp.mask, t,
			tank[1].post_damp.mask + 1 -
			SCALED_DELAY(tank[1].post_damp.tap2_base, smooth_size));
	*out_l -= delay_read(
			tank[0].pre_damp.buffer, tank[0].pre_damp.mask, t,
			tank[0].pre_damp.mask + 1 -
			SCALED_DELAY(tank[0].pre_damp.tap3_base, smooth_size));
	*out_l -= delay_read(
			tank[0].decay_diff2_buf, tank[0].decay_diff2_mask, t,
			tank[0].decay_diff2_mask + 1 -
			SCALED_DELAY(tank[0].decay_diff2_tap1_base, smooth_size));
	*out_l += delay_read(
			tank[0].post_damp.buffer, tank[0].post_damp.mask, t,
			tank[0].post_damp.mask + 1 -
			SCALED_DELAY(tank[0].post_damp.tap1_base, smooth_size));

	/* Right output: taps from tank[0] positive, tank[1] negative */
	*out_r = tank[0].decay_diff1_out;
	*out_r += delay_read(
			tank[0].pre_damp.buffer, tank[0].pre_damp.mask, t,
			tank[0].pre_damp.mask + 1 -
			SCALED_DELAY(tank[0].pre_damp.tap1_base, smooth_size));
	*out_r += delay_read(
			tank[0].pre_damp.buffer, tank[0].pre_damp.mask, t,
			tank[0].pre_damp.mask + 1 -
			SCALED_DELAY(tank[0].pre_damp.tap2_base, smooth_size));
	*out_r -= delay_read(
			tank[0].decay_diff2_buf, tank[0].decay_diff2_mask, t,
			tank[0].decay_diff2_mask + 1 -
			SCALED_DELAY(tank[0].decay_diff2_tap2_base, smooth_size));
	*out_r += delay_read(
			tank[0].post_damp.buffer, tank[0].post_damp.mask, t,
			tank[0].post_damp.mask + 1 -
			SCALED_DELAY(tank[0].post_damp.tap2_base, smooth_size));
	*out_r -= delay_read(
			tank[1].pre_damp.buffer, tank[1].pre_damp.mask, t,
			tank[1].pre_damp.mask + 1 -
			SCALED_DELAY(tank[1].pre_damp.tap3_base, smooth_size));
	*out_r -= delay_read(
			tank[1].decay_diff2_buf, tank[1].decay_diff2_mask, t,
			tank[1].decay_diff2_mask + 1 -
			SCALED_DELAY(tank[1].decay_diff2_tap1_base, smooth_size));
	*out_r += delay_read(
			tank[1].post_damp.buffer, tank[1].post_damp.mask, t,
			tank[1].post_damp.mask + 1 -
			SCALED_DELAY(tank[1].post_damp.tap1_base, smooth_size));
}

/* Process one sample through complete channel */
static void process_channel(reverb_channel_t *ch, ma_uint32 t, float input,
                            ma_uint32 predelay_samples, float bandwidth,
                            float input_diff1, float input_diff2,
                            float decay, float decay_diff1, float decay_diff2,
                            float damping, float hp,
                            ma_int32 mod_offset0, ma_int32 mod_offset1,
                            float smooth_size,
							ma_bool32 shimmer_in_loop, float shimmer_mix,
							float shimmer_xfade, float shimmer_pos1, float shimmer_pos2,
                            float *out_l, float *out_r) {
	float x, feedback0, feedback1;
	float shifted0_a, shifted0_b, shifted0;
	float shifted1_a, shifted1_b, shifted1;
	float base_pos0, base_pos1;
	ma_uint32 delay0, delay1;
	ma_uint32 scaled_delay;

	/* Predelay */
	delay_write(ch->predelay_buf, ch->predelay_mask, t, input);
	x = delay_read(ch->predelay_buf, ch->predelay_mask, t,
	               ch->predelay_mask + 1 - predelay_samples);

	/* Input bandwidth LPF */
	ONE_POLE(ch->input_lpf_state, x, bandwidth);
	x = ch->input_lpf_state;

	/* 4 input diffusers */
	x = allpass_process(
			ch->diffuser_buf[0], ch->diffuser_mask[0], t,
			SCALED_DELAY(ch->diffuser_delay[0], smooth_size),
			input_diff1, x);
	x = allpass_process(
			ch->diffuser_buf[1], ch->diffuser_mask[1], t,
			SCALED_DELAY(ch->diffuser_delay[1], smooth_size),
			input_diff1, x);
	x = allpass_process(
			ch->diffuser_buf[2], ch->diffuser_mask[2], t,
			SCALED_DELAY(ch->diffuser_delay[2], smooth_size),
			input_diff2, x);
	x = allpass_process(
			ch->diffuser_buf[3], ch->diffuser_mask[3], t,
			SCALED_DELAY(ch->diffuser_delay[3], smooth_size),
			input_diff2, x);

	/* Get cross-feedback from post-damping delays.
	 * Each tank half reads from the other's output for figure-8 topology. */
	delay0 = ch->tank[1].post_damp.mask + 1 -
		SCALED_DELAY(ch->tank[1].post_damp.main_delay_base, smooth_size);
	delay1 = ch->tank[0].post_damp.mask + 1 -
		SCALED_DELAY(ch->tank[0].post_damp.main_delay_base, smooth_size);

	feedback0 = delay_read(ch->tank[1].post_damp.buffer,
		ch->tank[1].post_damp.mask, t, delay0);
	feedback1 = delay_read(ch->tank[0].post_damp.buffer,
		ch->tank[0].post_damp.mask, t, delay1);

	/* In-loop shimmer: blend pitch-shifted feedback with unshifted.
	 * Two grains read at different positions, crossfaded with cosine window.
	 * Each recirculation shifts pitch further, building harmonic cascades. */
	if (shimmer_in_loop && shimmer_mix > 0.0f) {
		base_pos0 = (float)((t + delay0) & ch->tank[1].post_damp.mask);
		base_pos1 = (float)((t + delay1) & ch->tank[0].post_damp.mask);

		/* Grain A and B for tank[1] feedback, 180 degrees apart */
		shifted0_a = buffer_read_cubic_ps(ch->tank[1].post_damp.buffer,
			ch->tank[1].post_damp.mask + 1, base_pos0 - shimmer_pos1);
		shifted0_b = buffer_read_cubic_ps(ch->tank[1].post_damp.buffer,
			ch->tank[1].post_damp.mask + 1, base_pos0 - shimmer_pos2);
		shifted0 = shifted0_a * shimmer_xfade + shifted0_b * (1.0f - shimmer_xfade);

		/* Grain A and B for tank[0] feedback */
		shifted1_a = buffer_read_cubic_ps(ch->tank[0].post_damp.buffer,
			ch->tank[0].post_damp.mask + 1, base_pos1 - shimmer_pos1);
		shifted1_b = buffer_read_cubic_ps(ch->tank[0].post_damp.buffer,
			ch->tank[0].post_damp.mask + 1, base_pos1 - shimmer_pos2);
		shifted1 = shifted1_a * shimmer_xfade + shifted1_b * (1.0f - shimmer_xfade);

		/* Crossfade between dry feedback and shimmer */
		feedback0 = feedback0 * (1.0f - shimmer_mix) + shifted0 * shimmer_mix;
		feedback1 = feedback1 * (1.0f - shimmer_mix) + shifted1 * shimmer_mix;
	}

	feedback0 *= decay;
	feedback1 *= decay;

	/* Prevent denormals in feedback loop */
	feedback0 += 1e-25f;
	feedback1 += 1e-25f;

	/* Process tank halves with cross-feedback */
	process_tank_half(&ch->tank[0], t, x + feedback0, decay,
	                  decay_diff1, decay_diff2, damping, hp,
	                  mod_offset0, smooth_size);
	process_tank_half(&ch->tank[1], t, x + feedback1, decay,
	                  decay_diff1, decay_diff2, damping, hp,
	                  mod_offset1, smooth_size);

	/* Get stereo output from taps */
	get_tank_output(ch->tank, t, smooth_size, out_l, out_r);
}

/* ============================================================================
 * miniaudio node interface
 * ============================================================================ */

static void reverb_process_pcm_frames(
		ma_node *node,
		const float** frames_in,
		ma_uint32 *frame_count_in,
		float** frames_out,
		ma_uint32 *frame_count_out)
{
	reverb_node_t *reverb = (reverb_node_t*)node;
	ma_uint32 frame_count = *frame_count_in;
	const float *in = frames_in[0];
	float *out = frames_out[0];
	ma_uint32 i, j;
	ma_uint32 sample_rate;

	/* Get parameters */
	ma_uint32 predelay_samples;
	float bandwidth, damping;
	float input_diff1, input_diff2;
	float decay_diff1, decay_diff2;
	float decay;
	ma_int32 mod_offset[4];
	float dc_coeff;
	float size_target;

	float in_l, in_r;
	float wet_l0, wet_r0, wet_l1, wet_r1;
	float wet_l, wet_r;
	float mid, side;
	float decay_compensation;

	float shimmer_ratio, shimmer_window;
	float shimmer_xfade = 0.0f;
	float shimmer_pos1 = 0.0f; 
	float shimmer_pos2 = 0.0f;


	get_engine_format_info(NULL, NULL, &sample_rate);
	dc_coeff = expf(-2.0f * M_PI * 20.0f/ (float)sample_rate);

	predelay_samples = (ma_uint32)(reverb->predelay_ms * sample_rate / 1000.0f);
	if (predelay_samples > MAX_PREDELAY) predelay_samples = MAX_PREDELAY;
	if (predelay_samples < 1) predelay_samples = 1;

	/* bandwidth: 0=dark, 1=bright. Map to LPF coeff (higher = brighter) */
	bandwidth = 0.1f + reverb->bandwidth * 0.89f;

	/* damping: 0=bright, 1=dark. Map to LPF coeff (higher = less damping) */
	damping = 0.99f - reverb->damping * 0.89f;

	/* Diffusion coefficients - fixed for now, could be parameters */
	input_diff1 = 0.75f;
	input_diff2 = 0.625f;
	decay_diff1 = 0.70f;
	decay_diff2 = 0.50f;

	/* Compensate output for high decay values.
	 * Energy in feedback loop ~ 1/(1-d²), so attenuate by sqrt(1-d²)
	 * Use user's decay setting, not freeze-modified value */
	decay_compensation = sqrtf(1.0f - reverb->decay * reverb->decay);

	decay = reverb->decay;

	/* Freeze mode: infinite decay, no new input, no damping */
	if (reverb->freeze) {
		decay = 1.0f;
		damping = 1.0f;
	}

	/* smooth size parameter */
	size_target = CLAMP(reverb->size, 0.0f, 2.0f);

	for (i = 0; i < frame_count; i++) {
		if (reverb->freeze) {
			in_l = 0.0f;
			in_r = 0.0f;
		} else {
			in_l = in[i * 2];
			in_r = in[i * 2 + 1];
		}

		/* Slow modulation for chorus effect in tank */
		for (j = 0; j < 4; j++) {
			reverb->mod_phase[j] += reverb->mod_rate * mod_freq_ratios[j] / (float)sample_rate;
			if (reverb->mod_phase[j] >= 1.0f) reverb->mod_phase[j] -= 1.0f;
			mod_offset[j] = (ma_int32)(sinf(reverb->mod_phase[j] * 2.0f * M_PI) * reverb->mod_depth * 16.0f);
		}

		/* shimmer phase for in-loop pitch shift (freeze stops phase) */
		if (reverb->shimmer_in_loop && reverb->shimmer1_mix > 0.0f && !reverb->freeze) {
			shimmer_ratio = SEMITONES_TO_RATIO(reverb->shimmer1_shift);
			shimmer_window = 128.0f + (3410.0f - 128.0f) * reverb->smooth_size;
			reverb->shimmer_phase += (1.0f - shimmer_ratio) / shimmer_window;
			WRAP_PHASE(reverb->shimmer_phase);

			shimmer_xfade = 0.5f - 0.5f * cosf(reverb->shimmer_phase * 2.0f * M_PI);
			shimmer_pos1 = reverb->shimmer_phase * shimmer_window;
			shimmer_pos2 = shimmer_pos1 + shimmer_window * 0.5f;
			if (shimmer_pos2 >= shimmer_window) shimmer_pos2 -= shimmer_window;
		}

		ONE_POLE(reverb->smooth_size, size_target, 0.001f);
		ONE_POLE(reverb->smooth_hp, reverb->hp, 0.001f);

		/* Process both channels */
		process_channel(&reverb->channels[0], reverb->t,
		                in_l + in_r * reverb->cross_feed,
		                predelay_samples, bandwidth,
		                input_diff1, input_diff2,
		                decay, decay_diff1, decay_diff2,
		                damping, reverb->smooth_hp,
		                mod_offset[0], mod_offset[1],
		                reverb->smooth_size,
						reverb->shimmer_in_loop, reverb->shimmer1_mix,
						shimmer_xfade, shimmer_pos1, shimmer_pos2,	
		                &wet_l0, &wet_r0);

		process_channel(&reverb->channels[1], reverb->t,
		                in_r + in_l * reverb->cross_feed,
		                predelay_samples, bandwidth,
		                input_diff1, input_diff2,
		                decay, decay_diff1, decay_diff2,
		                damping, reverb->smooth_hp,
		                mod_offset[2], mod_offset[3],
		                reverb->smooth_size,
						reverb->shimmer_in_loop, reverb->shimmer1_mix,
						shimmer_xfade, shimmer_pos1, shimmer_pos2,	
		                &wet_l1, &wet_r1);

		/* Combine stereo outputs from both channels */
		wet_l = (wet_l0 + wet_l1) * 0.5f;
		wet_r = (wet_r0 + wet_r1) * 0.5f;

		/* Apply shimmer pitch shifters.
		 * Skip shimmer1 if running in-loop (already applied to feedback). */
		if (!reverb->shimmer_in_loop) {
			wet_l = pitchshift_process(&reverb->shimmer[0][0], wet_l,
			                           reverb->shimmer1_shift, reverb->shimmer1_mix, sample_rate);
			wet_r = pitchshift_process(&reverb->shimmer[1][0], wet_r,
			                           reverb->shimmer1_shift, reverb->shimmer1_mix, sample_rate);
		}
		wet_l = pitchshift_process(&reverb->shimmer[0][1], wet_l,
		                           reverb->shimmer2_shift, reverb->shimmer2_mix, sample_rate);
		wet_r = pitchshift_process(&reverb->shimmer[1][1], wet_r,
		                           reverb->shimmer2_shift, reverb->shimmer2_mix, sample_rate);

		/* Stereo width with energy compensation */
		mid = (wet_l + wet_r) * 0.5f;
		side = (wet_l - wet_r) * 0.5f * reverb->width;
		float width_compensation = 1.0f / sqrtf((1.0f + reverb->width * reverb->width) * 0.5f);
		wet_l = (mid + side) * width_compensation;
		wet_r = (mid - side) * width_compensation;

		/* DC blocking (20Hz HPF) */
		reverb->dc_block_l = dc_coeff * reverb->dc_block_l + (1.0f - dc_coeff) * wet_l;
		reverb->dc_block_r = dc_coeff * reverb->dc_block_r + (1.0f - dc_coeff) * wet_r;
		wet_l -= reverb->dc_block_l;
		wet_r -= reverb->dc_block_r;

		/* Output tone shaping - HPF (low_cut) and LPF (high_cut) */
		if (reverb->low_cut > 0.0f) {
			float hpf_coeff = 1.0f - (2.0f * M_PI * reverb->low_cut / (float)sample_rate);
			reverb->hpf_l = hpf_coeff * reverb->hpf_l + (1.0f - hpf_coeff) * wet_l;
			reverb->hpf_r = hpf_coeff * reverb->hpf_r + (1.0f - hpf_coeff) * wet_r;
			wet_l -= reverb->hpf_l;
			wet_r -= reverb->hpf_r;
		}

		if (reverb->high_cut > 0.0f && reverb->high_cut < (float)sample_rate * 0.5f) {
			float lpf_coeff = 1.0f - expf(-2.0f * M_PI * reverb->high_cut / (float) sample_rate);
			ONE_POLE(reverb->lpf_l, wet_l, lpf_coeff);
			ONE_POLE(reverb->lpf_r, wet_r, lpf_coeff);
			wet_l = reverb->lpf_l;
			wet_r = reverb->lpf_r;
		}

		/* Mix wet and dry with decay compensation */
		out[i * 2] = in_l * reverb->dry + wet_l * reverb->wet * decay_compensation;
		out[i * 2 + 1] = in_r * reverb->dry + wet_r * reverb->wet * decay_compensation;

		reverb->t++;
	}

	*frame_count_out = frame_count;
}

ma_node_vtable reverb_vtable = {
	reverb_process_pcm_frames,
	NULL,
	1,
	1,
	0
};

/* ============================================================================
 * Public interface
 * ============================================================================ */

void free_reverb_node(reverb_node_t *reverb) {
	int i, j;
	free_channel(&reverb->channels[0]);
	free_channel(&reverb->channels[1]);
	for (i = 0; i < 2; i++) {
		for (j = 0; j < 2; j++) {
			if (reverb->shimmer[i][j].buffer) {
				ma_free(reverb->shimmer[i][j].buffer, NULL);
				reverb->shimmer[i][j].buffer = NULL;
			}
		}
	}
}

ma_result init_reverb_node(reverb_node_t *reverb, ma_uint32 sample_rate) {
	ma_result result;
	int i, j;
	ma_uint32 shimmer_size;

	/* Zero everything first */
	memset(&reverb->channels[0], 0, sizeof(reverb_channel_t));
	memset(&reverb->channels[1], 0, sizeof(reverb_channel_t));
	memset(&reverb->shimmer, 0, sizeof(reverb->shimmer));

	/* Initialize both channels */
	result = init_channel(&reverb->channels[0], sample_rate);
	if (result != MA_SUCCESS) {
		free_reverb_node(reverb);
		return result;
	}

	result = init_channel(&reverb->channels[1], sample_rate);
	if (result != MA_SUCCESS) {
		free_reverb_node(reverb);
		return result;
	}

	/* Initialize shimmer pitch shifters - buffer must be power of 2 */
	/* Use 4096 samples (~85ms at 48kHz) for good pitch shift range */
	shimmer_size = 4096;
	for (i = 0; i < 2; i++) {
		for (j = 0; j < 2; j++) {
			int k;
			ma_uint32 framesize = shimmer_size / 2;
			float slope = 2.0f / (float)framesize;

			reverb->shimmer[i][j].buffer = alloc_buffer(shimmer_size);
			if (!reverb->shimmer[i][j].buffer) {
				free_reverb_node(reverb);
				return MA_OUT_OF_MEMORY;
			}
			reverb->shimmer[i][j].size = shimmer_size;
			reverb->shimmer[i][j].write_pos = 0;

			/* Initialize 4 grains with staggered phases (90° apart) */
			/* At startup, assume no pitch shift - grains read from same position as write */
			for (k = 0; k < 4; k++) {
				reverb->shimmer[i][j].dsamp[k] = 2.0f;  /* small initial delay */
				reverb->shimmer[i][j].dsamp_slope[k] = 0.0f;  /* no movement until shift applied */
			}

			/* Stagger ramps: grain 0 at peak, grain 1 at 75%, grain 2 at 50%, grain 3 at 25% */
			reverb->shimmer[i][j].ramp[0] = 1.0f;
			reverb->shimmer[i][j].ramp[1] = 0.5f;
			reverb->shimmer[i][j].ramp[2] = 0.0f;
			reverb->shimmer[i][j].ramp[3] = 0.5f;

			/* Slopes: grain 0 falling, grain 1 rising, grain 2 rising, grain 3 falling */
			reverb->shimmer[i][j].ramp_slope[0] = -slope;
			reverb->shimmer[i][j].ramp_slope[1] = slope;
			reverb->shimmer[i][j].ramp_slope[2] = slope;
			reverb->shimmer[i][j].ramp_slope[3] = -slope;

			/* Counter starts partway through to quickly establish proper rhythm */
			reverb->shimmer[i][j].counter = framesize >> 3;
			reverb->shimmer[i][j].stage = 0;
		}
	}

	/* Default parameters */
	reverb->predelay_ms = 20.0f;
	reverb->bandwidth = 0.7f;
	reverb->decay = 0.5f;
	reverb->damping = 0.5f;
	reverb->mod_rate = 0.5f;
	reverb->mod_depth = 0.5f;
	reverb->shimmer1_shift = 0.0f;
	reverb->shimmer1_mix = 0.0f;
	reverb->shimmer2_shift = 0.0f;
	reverb->shimmer2_mix = 0.0f;
	reverb->width = 1.0f;
	reverb->cross_feed = 0.15f;
	reverb->low_cut = 80.0f;
	reverb->high_cut = 12000.0f;
	reverb->wet = 0.3f;
	reverb->dry = 1.0f;
	reverb->size = 1.0f;
	reverb->smooth_size = 1.0f;
	reverb->hp = 0.0f;
	reverb->smooth_hp = 0.0f;
	reverb->freeze = MA_FALSE;
	reverb->shimmer_in_loop = MA_FALSE;

	/* Initialize internal state */
	reverb->t = 0;
	reverb->mod_phase[0] = 0.0f;
	reverb->mod_phase[1] = 0.25f;
	reverb->mod_phase[2] = 0.5f;
	reverb->mod_phase[3] = 0.75f;
	reverb->shimmer_phase = 0.0f;
	reverb->hpf_l = 0.0f;
	reverb->hpf_r = 0.0f;
	reverb->lpf_l = 0.0f;
	reverb->lpf_r = 0.0f;
	reverb->dc_block_l = 0.0f;
	reverb->dc_block_r = 0.0f;

	return MA_SUCCESS;
}
