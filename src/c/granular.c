/*
 * granular.c - Granular delay processor
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#include "promini.h"

#define GET_GRANULAR_FROM_HANDLE(handle_term, granular_var, slot_var) \
	do { \
		if (!get_typed_handle(handle_term, "granular", &slot_var)) { \
			return PL_type_error("granular", handle_term); \
		} \
		if (slot_var < 0 || slot_var >= MAX_GRANULAR_DELAYS || !g_granular_delays[slot_var].in_use) { \
			return PL_existence_error("granular_delay", handle_term); \
		} \
		granular_var = &g_granular_delays[slot_var]; \
	} while(0)

/******************************************************************************
 * GLOBAL VARIABLES
 *****************************************************************************/

granular_delay_t g_granular_delays[MAX_GRANULAR_DELAYS] = {{0}};
pthread_mutex_t g_granular_mutex = PTHREAD_MUTEX_INITIALIZER;

/******************************************************************************
 * AUDIO PROCESSING
 *****************************************************************************/

/*
 * trigger_grain()
 * Find a free grain slot and start a new grain with current parameters.
 * Returns slot index, or -1 if no free slots.
 */
int trigger_grain(granular_delay_t *g)
{
	int i;
	grain_t *grain;
	float rand_pos;
	float rand_size;
	float rand_pan;
	ma_uint32 sample_rate;
	ma_format format;
	ma_uint32 channels;
	ma_uint64 offset;
	float max_position;
	int range, degree, octave, index;
	float semitone;

	/* find a free slot */
	for (i = 0; i < MAX_GRAINS; i++) {
		if (!g->grains[i].active) {
			grain = &g->grains[i];
			break;
		}
	}
	if (i == MAX_GRAINS) {
		return -1;
	}

	get_engine_format_info(&format, &channels, &sample_rate);

	/* apply spray: random in range [-spray, + spray] */
	rand_pos = g->position_spray * (2.0f * ((float)rand() / RAND_MAX) - 1.0f);
	rand_size = g->size_spray * (2.0f * ((float)rand() / RAND_MAX) - 1.0f);
	rand_pan = g->pan_spray * (2.0f * ((float)rand() / RAND_MAX) - 1.0f);

	max_position = (g->buffer.capacity_frames > 0 && g->frames_recorded > 0)
		? (float)g->frames_recorded / (float)g->buffer.capacity_frames
		: 0.0f;
	if (max_position < 0.01f) {
		return -1; /* not enough recorded */
	}

	/* set grain parameters */
	grain->source_type = GRAIN_SOURCE_RING_BUFFER;
	grain->source_index = 0;

	/* position is relative to write head: 0 = newest, 1 = oldest */
	offset = (ma_uint64)(CLAMP(g->position + rand_pos, 0.0f, max_position * 0.99f) * g->buffer.capacity_frames);
	grain->position = (g->buffer.write_pos + g->buffer.capacity_frames - offset) % g->buffer.capacity_frames;
	grain->size = (ma_uint64)(fmaxf(g->size_ms + rand_size, 1.0f) * sample_rate / 1000.0f);
	grain->cursor = 0.0f;

	/* calculate pitch from mode if enabled */
	if (g->mode_length > 0) {
       range = g->deviation_up + g->deviation_down + 1;
       degree = (rand() % range) - g->deviation_down;
       octave = (degree >= 0) ? degree / g->mode_length : -1 + (degree + 1) / g->mode_length;
       index = degree - octave * g->mode_length;
       semitone = g->pitch + octave * 12.0f + g->mode[index];
	   grain->pitch_ratio = SEMITONES_TO_RATIO(semitone);
	} else {
		grain->pitch_ratio = SEMITONES_TO_RATIO(g->pitch);
	}

	grain->envelope = g->envelope;
	grain->pan = CLAMP(g->pan + rand_pan, -1.0f, 1.0f);
	grain->envelope_phase = 0.0f;
	grain->direction = ((float)rand() / RAND_MAX < g->reverse_probability) ? -1 : 1;
	grain->active = MA_TRUE;

	/* reverse grains start at end and count backward */
	if (grain->direction == -1) {
		grain->cursor = (float)(grain->size - 1);
		grain->envelope_phase = 1.0f;
	}

	/* For frozen buffers, ensure grain doesn't cross the write head seam */
	if (!g->recording && g->frames_recorded >= g->buffer.capacity_frames) {
		ma_uint64 grain_samples = (ma_uint64)(grain->size * grain->pitch_ratio) + 4;
		ma_int64 dist_to_write;

		if (grain->direction == 1) {
			/* Forward grain: check distance from grain end to write pos */
			ma_uint64 grain_end = (grain->position + grain_samples) % g->buffer.capacity_frames;
			dist_to_write = (ma_int64)g->buffer.write_pos - (ma_int64)grain->position;
			if (dist_to_write < 0) dist_to_write += g->buffer.capacity_frames;

			if ((ma_uint64)dist_to_write < grain_samples) {
				/* Grain would cross seam, push back */
				grain->position = (g->buffer.write_pos + g->buffer.capacity_frames - grain_samples) % g->buffer.capacity_frames;
			}
		} else {
			/* Reverse grain: check distance from grain start to write pos */
			dist_to_write = (ma_int64)grain->position - (ma_int64)g->buffer.write_pos;
			if (dist_to_write < 0) dist_to_write += g->buffer.capacity_frames;

			if ((ma_uint64)dist_to_write < grain_samples) {
				/* Grain would cross seam, push forward */
				grain->position = (g->buffer.write_pos + grain_samples) % g->buffer.capacity_frames;
			}
		}
	}

	return i;
}

/* compute_envelope()
 * Calculate amplitude envelope for grain at given positon.
 * Shape: 
 * - 0.0 = square, 
 * - 0.5 = Hann window (smooth, zero at edges), 
 * - 1.0 = sawtooth (percussive)
 */
static float compute_envelope(float shape, float position)
{
	float square;
	float hann; 
	float sawtooth;

	/* position is 0.0 to 1.0 through the grain */

	/* square: tiny ramps at start and end to avoid clicks */
	if (position < 0.02f) {
		square = position / 0.02f;
	} else if (position > 0.98f) {
		square = (1.0f - position) / 0.02f;
	} else {
		square = 1.0f;
	}

	/* hann window for bell curve with zero at edges */
	hann = 0.5f * (1.0f - cos(M_PI * 2.0f * position));

	/* sawtooth: tiny attack ramp then linear decay to zero */
	if (position < 0.02f) {
		sawtooth = position / 0.02f;
	} else {
		sawtooth = 1.0f - (position - 0.02f) / 0.98f;
	}

	if (shape <= 0.5f) {
		return square + (hann - square) * (shape * 2.0f);
	} else {
		return hann + (sawtooth - hann) * ((shape - 0.5f) * 2.0f);
	}
}

/*
 * process_grain()
 * Process a single grain for one frame, return stereo sample.
 * Returns MA_FALSE if grain has finished.
 */
static ma_bool32 process_grain(granular_delay_t *g, grain_t *grain, float *out_left, float *out_right)
{
	float read_pos;
	float src_left;
	float src_right;
	float gain_l;
	float gain_r;
	float env;

	/* check if envelope has completed */
	if (grain->envelope_phase < 0.0f || grain->envelope_phase > 1.0f) {
		return MA_FALSE;
	}

	/* calculate envelope from phase (independent of pitch) */
	env = compute_envelope(grain->envelope, grain->envelope_phase);

	/* get read position */
	if (grain->source_type == GRAIN_SOURCE_RING_BUFFER) {
		read_pos = (float)grain->position + grain->cursor;

		src_left = ring_buffer_read_interpolated(&g->buffer, read_pos, 0);
		src_right = (g->buffer.channels > 1)
			? ring_buffer_read_interpolated(&g->buffer, read_pos, 1)
			: src_left;
	}  else {
		/*TODO data buffer source */
		return MA_FALSE;
	}

	/* apply envelope */
	src_left *= env;
	src_right *= env;

	/* apply pan: balance with cross-feed */
	if (grain->pan < 0.0f) {
		gain_l = 1.0f;
		gain_r = 1.0f + grain->pan;
	} else {
		gain_r = 1.0f;
		gain_l = 1.0f - grain->pan;
	}
	*out_left = src_left * gain_l + src_right * (1.0f - gain_r);
	*out_right = src_right * gain_r + src_left * (1.0f - gain_l);

	/* advance cursor by pitch ratio (for buffer reading) */
	grain->cursor += grain->pitch_ratio * grain->direction;

	/* advance envelope at fixed rate (independent of pitch) */
	grain->envelope_phase += grain->direction / (float)grain->size;

	return MA_TRUE;
}

/*
 * granular_process_pcm_frames()
 * Process audio: record input to ring buffer, generate grains.
 */
static void granular_process_pcm_frames(
		ma_node *node,
		const float** frames_in,
		ma_uint32 *frame_count_in,
		float** frames_out,
		ma_uint32 *frame_count_out)
{
	granular_delay_t *g = (granular_delay_t*)node;
	ma_uint32 frame_count = *frame_count_out;
	ma_uint32 channels = g->buffer.channels;
	float *output = frames_out[0];
	int i, j;
	float left, right;
	float grains_per_frame;
	float jitter;
	ma_format format;
	ma_uint32 sample_rate;
	int active_count = 0;
	float target_gain;
	float window_gain;
	float overlap;
	ma_uint32 c;

	/* record input to ring buffer if recording */
	if (g->recording && frames_in != NULL && frames_in[0] != NULL) {
		ring_buffer_write(&g->buffer, frames_in[0], *frame_count_in);

		if (g->frames_recorded < g->buffer.capacity_frames) {
			g->frames_recorded += frame_count;
			if (g->frames_recorded > g->buffer.capacity_frames) {
				g->frames_recorded = g->buffer.capacity_frames;
			}
		}
	}

	/* clear output buffer */
	memset(output, 0, frame_count * channels * sizeof(float));

	/* density-based triggering: grains per second -> grains per frame */
	grains_per_frame = 0.0f;
	if (g->density > 0.0f) {
		get_engine_format_info(&format, NULL, &sample_rate);
		grains_per_frame = g->density / (float)sample_rate;
	}
	
	/* process each frame */
	for (i = 0; i < frame_count; i++) {
		active_count = 0;

		/* density-based trigger */
		if (g->density > 0.0f) {
			g->density_accumulator += grains_per_frame;

			/* add random jitter for irregular timing */
			if (g->regularity < 1.0f) {
				jitter = (2.0f * ((float)rand() / RAND_MAX) - 1.0f);
				jitter *= grains_per_frame * (1.0f - g->regularity);
				g->density_accumulator += jitter;
			}

			while (g->density_accumulator > 1.0f) {
				trigger_grain(g);
				g->density_accumulator -= 1.0f;
			}
		}

		/* sum all active grains */
		for (j = 0; j < MAX_GRAINS; j++) {
			if (g->grains[j].active) {
				if (process_grain(g, &g->grains[j], &left, &right)) {
					output[i * channels] += left;
					if (channels > 1) {
						output[i * channels + 1] += right;
					}
					active_count++;
				} else {
					g->grains[j].active = MA_FALSE;
				}
			}
		}

		if (g->normalize) {
			/* asymmetric smoothing: moderate attack, slow release */
			if (active_count > g->num_grains_smoothed) {
				ONE_POLE(g->num_grains_smoothed, (float)active_count, 0.1f);
			} else {
				ONE_POLE(g->num_grains_smoothed, (float)active_count, 0.05f);
			}

			/* inverse sqrt normalization */
			target_gain = (g->num_grains_smoothed > 2.0f)
				? 1.0f / sqrtf(g->num_grains_smoothed - 1.0f)
				: 1.0f;

			/* window shape compensation, crossfaded by overlap amount (like Clouds) */
			/* overlap = expected number of concurrent grains */
			overlap = g->num_grains_smoothed;
			overlap = CLAMP(overlap, 0.0f, 4.0f) / 4.0f;  /* normalize to 0-1 */
			window_gain = 1.0f + g->envelope;  /* 1.0 to 2.0 */
			/* at low overlap, no compensation; at high overlap, full compensation */
			target_gain *= 1.0f + (window_gain - 1.0f) * overlap;

			/* smooth final gain */
			ONE_POLE(g->gain_normalization, target_gain, 0.01f);

			/* apply normalization */
			for (c = 0; c < channels; c++) {
				output[i * channels + c] *= g->gain_normalization;
			}
		}

	}
}

static ma_node_vtable granular_vtable = {
	granular_process_pcm_frames,
	NULL,
	1,		/* 1 input bus (audio in) */
	1
};

/******************************************************************************
 * SLOT MANAGEMENT
 *****************************************************************************/

/*
   * allocate_granular_slot()
   * Finds a free granular delay slot and marks it as in use.
   */
  static int allocate_granular_slot(void)
  {
  	int i;

  	pthread_mutex_lock(&g_granular_mutex);
  	for (i = 0; i < MAX_GRANULAR_DELAYS; i++) {
  		if (!g_granular_delays[i].in_use) {
  			g_granular_delays[i].in_use = MA_TRUE;
  			pthread_mutex_unlock(&g_granular_mutex);
  			return i;
  		}
  	}

  	pthread_mutex_unlock(&g_granular_mutex);
  	return -1;
  }

  /*
   * free_granular_slot()
   * Uninitialize and free a granular delay slot.
   */
  static void free_granular_slot(int index)
  {
  	if (index >= 0 && index < MAX_GRANULAR_DELAYS) {
  		pthread_mutex_lock(&g_granular_mutex);

  		granular_delay_t *g = &g_granular_delays[index];
  		if (g->in_use) {
  			free_effect_chain(g->effect_chain);
  			g->effect_chain = NULL;
  			ma_node_detach_output_bus(&g->base, 0);
  			ring_buffer_free(&g->buffer);
  			ma_node_uninit(&g->base, NULL);
  			g->in_use = MA_FALSE;
  		}

  		pthread_mutex_unlock(&g_granular_mutex);
  	}
  }


/******************************************************************************
 * PROLOG INTERFACE
 *****************************************************************************/

/*
 * pl_granular_init()
 * Creates a granular delay with the specified buffer duration.
 * granular_init(+BufferSeconds, -Granular)
 * Returns granular(N).
 */
static foreign_t pl_granular_init(term_t buffer_term, term_t handle_term)
{
	double buffer_seconds;
	int slot;
	granular_delay_t *g;
	ma_node_config config;
	ma_uint32 channels;
	ma_uint32 sample_rate;
	ma_format format;
	ma_uint64 buffer_frames;
	ma_result result;

	ENSURE_ENGINE_INITIALIZED();

	if (!PL_get_float(buffer_term, &buffer_seconds)) {
  		return PL_type_error("float", buffer_term);
  	}

  	if (buffer_seconds <= 0.0) {
  		return PL_domain_error("positive_number", buffer_term);
  	}

	slot = allocate_granular_slot();
	if (slot < 0) {
		return PL_resource_error("granular_slots");
	}

	g = &g_granular_delays[slot];
	get_engine_format_info(&format, &channels, &sample_rate);
	buffer_frames = (ma_uint64)(buffer_seconds * sample_rate);

	result = ring_buffer_init(&g->buffer, buffer_frames, channels, format);
	if (result != MA_SUCCESS) {
		free_granular_slot(slot);
		return PL_resource_error("memory");
	}

	config = ma_node_config_init();
	config.vtable = &granular_vtable;
	config.pInputChannels = &channels;
	config.pOutputChannels = &channels;

	result = ma_node_init(ma_engine_get_node_graph(g_engine), &config, NULL, &g->base);
  	if (result != MA_SUCCESS) {
  		ring_buffer_free(&g->buffer);
  		free_granular_slot(slot);
  		return PL_resource_error("node_init");
  	}

	result = ma_node_attach_output_bus(
			&g->base,
			0,
			ma_node_graph_get_endpoint(ma_engine_get_node_graph(g_engine)),
			0);
	if (result != MA_SUCCESS) {
		ma_node_uninit(&g->base, NULL);
		ring_buffer_free(&g->buffer);
		free_granular_slot(slot);
		return PL_resource_error("node_attach_output_bus");
	}
	
	/* initialize to defaults */
	g->recording = MA_FALSE;
  	g->effect_chain = NULL;
  	g->density = 0.0f;           	/* no automatic triggering */
	g->regularity = 1.0;			/* fully regular by default */
  	g->position = 0.5f;          	/* middle of buffer */
  	g->position_spray = 0.0f;    	/* no randomization */
  	g->size_ms = 100.0f;         	/* 100ms grains */
  	g->size_spray = 0.0f;        	/* no randomization */
	g->reverse_probability = 0.0f;	/*always forward by default */
  	g->pitch = 0.0f;             	/* original pitch */
  	g->envelope = 0.5f;          	/* gaussian (smooth) */
  	g->pan = 0.0f;               	/* center */
  	g->pan_spray = 0.0f;         	/* no randomization */
  	g->mode_length = 0;          	/* no pitch quantization */
  	g->deviation_up = 0;
  	g->deviation_down = 0;
  	g->density_accumulator = 0.0f;
	g->num_grains_smoothed = 0.0f;
	g->gain_normalization = 1.0f;
	g->normalize = MA_TRUE;
	g->frames_recorded = 0;

	return unify_typed_handle(handle_term, "granular", slot);
}

/*
 * pl_granular_trigger()
 * Manually trigger a grain.
 * granular_trigger(+Granular)
 */
static foreign_t pl_granular_trigger(term_t handle_term)
{
	int slot;
	granular_delay_t *g;

	GET_GRANULAR_FROM_HANDLE(handle_term, g, slot);
	trigger_grain(g);

	return TRUE;
}

/*
 * pl_granular_set()
 * Set granular delay parameters from key=value list.
 * granular_set(+Granular, +Params)
 */
static foreign_t pl_granular_set(term_t handle_term, term_t params_term)
{
	int slot;
	granular_delay_t *g;
	float f;
	ma_bool32 b;

	GET_GRANULAR_FROM_HANDLE(handle_term, g, slot);

	if (get_param_float(params_term, "density", &f)) {
		g->density = f;
	}
	if (get_param_float(params_term, "position", &f)) {
		g->position = CLAMP(f, 0.0f, 1.0f);
	}
	if (get_param_float(params_term, "position_spray", &f)) {
		g->position_spray = f;
	}
	if (get_param_float(params_term, "size", &f)) {
		g->size_ms = f;
	}
	if (get_param_float(params_term, "size_spray", &f)) {
		g->size_spray = f;
	}
	if (get_param_float(params_term, "pitch", &f)) {
		g->pitch = f;
	}
	if (get_param_float(params_term, "envelope", &f)) {
		g->envelope = CLAMP(f, 0.0f, 1.0f);
	}
	if (get_param_float(params_term, "pan", &f)) {
		g->pan = CLAMP(f, -1.0f, 1.0f);
	}
	if (get_param_float(params_term, "pan_spray", &f)) {
		g->pan_spray = f;
	}
	if (get_param_float(params_term, "reverse_probability", &f)) {
		g->reverse_probability = CLAMP(f, 0.0f, 1.0f);
	}
	if (get_param_float(params_term, "regularity", &f)) {
		g->regularity = CLAMP(f, 0.0f, 1.0f);
	}
	if (get_param_bool(params_term, "recording", &b)) {
		g->recording = b;
	}
	if (get_param_bool(params_term, "normalize", &b)) {
		if (b && !g->normalize) {
			/* turning on - reset smoothing state */
			g->num_grains_smoothed = 0.0f;
			g->gain_normalization = 1.0f;
		}
		g->normalize = b;
	}

	return TRUE;
}

/*
 * add_float_param()
 * Helper to add key=value pair to list for float parameters.
 */
static int add_float_param(term_t *list, functor_t eq, const char *name, float value)
{
	term_t pair = PL_new_term_ref();
	term_t args = PL_new_term_refs(2);

	if (!PL_put_atom_chars(args, name)) return 0;
	if (!PL_put_float(args + 1, value)) return 0;
	if (!PL_cons_functor_v(pair, eq, args)) return 0;
	if (!PL_cons_list(*list, pair, *list)) return 0;
	return 1;
}

/*
 * add_bool_param()
 * Helper to add key=value pair to list for boolean parameters.
 */
static int add_bool_param(term_t *list, functor_t eq, const char *name, ma_bool32 value)
{
	term_t pair = PL_new_term_ref();
	term_t args = PL_new_term_refs(2);

	if (!PL_put_atom_chars(args, name)) return 0;
	if (!PL_put_atom_chars(args + 1, value ? "true" : "false")) return 0;
	if (!PL_cons_functor_v(pair, eq, args)) return 0;
	if (!PL_cons_list(*list, pair, *list)) return 0;
	return 1;
}

/*
 * pl_granular_get()
 * Get all granular delay parameters as key=value list.
 * granular_get(+Granular, -Params)
 */
static foreign_t pl_granular_get(term_t handle_term, term_t params_term)
{
	int slot;
	granular_delay_t *g;
	term_t list;
	functor_t eq;

	GET_GRANULAR_FROM_HANDLE(handle_term, g, slot);

	list = PL_new_term_ref();
	PL_put_nil(list);
	eq = PL_new_functor(PL_new_atom("="), 2);

	/* add params in reverse order so list comes out in natural order */
	if (!add_bool_param(&list, eq, "normalize", g->normalize)) return FALSE;
	if (!add_bool_param(&list, eq, "recording", g->recording)) return FALSE;
	if (!add_float_param(&list, eq, "regularity", g->regularity)) return FALSE;
	if (!add_float_param(&list, eq, "reverse_probability", g->reverse_probability)) return FALSE;
	if (!add_float_param(&list, eq, "pan_spray", g->pan_spray)) return FALSE;
	if (!add_float_param(&list, eq, "pan", g->pan)) return FALSE;
	if (!add_float_param(&list, eq, "envelope", g->envelope)) return FALSE;
	if (!add_float_param(&list, eq, "pitch", g->pitch)) return FALSE;
	if (!add_float_param(&list, eq, "size_spray", g->size_spray)) return FALSE;
	if (!add_float_param(&list, eq, "size", g->size_ms)) return FALSE;
	if (!add_float_param(&list, eq, "position_spray", g->position_spray)) return FALSE;
	if (!add_float_param(&list, eq, "position", g->position)) return FALSE;
	if (!add_float_param(&list, eq, "density", g->density)) return FALSE;

	return PL_unify(params_term, list);
}

/*
 * pl_granular_uninit()
 * Unload a granular delay and free resources.
 * granular_uninit(+Granular)
 */
static foreign_t pl_granular_uninit(term_t handle_term)
{
	int slot;
	granular_delay_t *g;

	GET_GRANULAR_FROM_HANDLE(handle_term, g, slot);
	(void)g;

	free_granular_slot(slot);

	return TRUE;
}

/*
 * pl_granular_set_mode()
 * Set pitch quantization mode for grain triggering.
 * granular_set_mode(+Granular, +ModeList, +DeviationDown, +DeviationUp)
 * ModeList is a list of semitone intervals, e.g. [0, 2, 4, 5, 7, 9, 11] for major.
 * Empty list disables mode quantization.
 */
static foreign_t pl_granular_set_mode(term_t handle_term, term_t mode_term,
                                      term_t down_term, term_t up_term)
{
	int slot;
	int deviation_down, deviation_up;
	granular_delay_t *g;
	term_t head;
	term_t tail;
	int count;
	double interval;

	head = PL_new_term_ref();
	tail = PL_new_term_ref();
	count = 0;

	GET_GRANULAR_FROM_HANDLE(handle_term, g, slot);

	if (!PL_get_integer(down_term, &deviation_down)) {
		return PL_type_error("integer", down_term);
	}

	if (!PL_get_integer(up_term, &deviation_up)) {
		return PL_type_error("integer", up_term);
	}

	/* parse mode list */
	if (!PL_put_term(tail, mode_term)) return FALSE;

	while (PL_get_list(tail, head, tail) && count < MAX_MODE_INTERVAL) {
		if (!PL_get_float(head, &interval)) {
			return PL_type_error("float", head);
		}
		g->mode[count] = (float)interval;
		count++;
	}

	g->mode_length = count;
	g->deviation_down = deviation_down;
	g->deviation_up = deviation_up;

	return TRUE;
}

/*
 * pl_granular_connect()
 * Connect a source to the granular delay input.
 * granular_connect(+Granular, +Source)
 * Source is sound(N), voice(N), image_synth(N), or capture(N).
 */
static foreign_t pl_granular_connect(term_t handle_term, term_t source_term)
{
	int slot;
	granular_delay_t *g;
	ma_node *source_node;
	ma_node *output_node;
	effect_node_t *chain;
	ma_result result;

	GET_GRANULAR_FROM_HANDLE(handle_term, g, slot);

	if (!get_source_from_term(source_term, &source_node, &chain)) {
		return PL_existence_error("source", source_term);
	}

	/* find output node: end of effect chain, or source itself */
	output_node = get_effect_chain_tail(chain);
	if (output_node == NULL) {
		output_node = source_node;
	}

	/* attach source to granular input */
	result = ma_node_attach_output_bus(output_node, 0, (ma_node *)&g->base, 0);

	return result == MA_SUCCESS ? TRUE : FALSE;
}

install_t granular_register_predicates(void)
{
	PL_register_foreign("granular_init", 2, pl_granular_init, 0);
	PL_register_foreign("granular_uninit", 1, pl_granular_uninit, 0);
	PL_register_foreign("granular_trigger", 1, pl_granular_trigger, 0);
	PL_register_foreign("granular_set", 2, pl_granular_set, 0);
	PL_register_foreign("granular_get", 2, pl_granular_get, 0);
	PL_register_foreign("granular_set_mode", 4, pl_granular_set_mode, 0);
	PL_register_foreign("granular_connect", 2, pl_granular_connect, 0);
}

install_t uninstall_granular(void)
{
	int i;

	for (i = 0; i < MAX_GRANULAR_DELAYS; i++) {
		if (g_granular_delays[i].in_use) {
			free_granular_slot(i);
		}
	}
}

