/*
 * mod.c - Modulation system
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#include "promini.h"

/******************************************************************************
 * GLOBAL VARIABLES
 *****************************************************************************/

mod_source_t g_mod_sources[MAX_MOD_SOURCES] = {{0}};
mod_route_t g_mod_routes[MAX_MOD_ROUTES] = {{0}};
pthread_mutex_t g_mod_mutex = PTHREAD_MUTEX_INITIALIZER;


/******************************************************************************
 * SLOT MANAGEMENT
 *****************************************************************************/

static int allocate_source_slot(void)
{
	int i;
	for (i = 0; i < MAX_MOD_SOURCES; i++) {
		if (!g_mod_sources[i].in_use) {
			g_mod_sources[i].in_use = MA_TRUE;
			return i;
		}
	}
	return -1;
}

static void free_source_slot(int index)
{
	if (index >= 0 && index < MAX_MOD_SOURCES) {
		g_mod_sources[index].in_use = MA_FALSE;
		g_mod_sources[index].type = MOD_SOURCE_NONE;
	}
}

static int allocate_route_slot(void)
{
	int i;
	for (i = 0; i < MAX_MOD_ROUTES; i++) {
		if (!g_mod_routes[i].in_use) {
			g_mod_routes[i].in_use = MA_TRUE;
			return i;
		}
	}
	return -1;
}

static void free_route_slot(int index)
{
	if (index >= 0 && index < MAX_MOD_ROUTES) {
		g_mod_routes[index].in_use = MA_FALSE;
	}
}


/* 
 * unistall_mod()
 * Cleanus up all modulation resources
 */
install_t uninstall_mod(void)
{
	int i;

	pthread_mutex_lock(&g_mod_mutex);

	for (i = 0; i < MAX_MOD_SOURCES; i++) {
		if (g_mod_sources[i].in_use) {
			if (g_mod_sources[i].type == MOD_SOURCE_WAVEFORM) {
				ma_waveform_uninit(&g_mod_sources[i].source.waveform);
			} else if (g_mod_sources[i].type == MOD_SOURCE_NOISE) {
				ma_noise_uninit(&g_mod_sources[i].source.noise, NULL);
			}
			g_mod_sources[i].in_use = MA_FALSE;
		}
	}

	for (i = 0; i < MAX_MOD_ROUTES; i++) {
		g_mod_routes[i].in_use = MA_FALSE;
	}

	pthread_mutex_unlock(&g_mod_mutex);
}

/******************************************************************************
 * MODULATION READING
 *****************************************************************************/

/*
 * read_source_value()
 * Reads the current value from a modulation source.
 * Advances the source state by frame_count frames.
 * Returns a value in [-1, 1] for LFO/noise, [0, 1] for envelope.
 */
static float read_source_value(mod_source_t* src, ma_uint32 frame_count, ma_uint32 sample_rate)
{
	float value = 0.0f;
	float waveform_buf[512];
	ma_uint64 to_read;
	data_slot_t* data;
	float* samples;
	ma_uint64 buf_frames;
	ma_uint32 channels, pos, i;
	float stage_frames, increment;

	switch(src->type) {
		case MOD_SOURCE_WAVEFORM:
			to_read = frame_count < 512 ? frame_count : 512;
			ma_waveform_read_pcm_frames(&src->source.waveform, waveform_buf, to_read, NULL);
			value = 0.0f;
			for (i = 0; i < to_read; i++) {
				value += waveform_buf[i];
			}
			value /= to_read;
			break;

		case MOD_SOURCE_NOISE:
			ma_noise_read_pcm_frames(&src->source.noise, &value, 1, NULL);
			break;

		case MOD_SOURCE_SAMPLER:
			data = get_data_slot(src->source.sampler.data_slot);
			if (data == NULL) break;

			samples = (float*)data->pData;
			buf_frames = data->buffer->ref.sizeInFrames;
			channels = data->buffer->ref.channels;
			pos = (ma_uint32)src->source.sampler.cursor % buf_frames;

			/* mono average across channels */
			value = 0.0f;
			for (i = 0; i < channels; i++) {
				value += samples[pos * channels + i];
			}
			value /= channels;

			/* advance cursor, wrap at buffer end for looping */
			src->source.sampler.cursor += frame_count * src->source.sampler.rate;
			while (src->source.sampler.cursor >= buf_frames) {
				src->source.sampler.cursor -= buf_frames;
			}
			break;

		case MOD_SOURCE_ENVELOPE:
			/* calculate envelope value based on stage */
			switch (src->source.envelope.stage) {
				case 0: /* attack: 0 -> 1 */
					value = src->source.envelope.stage_progress;
					break;
				case 1: /* decay: 1 -> break_level */
					value = 1.0f - src->source.envelope.stage_progress *
					        (1.0f - src->source.envelope.break_level);
					break;
				case 2: /* break: hold at break_level */
					value = src->source.envelope.break_level;
					break;
				case 3: /* release: break_level -> 0 */
					value = src->source.envelope.break_level *
					        (1.0f - src->source.envelope.stage_progress);
					break;
				default: /* done */
					value = 0.0f;
					break;
			}

			/* advance stage progress */
			if (src->source.envelope.stage < 4) {
				float proportion;
				switch (src->source.envelope.stage) {
					case 0: proportion = src->source.envelope.attack; break;
					case 1: proportion = src->source.envelope.decay; break;
					case 2: proportion = src->source.envelope.brk; break;
					case 3: proportion = src->source.envelope.release; break;
					default: proportion = 0.0f; break;
				}
				stage_frames = (src->source.envelope.duration_ms / 1000.0f) *
				               proportion * sample_rate;
				increment = stage_frames > 0.0f ? (float)frame_count / stage_frames : 1.0f;
				src->source.envelope.stage_progress += increment;

				/* check for stage transition */
				if (src->source.envelope.stage_progress >= 1.0f) {
					src->source.envelope.stage++;
					src->source.envelope.stage_progress = 0.0f;
					if (src->source.envelope.stage > 3 && src->source.envelope.loop) {
						src->source.envelope.stage = 0;
					}
				}
			}
			break;

		default:
			break;
	}

	return value;
}

/*
 * process_modulation()
 * Called once per audio block from the audio callback
 * Updates all modulation sources and applies routes to their targets
 */
void process_modulation(ma_uint32 frame_count, ma_uint32 sample_rate)
{
	int i;
	mod_source_t* src;
	mod_route_t* route;
	float raw_value, target_value, max_change, diff;


	pthread_mutex_lock(&g_mod_mutex);

	/* update all active sources */
	for (i = 0; i < MAX_MOD_SOURCES; i++) {
		src = &g_mod_sources[i];
		if (!src->in_use) continue;

		raw_value = read_source_value(src, frame_count, sample_rate);

		/* apply sample & hold if enabled 
		 * only update output when counter exceeds interval
		 * creates stepped/quantized modulation 
		 */
		if (src->sh_enabled && src->sh_interval > 0) {
			src->sh_counter += frame_count;
			if (src->sh_counter >= src->sh_interval) {
				src->sh_held_value = raw_value;
				src->sh_counter = 0;
			}
			src->current_value = src->sh_held_value;
		} else {
			src->current_value = raw_value;
		}
	}

	/* process all active routes */
	for (i = 0; i < MAX_MOD_ROUTES; i++) {
		route = &g_mod_routes[i];
		if (!route->in_use) continue;

		target_value = route->offset + (g_mod_sources[route->source_slot].current_value * route->depth);

		/* apply slew limiting: cap rate of cghange to slew units/second.
		 * e.g. if slew = 1000 on filter cutoff, max of 1000 Hz/sec change.
		 * smooths sudden jumps in modulation
		 */
		if (route-> slew > 0) {
			max_change = route->slew * ((float)frame_count / sample_rate);
			diff = target_value - route->current_value;
			if (diff > max_change) diff = max_change;
			if (diff < -max_change) diff = -max_change;
			route->current_value += diff;
		} else {
			route->current_value = target_value;
		}

		route->setter(route->target, route->current_value);
	}

	pthread_mutex_unlock(&g_mod_mutex);
}
/******************************************************************************
 * SETTERS
 *****************************************************************************/

/*
 * set_oscillator_frequency()
 * Setter for oscillator frequency. Target is synth_oscillator_t*.
 */
static void set_oscillator_frequency(void* target, float value)
{
	synth_oscillator_t* osc = (synth_oscillator_t*)target;
	ma_waveform_set_frequency(&osc->source.waveform, value);
}


/******************************************************************************
 * SOURCE AND ROUTE MANAGEMENT
 *****************************************************************************/

/*
 * pl_mod_source_unload()
 * Unloads a modulation source and removes any routes using it.
 * mod_source_unload(+Handle)
 */
static foreign_t pl_mod_source_unload(term_t handle_term)
{
	int slot, i;
	mod_source_t* src;

	if (!PL_get_integer(handle_term, &slot)) return FALSE;
	if (slot < 0 || slot >= MAX_MOD_SOURCES) {
		return PL_existence_error("mod_source", handle_term);
	}

	pthread_mutex_lock(&g_mod_mutex);
	src = &g_mod_sources[slot];
	if (!src->in_use) {
		pthread_mutex_unlock(&g_mod_mutex);
		return PL_existence_error("mod_source", handle_term);
	}

	/* remove any routes using this source */
	for (i = 0; i < MAX_MOD_ROUTES; i++) {
		if (g_mod_routes[i].in_use && g_mod_routes[i].source_slot == slot) {
			free_route_slot(i);
		}
	}

	/* uninit the source */
	if (src->type == MOD_SOURCE_WAVEFORM) {
		ma_waveform_uninit(&src->source.waveform);
	} else if (src->type == MOD_SOURCE_NOISE) {
		ma_noise_uninit(&src->source.noise, NULL);
	}

	free_source_slot(slot);
	pthread_mutex_unlock(&g_mod_mutex);
	return TRUE;
}

/* 
 * pl_mod_route_create()
 * Creates a modulation route from source to target parameter.
 * mod_route_create(+SourceHandle, +TargetType, +TargetHandle, +Param, +Depth, +Offset, +Slew, -RouteHandle)
 */
static foreign_t pl_mod_route_create(
		term_t source_term,
		term_t type_term,
		term_t target_term,
		term_t param_term,
		term_t depth_term,
		term_t offset_term,
		term_t slew_term,
		term_t handle_term
		)
{
	int source_slot, target_handle, slot;
	char* target_type;
	char* param;
	double depth, offset, slew;
	mod_route_t* route;
	void* target;
	mod_setter_t setter;

	if (!PL_get_integer(source_term, &source_slot)) return FALSE;
	if (!PL_get_atom_chars(type_term, &target_type)) return FALSE;
	if (!PL_get_integer(target_term, &target_handle)) return FALSE;
	if (!PL_get_atom_chars(param_term, &param)) return FALSE;
	if (!PL_get_float(depth_term, &depth)) return FALSE;
	if (!PL_get_float(offset_term, &offset)) return FALSE;
	if (!PL_get_float(slew_term, &slew)) return FALSE;

	if (source_slot < 0 || source_slot >= MAX_MOD_SOURCES) {
		return PL_existence_error("mod_source", source_term);
	}

	/* resolve target and setter */
	target = NULL;
	setter = NULL;

	if (strcmp(target_type, "oscillator") == 0) {
		if (target_handle < 0 || target_handle >= MAX_OSCILLATORS || !g_oscillators[target_handle].in_use) {
			return PL_existence_error("oscillator", target_term);
		}
		if (strcmp(param, "frequency") == 0) {
			target = &g_oscillators[target_handle];
			setter = set_oscillator_frequency;
		} else {
			return PL_domain_error("oscillator_param", param_term);
		}
	} else {
		return PL_domain_error("target_type", type_term);
	}

	pthread_mutex_lock(&g_mod_mutex);

	if (!g_mod_sources[source_slot].in_use) {
		pthread_mutex_unlock(&g_mod_mutex);
		return PL_existence_error("mod_source", source_term);
	}

	slot = allocate_route_slot();
	if (slot < 0) {
		pthread_mutex_unlock(&g_mod_mutex);
		return PL_resource_error("mod_route_slots");
	}

	route = &g_mod_routes[slot];
	route->source_slot = source_slot;
	route->target = target;
	route->setter = setter;
	route->depth = (float)depth;
	route->offset = (float)offset;
	route->slew = (float)slew;
	route->current_value = (float)offset;

	pthread_mutex_unlock(&g_mod_mutex);
	return PL_unify_integer(handle_term, slot);
}

/*
 * pl_mod_route_unload()
 * Removes a modulation route.
 * mod_route_unload(+Handle)
 */
static foreign_t pl_mod_route_unload(term_t handle_term)
{
	int slot;

	if (!PL_get_integer(handle_term, &slot)) return FALSE;
	if (slot < 0 || slot >= MAX_MOD_ROUTES) {
		return PL_existence_error("mod_route", handle_term);
	}

	pthread_mutex_lock(&g_mod_mutex);
	if (!g_mod_routes[slot].in_use) {
		pthread_mutex_unlock(&g_mod_mutex);
		return PL_existence_error("mod_route", handle_term);
	}

	free_route_slot(slot);
	pthread_mutex_unlock(&g_mod_mutex);
	return TRUE;
}

/******************************************************************************
 * LFO  
 *****************************************************************************/

/*
 * pl_mod_lfo_create()
 * Createas an LFO modulation source.
 * mod_lfo_create(+Type, +Freq, -Handle)
 * Type is one of: sine, square, triangle, sawtooth
 */
static foreign_t pl_mod_lfo_create(term_t type_term, term_t freq_term, term_t handle_term)
{
	char* type_str;
	double freq;
	int slot;
	ma_waveform_type waveform_type;
	ma_waveform_config config;
	ma_result result;
	mod_source_t* src;

	ENSURE_ENGINE_INITIALIZED();

	if (!PL_get_atom_chars(type_term, &type_str)) return FALSE;
	if (!PL_get_float(freq_term, &freq)) return FALSE;

	if (strcmp(type_str, "sine") == 0) {
		waveform_type = ma_waveform_type_sine;
	} else if (strcmp(type_str, "square") == 0) {
		waveform_type = ma_waveform_type_square;
	} else if (strcmp(type_str, "triangle") == 0) {
		waveform_type = ma_waveform_type_triangle;
	} else if (strcmp(type_str, "sawtooth") == 0) {
		waveform_type = ma_waveform_type_sawtooth;
	} else {
		return FALSE;
	}

	pthread_mutex_lock(&g_mod_mutex);
	slot = allocate_source_slot();
	if (slot < 0) {
		pthread_mutex_unlock(&g_mod_mutex);
		return PL_resource_error("mod_source_slots");
	}

	src = &g_mod_sources[slot];
	src->type = MOD_SOURCE_WAVEFORM;

	config = ma_waveform_config_init(
			ma_format_f32,
			1,
			ma_engine_get_sample_rate(g_engine),
			waveform_type,
			1.0,
			freq
			);

	result = ma_waveform_init(&config, &src->source.waveform);
	if (result != MA_SUCCESS) {
		free_source_slot(slot);
		pthread_mutex_unlock(&g_mod_mutex);
		return FALSE;
	}

	pthread_mutex_unlock(&g_mod_mutex);
	return PL_unify_integer(handle_term, slot);
}

/*
 * set_lfo_frequency()
 * Setter for LFO frequency. Target is mod_source_t*.
 */
static void set_lfo_frequency(void* target, float value)
{
	mod_source_t* src = (mod_source_t*)target;
	ma_waveform_set_frequency(&src->source.waveform, value);
}

/*
 * pl_mod_lfo_set_frequency()
 * mod_lfo_set_frequency(+Handle, +Freq)
 */
static foreign_t pl_mod_lfo_set_frequency(term_t handle_term, term_t freq_term)
{
	int slot;
	double freq;
	mod_source_t* src;

	if (!PL_get_integer(handle_term, &slot)) return FALSE;
	if (!PL_get_float(freq_term, &freq)) return FALSE;

	if (slot < 0 || slot >= MAX_MOD_SOURCES) return FALSE;

	pthread_mutex_lock(&g_mod_mutex);
	src = &g_mod_sources[slot];
	if (!src->in_use || src->type != MOD_SOURCE_WAVEFORM) {
		pthread_mutex_unlock(&g_mod_mutex);
		return FALSE;
	}

	ma_waveform_set_frequency(&src->source.waveform, freq);
	pthread_mutex_unlock(&g_mod_mutex);
	return TRUE;
}

/*
 * pl_mod_lfo_get_frequency()
 * mod_lfo_get_frequency(+Handle, -Freq)
 */
static foreign_t pl_mod_lfo_get_frequency(term_t handle_term, term_t freq_term)
{
	int slot;
	mod_source_t* src;
	float freq;

	if (!PL_get_integer(handle_term, &slot)) return FALSE;
	if (slot < 0 || slot >= MAX_MOD_SOURCES) return FALSE;

	pthread_mutex_lock(&g_mod_mutex);
	src = &g_mod_sources[slot];
	if (!src->in_use || src->type != MOD_SOURCE_WAVEFORM) {
		pthread_mutex_unlock(&g_mod_mutex);
		return FALSE;
	}

	freq = src->source.waveform.config.frequency;
	pthread_mutex_unlock(&g_mod_mutex);
	return PL_unify_float(freq_term, freq);
}

/******************************************************************************
 * ENVELOPE
 *****************************************************************************/

/*
 * pl_mod_envelope_create()
 * Creates an ADBR envelope modulation source.
 * mod_envelope_create(+Attack, +Decay, +Break, +BreakLevel, +Release, +DurationMs, +Loop, -Handle)
 * Attack, Decay, Break, Release are proportions (should sum to 1.0)
 * BreakLevel is the level at the break point (0.0-1.0)
 * DurationMs is the total envelope time in milliseconds
 * Loop is true/false
 */
static foreign_t pl_mod_envelope_create(
		term_t attack_term,
		term_t decay_term,
		term_t break_term,
		term_t break_level_term,
		term_t release_term,
		term_t duration_term,
		term_t loop_term,
		term_t handle_term)
{
	double attack, decay, brk, break_level, release, duration;
	int loop_int;
	int slot;
	mod_source_t* src;

	if (!PL_get_float(attack_term, &attack)) return FALSE;
	if (!PL_get_float(decay_term, &decay)) return FALSE;
	if (!PL_get_float(break_term, &brk)) return FALSE;
	if (!PL_get_float(break_level_term, &break_level)) return FALSE;
	if (!PL_get_float(release_term, &release)) return FALSE;
	if (!PL_get_float(duration_term, &duration)) return FALSE;
	if (!PL_get_bool(loop_term, &loop_int)) return FALSE;

	pthread_mutex_lock(&g_mod_mutex);
	slot = allocate_source_slot();
	if (slot < 0) {
		pthread_mutex_unlock(&g_mod_mutex);
		return PL_resource_error("mod_source_slots");
	}

	src = &g_mod_sources[slot];
	src->type = MOD_SOURCE_ENVELOPE;
	src->source.envelope.attack = (float)attack;
	src->source.envelope.decay = (float)decay;
	src->source.envelope.brk = (float)brk;
	src->source.envelope.break_level = (float)break_level;
	src->source.envelope.release = (float)release;
	src->source.envelope.duration_ms = (float)duration;
	src->source.envelope.loop = loop_int ? MA_TRUE : MA_FALSE;
	src->source.envelope.stage = 4; /* start in "done" state until triggered */
	src->source.envelope.stage_progress = 0.0f;

	pthread_mutex_unlock(&g_mod_mutex);
	return PL_unify_integer(handle_term, slot);
}

/*
 * pl_mod_envelope_trigger()
 * Triggers (restarts) an envelope from the beginning.
 * mod_envelope_trigger(+Handle)
 */
static foreign_t pl_mod_envelope_trigger(term_t handle_term)
{
	int slot;
	mod_source_t* src;

	if (!PL_get_integer(handle_term, &slot)) return FALSE;
	if (slot < 0 || slot >= MAX_MOD_SOURCES) return FALSE;

	pthread_mutex_lock(&g_mod_mutex);
	src = &g_mod_sources[slot];
	if (!src->in_use || src->type != MOD_SOURCE_ENVELOPE) {
		pthread_mutex_unlock(&g_mod_mutex);
		return FALSE;
	}

	src->source.envelope.stage = 0;
	src->source.envelope.stage_progress = 0.0f;

	pthread_mutex_unlock(&g_mod_mutex);
	return TRUE;
}

/******************************************************************************
 * REGISTRATION
 *****************************************************************************/

install_t mod_register_predicates(void)
{
	PL_register_foreign("mod_lfo_create", 3, pl_mod_lfo_create, 0);
	PL_register_foreign("mod_lfo_set_frequency", 2, pl_mod_lfo_set_frequency, 0);
	PL_register_foreign("mod_lfo_get_frequency", 2, pl_mod_lfo_get_frequency, 0);
	PL_register_foreign("mod_envelope_create", 8, pl_mod_envelope_create, 0);
	PL_register_foreign("mod_envelope_trigger", 1, pl_mod_envelope_trigger, 0);
	PL_register_foreign("mod_source_unload", 1, pl_mod_source_unload, 0);
	PL_register_foreign("mod_route_create", 8, pl_mod_route_create, 0);
	PL_register_foreign("mod_route_unload", 1, pl_mod_route_unload, 0);
}
