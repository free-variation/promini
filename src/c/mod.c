/*
 * mod.c - Modulation system
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#include "promini.h"

#define APPLY_MOD_VALUE(current, value, route) \
	((route)->rate_mode ? (current) + (value) : (value))

#define GET_MOD_SOURCE(handle_term, src_var, slot_var) \
	do { \
		if (!get_typed_handle(handle_term, "mod_source", &slot_var)) { \
			return PL_type_error("mod_source", handle_term); \
		} \
		if (slot_var < 0 || slot_var >= MAX_MOD_SOURCES) { \
			return PL_existence_error("mod_source", handle_term); \
		} \
		src_var = &g_mod_sources[slot_var]; \
	} while (0)

/******************************************************************************
 * GLOBAL VARIABLES
 *****************************************************************************/

mod_source_t g_mod_sources[MAX_MOD_SOURCES] = {{0}};
mod_route_t g_mod_routes[MAX_MOD_ROUTES] = {{0}};
pthread_mutex_t g_mod_mutex = PTHREAD_MUTEX_INITIALIZER;

static struct {
	const char *name;
	SDL_Scancode scancode;
} key_map[] = {
	{"space", SDL_SCANCODE_SPACE},
	{"left_control", SDL_SCANCODE_LCTRL},
	{"right_control", SDL_SCANCODE_RCTRL},
	{"left_shift", SDL_SCANCODE_LSHIFT},
	{"right_shift", SDL_SCANCODE_RSHIFT},
	{"up", SDL_SCANCODE_UP},
	{"down", SDL_SCANCODE_DOWN},
	{"left", SDL_SCANCODE_LEFT},
	{"right", SDL_SCANCODE_RIGHT},
	{"tab", SDL_SCANCODE_TAB},
	{"return", SDL_SCANCODE_RETURN},
	{"backspace", SDL_SCANCODE_BACKSPACE},
	{NULL, 0}
};


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
 * HELPER FUNCTIONS
 *****************************************************************************/

/*
 * get_scancode_from_atom()
 * Convert key atom to SDL_Scancode
 * Returns 1 on soccess, 0 on failure.
 */
static int get_scancode_from_atom(atom_t atom, SDL_Scancode *scancode)
{
	const char *name;
	int i;

	name = PL_atom_chars(atom);
	for (i = 0; key_map[i].name != NULL; i++) {
		if (strcmp(name, key_map[i].name) == 0) {
			*scancode = key_map[i].scancode;
			return 1;
		}
	}
	return 0;
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
static float read_source_value(mod_source_t *src, ma_uint32 frame_count, ma_uint32 sample_rate)
{
	float value = 0.0f;
	float waveform_buf[512];
	ma_uint64 to_read;
	data_slot_t *data;
	float *samples;
	ma_uint64 buf_frames;
	ma_uint32 channels, pos, i;
	float stage_frames, increment;
	const bool *keys;
	ma_bool32 pressed, rising_edge;
	float target, rate;

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
				case 3: /* release: release_from -> 0 */
					value = src->source.envelope.release_from *
					        (1.0f - src->source.envelope.stage_progress);
					break;
				default: /* done */
					value = 0.0f;
					break;
			}

			/* advance stage progress (break stage holds when gate is TRUE) */
			if (src->source.envelope.stage < 4 &&
					!(src->source.envelope.stage == 2 && src->source.envelope.gate)) {
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
					if (src->source.envelope.stage == 3) {
						src->source.envelope.release_from = src->source.envelope.break_level;
					}
					if (src->source.envelope.stage > 3 && src->source.envelope.loop) {
						src->source.envelope.stage = 0;
					}
				}
			}
			break;
		
		case MOD_SOURCE_GAMEPAD:
			if (src->source.gamepad.dpad_axis == 1) {
				int left = SDL_GetGamepadButton(src->source.gamepad.gamepad, SDL_GAMEPAD_BUTTON_DPAD_LEFT);
				int right = SDL_GetGamepadButton(src->source.gamepad.gamepad, SDL_GAMEPAD_BUTTON_DPAD_RIGHT);
				value = (float)(right - left);
			} else if (src->source.gamepad.dpad_axis == 2) {
				int down = SDL_GetGamepadButton(src->source.gamepad.gamepad, SDL_GAMEPAD_BUTTON_DPAD_DOWN);
				int up = SDL_GetGamepadButton(src->source.gamepad.gamepad, SDL_GAMEPAD_BUTTON_DPAD_UP);
				value = (float)(down - up);
			} else {
				value = SDL_GetGamepadAxis(src->source.gamepad.gamepad, src->source.gamepad.axis) / 32767.0f;
				if (fabs(value) < src->source.gamepad.dead_zone) {
					value = 0.0f;
				}
			}
			break;

		case MOD_SOURCE_GAMEPAD_BUTTON:
			pressed = SDL_GetGamepadButton(src->source.gamepad_button.gamepad,
			                               src->source.gamepad_button.button);
			rising_edge = pressed && !src->source.gamepad_button.prev_pressed;

			switch (src->source.gamepad_button.mode) {
			case BUTTON_MODE_CYCLING:
				if (rising_edge) {
					src->source.gamepad_button.current_index =
						(src->source.gamepad_button.current_index + 1) %
						src->source.gamepad_button.preset_count;
				}
				value = src->source.gamepad_button.presets[src->source.gamepad_button.current_index];
				break;
			case BUTTON_MODE_MOMENTARY:
				value = pressed ? 1.0f : 0.0f;
				break;
			case BUTTON_MODE_TRIGGER:
				value = rising_edge ? 1.0f : 0.0f;
				break;
			case BUTTON_MODE_TOGGLE:
				if (rising_edge) {
					src->source.gamepad_button.toggle_state = !src->source.gamepad_button.toggle_state;
				}
				value = src->source.gamepad_button.toggle_state ? 1.0f : 0.0f;
				break;
			}
			src->source.gamepad_button.prev_pressed = pressed;
			break;

		case MOD_SOURCE_KEYBOARD:
			keys = SDL_GetKeyboardState(NULL);
			pressed = keys[src->source.keyboard.scancode];
			target = pressed ? 1.0f : 0.0f;

			if (src->source.keyboard.invert) {
				target = 1.0f - target;
			}

			if (src->source.keyboard.value < target) {
				/* attack */
				rate = (src->source.keyboard.attack_ms > 0)
					? (1000.0f / src->source.keyboard.attack_ms) * frame_count / sample_rate
					: 1.0f;
				src->source.keyboard.value = CLAMP(src->source.keyboard.value + rate, 0.0f, target);
			} else if (src->source.keyboard.value > target) {
				/* release */
				rate = (src->source.keyboard.release_ms > 0)
					? (1000.0f / src->source.keyboard.release_ms) * frame_count / sample_rate
					: 1.0f;
				src->source.keyboard.value = CLAMP(src->source.keyboard.value - rate, 0.0f, target);
			}

			value = src->source.keyboard.value;
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
	mod_source_t *src;
	mod_route_t *route;
	float raw_value, target_value, max_change, diff;

	static ma_uint64 total_frames = 0;
	float dt, delta, final_value;
	ma_uint64 frames_since;
	char msg[LOG_MESSAGE_SIZE];

	pthread_mutex_lock(&g_mod_mutex);

	/* process clock pulse routes */
	if (g_clock.running) {
		update_clock_routes(CLOCK_ROUTE_PULSE);
	}

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

	total_frames += frame_count;

	/* process all active routes */
	for (i = 0; i < MAX_MOD_ROUTES; i++) {
		route = &g_mod_routes[i];
		if (!route->in_use) continue;

		if (route->rate_mode) {
			/* rate mode: pass delta to setter, which adds to current target value */
			dt = (float)frame_count / sample_rate;
			delta = g_mod_sources[route->source_slot].current_value * route->depth * dt;
			final_value = route->setter(route->target, delta, frame_count, route);
		} else {
			/* absolute mode: compute target value with slew limiting */
			target_value = route->offset + (g_mod_sources[route->source_slot].current_value * route->depth);

			/* apply slew limiting: cap rate of change to slew units/second.
			 * e.g. if slew = 1000 on filter cutoff, max of 1000 Hz/sec change.
			 * smooths sudden jumps in modulation
			 */
			if (route->slew > 0) {
				max_change = route->slew * ((float)frame_count / sample_rate);
				diff = target_value - route->current_value;
				if (diff > max_change) diff = max_change;
				if (diff < -max_change) diff = -max_change;
				route->current_value += diff;
			} else {
				route->current_value = target_value;
			}

			final_value = route->setter(route->target, route->current_value, frame_count, route);
		}

		if (route->monitor && final_value != route->last_printed_value) {
			frames_since = total_frames - route->last_print_time;
			if (frames_since > sample_rate / 10) {
				snprintf(msg, sizeof(msg), "%s: %.3f", route->param_name, final_value);
				control_set_log_message(msg);
				route->last_printed_value = final_value;
				route->last_print_time = total_frames;
			}
		}
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
static float set_oscillator_frequency(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	synth_oscillator_t *osc = (synth_oscillator_t *)target;
	float freq;
	(void)frame_count;

	freq = APPLY_MOD_VALUE(osc->source.waveform.config.frequency, value, route);
	freq = CLAMP(freq, 20.0f, 20000.0f);
	ma_waveform_set_frequency(&osc->source.waveform, freq);
	return freq;
}

/*
 * set_oscillator_volume()
 * Setter for oscillator volume. Target is synth_oscillator_t*.
 * Uses fade matching frame_count to avoid clicks.
 */
static float set_oscillator_volume(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	synth_oscillator_t *osc = (synth_oscillator_t *)target;
	float vol;

	vol = APPLY_MOD_VALUE(ma_sound_get_volume(&osc->sound), value, route);
	vol = CLAMP(vol, 0.0f, 1.0f);
	ma_sound_set_fade_in_pcm_frames(&osc->sound, -1, vol, frame_count);
	return vol;
}

/*
 * set_effect_pan()
 * Setter for pan effect. Target is pan_node_t*.
 * Sets target_pan which is interpolated at sample rate in the effect callback.
 */
static float set_effect_pan(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	pan_node_t *node = (pan_node_t *)target;
	float pan;
	(void)frame_count;

	pan = APPLY_MOD_VALUE(node->target_pan, value, route);
	pan = CLAMP(pan, -1.0f, 1.0f);
	node->target_pan = pan;
	return pan;
}

/*
 * set_moog_cutoff()
 * Setter for Moog cutoff.  Target is moog_node_t*
 * Uses per-sample interpolation so just sets target
 */
static float set_moog_cutoff(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	moog_node_t *moog = (moog_node_t *)target;
	float cutoff;
	(void)frame_count;

	cutoff = APPLY_MOD_VALUE(moog->target_cutoff, value, route);
	cutoff = CLAMP(cutoff, 20.0f, 20000.0f);
	moog->target_cutoff = cutoff;
	return cutoff;
}

/*
 * set_moog_resonance()
 * Setter for Moog resonance. Target is moog_node_t*
 * Uses per-sample interpolation so just sets target
 */
static float set_moog_resonance(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	moog_node_t *moog = (moog_node_t *)target;
	float res;
	(void)frame_count;

	res = APPLY_MOD_VALUE(moog->target_resonance, value, route);
	res = CLAMP(res, 0.0f, 4.0f);
	moog->target_resonance = res;
	return res;
}

/*
 * set_vca_gain()
 * Setter for VCA gain. Target is vca_node_t*
 * Uses per-sample interpolation so just sets target
 */
static float set_vca_gain(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	vca_node_t *vca = (vca_node_t *)target;
	float gain;
	(void)frame_count;

	gain = APPLY_MOD_VALUE(vca->target_gain, value, route);
	gain = CLAMP(gain, 0.0f, 1.0f);
	vca->target_gain = gain;
	return gain;
}

/*
 * set_ping_pong_delay()
 * Setter for ping-pong delay time. Target is ping_pong_delay_node_t*
 * Sets target_delay_in_frames for smooth transitions
 */
static float set_ping_pong_delay(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	ping_pong_delay_node_t *pp = (ping_pong_delay_node_t *)target;
	float delay;
	(void)frame_count;

	delay = APPLY_MOD_VALUE((float)pp->target_delay_in_frames, value, route);
	delay = CLAMP(delay, 1.0f, (float)pp->buffer_size);
	pp->target_delay_in_frames = (ma_uint32)delay;
	return delay;
}

/*
 * set_granular_density()
 * Setter for granular density. Target is granular_delay_t*.
 */
static float set_granular_density(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	granular_delay_t *g = (granular_delay_t *)target;
	float density;
	(void)frame_count;

	density = APPLY_MOD_VALUE(g->density, value, route);
	if (density < 0.0f) density = 0.0f;
	g->density = density;
	return density;
}

/*
 * set_granular_pitch()
 * Setter for granular pitch. Target is granular_delay_t*.
 */
static float set_granular_pitch(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	granular_delay_t *g = (granular_delay_t *)target;
	(void)frame_count;

	g->pitch = APPLY_MOD_VALUE(g->pitch, value, route);
	return g->pitch;
}

/*
 * set_granular_position()
 * Setter for granular position. Target is granular_delay_t*.
 */
static float set_granular_position(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	granular_delay_t *g = (granular_delay_t *)target;
	float pos;
	(void)frame_count;

	pos = APPLY_MOD_VALUE(g->position, value, route);
	g->position = CLAMP(pos, 0.0f, 1.0f);
	return g->position;
}

/*
 * set_granular_size()
 * Setter for granular size in ms. Target is granular_delay_t*.
 */
static float set_granular_size(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	granular_delay_t *g = (granular_delay_t *)target;
	float size;
	(void)frame_count;

	size = APPLY_MOD_VALUE(g->size_ms, value, route);
	if (size < 1.0f) size = 1.0f;
	g->size_ms = size;
	return size;
}

/*
 * set_granular_position_spray()
 * Setter for granular position spray. Target is granular_delay_t*.
 */
static float set_granular_position_spray(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	granular_delay_t *g = (granular_delay_t *)target;
	float spray;
	(void)frame_count;

	spray = APPLY_MOD_VALUE(g->position_spray, value, route);
	g->position_spray = CLAMP(spray, 0.0f, 1.0f);
	return g->position_spray;
}

/*
 * set_granular_size_spray()
 * Setter for granular size spray. Target is granular_delay_t*.
 */
static float set_granular_size_spray(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	granular_delay_t *g = (granular_delay_t *)target;
	float spray;
	(void)frame_count;

	spray = APPLY_MOD_VALUE(g->size_spray, value, route);
	g->size_spray = CLAMP(spray, 0.0f, 1.0f);
	return g->size_spray;
}

/*
 * set_granular_envelope()
 * Setter for granular envelope shape. Target is granular_delay_t*.
 */
static float set_granular_envelope(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	granular_delay_t *g = (granular_delay_t *)target;
	float env;
	(void)frame_count;

	env = APPLY_MOD_VALUE(g->envelope, value, route);
	g->envelope = CLAMP(env, 0.0f, 1.0f);
	return g->envelope;
}

/*
 * set_granular_regularity()
 * Setter for granular regularity. Target is granular_delay_t*.
 */
static float set_granular_regularity(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	granular_delay_t *g = (granular_delay_t *)target;
	float reg;
	(void)frame_count;

	reg = APPLY_MOD_VALUE(g->regularity, value, route);
	g->regularity = CLAMP(reg, 0.0f, 1.0f);
	return g->regularity;
}

/*
 * set_granular_reverse()
 * Setter for granular reverse probability. Target is granular_delay_t*.
 */
static float set_granular_reverse(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	granular_delay_t *g = (granular_delay_t *)target;
	float rev;
	(void)frame_count;

	rev = APPLY_MOD_VALUE(g->reverse_probability, value, route);
	g->reverse_probability = CLAMP(rev, 0.0f, 1.0f);
	return g->reverse_probability;
}

/*
 * set_granular_pan()
 * Setter for granular pan. Target is granular_delay_t*.
 */
static float set_granular_pan(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	granular_delay_t *g = (granular_delay_t *)target;
	float pan;
	(void)frame_count;

	pan = APPLY_MOD_VALUE(g->pan, value, route);
	g->pan = CLAMP(pan, -1.0f, 1.0f);
	return g->pan;
}

/*
 * set_granular_pan_spray()
 * Setter for granular pan spray. Target is granular_delay_t*.
 */
static float set_granular_pan_spray(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	granular_delay_t *g = (granular_delay_t *)target;
	float spray;
	(void)frame_count;

	spray = APPLY_MOD_VALUE(g->pan_spray, value, route);
	g->pan_spray = CLAMP(spray, 0.0f, 1.0f);
	return g->pan_spray;
}

/*
 * set_granular_recording()
 * Setter for granular recording state. Target is granular_delay_t*.
 */
static float set_granular_recording(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	granular_delay_t *g = (granular_delay_t *)target;
	float v;
	(void)frame_count;

	v = APPLY_MOD_VALUE(g->recording ? 1.0f : 0.0f, value, route);
	g->recording = (v >= 0.5f) ? MA_TRUE : MA_FALSE;
	return g->recording ? 1.0f : 0.0f;
}

/*
 * set_granular_trigger()
 * Setter for granular trigger. Target is granular_delay_t*.
 */
static float set_granular_trigger(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	granular_delay_t *g = (granular_delay_t *)target;
	float v;
	(void)frame_count;

	v = APPLY_MOD_VALUE(0.0f, value, route);
	if (v >= 0.5f) {
		trigger_grain(g);
	}
	return v >= 0.5f ? 1.0f : 0.0f;
}

/*
 * set_granular_reset()
 * Setter for granular reset. Target is granular_delay_t*.
 */
static float set_granular_reset(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	granular_delay_t *g = (granular_delay_t *)target;
	float v;
	(void)frame_count;

	v = APPLY_MOD_VALUE(0.0f, value, route);
	if (v >= 0.5f) {
		g->density = 5.0f;
		g->position = 0.5f;
		g->position_spray = 0.0f;
		g->size_ms = 150.0f;
		g->pitch = 0.0f;
		g->envelope = 0.5f;
		g->regularity = 1.0f;
		g->reverse_probability = 0.0f;
		g->recording = MA_TRUE;
	}
	return v >= 0.5f ? 1.0f : 0.0f;
}

/*
 * set_granular_deviation_up()
 * Setter for granular pitch deviation up range. Target is granular_delay_t*.
 */
static float set_granular_deviation_up(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	granular_delay_t *g = (granular_delay_t *)target;
	float v;
	(void)frame_count;

	v = APPLY_MOD_VALUE(g->deviation_up, value, route);
	g->deviation_up = CLAMP(v, 0.0f, 48.0f);
	return g->deviation_up;
}

/*
 * set_granular_deviation_down()
 * Setter for granular pitch deviation down range. Target is granular_delay_t*.
 */
static float set_granular_deviation_down(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	granular_delay_t *g = (granular_delay_t *)target;
	float v;
	(void)frame_count;

	v = APPLY_MOD_VALUE(g->deviation_down, value, route);
	g->deviation_down = CLAMP(v, -48.0f, 48.0f);
	return g->deviation_down;
}

/*
 * set_granular_wet()
 * Setter for granular wet level. Target is granular_delay_t*.
 */
static float set_granular_wet(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	granular_delay_t *g = (granular_delay_t *)target;
	float v;
	(void)frame_count;

	v = APPLY_MOD_VALUE(g->wet, value, route);
	g->wet = CLAMP(v, 0.0f, 1.0f);
	return g->wet;
}

/*
 * set_reverb_wet()
 * Setter for reverb wet level. Target is reverb_node_t*.
 */
static float set_reverb_wet(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	reverb_node_t *r = (reverb_node_t *)target;
	float wet;
	(void)frame_count;

	wet = APPLY_MOD_VALUE(r->wet, value, route);
	r->wet = CLAMP(wet, 0.0f, 1.0f);
	return r->wet;
}

/*
 * set_reverb_decay()
 * Setter for reverb decay. Target is reverb_node_t*.
 */
static float set_reverb_decay(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	reverb_node_t *r = (reverb_node_t *)target;
	float decay;
	(void)frame_count;

	decay = APPLY_MOD_VALUE(r->decay, value, route);
	r->decay = CLAMP(decay, 0.0f, 0.99f);
	return r->decay;
}

/*
 * set_reverb_damping()
 * Setter for reverb damping. Target is reverb_node_t*.
 */
static float set_reverb_damping(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	reverb_node_t *r = (reverb_node_t *)target;
	float damp;
	(void)frame_count;

	damp = APPLY_MOD_VALUE(r->damping, value, route);
	r->damping = CLAMP(damp, 0.0f, 1.0f);
	return r->damping;
}

/*
 * set_reverb_size()
 * Setter for reverb room size. Target is reverb_node_t*.
 */
static float set_reverb_size(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	reverb_node_t *r = (reverb_node_t *)target;
	float size;
	(void)frame_count;

	size = APPLY_MOD_VALUE(r->size, value, route);
	r->size = CLAMP(size, 0.0f, 2.0f);
	return r->size;
}

/*
 * set_reverb_freeze()
 * Setter for reverb freeze. Target is reverb_node_t*.
 */
static float set_reverb_freeze(void *target, float value, ma_uint32 frame_count, mod_route_t *route)
{
	reverb_node_t *r = (reverb_node_t *)target;
	float v;
	(void)frame_count;

	v = APPLY_MOD_VALUE(r->freeze ? 1.0f : 0.0f, value, route);
	r->freeze = (v >= 0.5f) ? MA_TRUE : MA_FALSE;
	return r->freeze ? 1.0f : 0.0f;
}

/******************************************************************************
 * SOURCE AND ROUTE MANAGEMENT
 *****************************************************************************/

/*
 * pl_mod_source_uninit()
 * Unloads a modulation source and removes any routes using it.
 * mod_source_uninit(+Source)
 */
static foreign_t pl_mod_source_uninit(term_t handle_term)
{
	int slot, i;
	mod_source_t *src;

	GET_MOD_SOURCE(handle_term, src, slot);

	pthread_mutex_lock(&g_mod_mutex);
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
 * pl_mod_route_init()
 * Creates a modulation route from source to target parameter.
 * mod_route_init(+Source, +TargetType, +Target, +Param, +Mode, +Depth, +Offset, +Slew, -Route)
 * Returns mod_route(N). Mode is 'absolute' or 'rate'.
 */
static foreign_t pl_mod_route_init(
		term_t source_term,
		term_t type_term,
		term_t target_term,
		term_t param_term,
		term_t mode_term,
		term_t depth_term,
		term_t offset_term,
		term_t slew_term,
		term_t handle_term
		)
{
	int source_slot, target_handle, slot;
	char *target_type;
	char *param;
	char *mode;
	double depth, offset, slew;
	ma_bool32 rate_mode;
	mod_route_t *route;
	void *target;
	mod_setter_t setter;

	if (!get_typed_handle(source_term, "mod_source", &source_slot)) {
		return PL_type_error("mod_source", source_term);
	}
	if (!PL_get_atom_chars(type_term, &target_type)) return FALSE;
	if (!PL_get_atom_chars(param_term, &param)) return FALSE;
	if (!PL_get_atom_chars(mode_term, &mode)) return FALSE;
	if (!PL_get_float(depth_term, &depth)) return FALSE;
	if (!PL_get_float(offset_term, &offset)) return FALSE;
	if (!PL_get_float(slew_term, &slew)) return FALSE;

	if (strcmp(mode, "absolute") == 0) {
		rate_mode = MA_FALSE;
	} else if (strcmp(mode, "rate") == 0) {
		rate_mode = MA_TRUE;
	} else {
		return PL_domain_error("mode", mode_term);
	}

	if (source_slot < 0 || source_slot >= MAX_MOD_SOURCES) {
		return PL_existence_error("mod_source", source_term);
	}

	/* resolve target and setter */
	target = NULL;
	setter = NULL;

	if (strcmp(target_type, "oscillator") == 0) {
		if (!get_typed_handle(target_term, "oscillator", &target_handle)) {
			return PL_type_error("oscillator", target_term);
		}
		if (target_handle < 0 || target_handle >= MAX_OSCILLATORS || !g_oscillators[target_handle].in_use) {
			return PL_existence_error("oscillator", target_term);
		}
		target = &g_oscillators[target_handle];
		if (strcmp(param, "frequency") == 0) {
			setter = set_oscillator_frequency;
		} else if (strcmp(param, "volume") == 0) {
			setter = set_oscillator_volume;
		} else {
			return PL_domain_error("oscillator_param", param_term);
		}
	} else if (strcmp(target_type, "pan") == 0) {
		target = get_effect_pointer(target_term);
		if (target == NULL) {
			return PL_type_error("effect", target_term);
		}
		if (strcmp(param, "pan") == 0) {
			setter = set_effect_pan;
		} else {
			return PL_domain_error("pan_param", param_term);
		}
	} else if (strcmp(target_type, "moog") == 0) {
		target = get_effect_pointer(target_term);
		if (target == NULL) {
			return PL_type_error("effect", target_term);
		}
		if (strcmp(param, "cutoff") == 0) {
			setter = set_moog_cutoff;
		} else if (strcmp(param, "resonance") == 0) {
			setter = set_moog_resonance;
		} else {
			return PL_domain_error("moog_param", param_term);
		}
	} else if (strcmp(target_type, "vca") == 0) {
		target = get_effect_pointer(target_term);
		if (target == NULL) {
			return PL_type_error("effect", target_term);
		}
		if (strcmp(param, "gain") == 0) {
			setter = set_vca_gain;
		} else {
			return PL_domain_error("vca_param", param_term);
		}
	} else if (strcmp(target_type, "ping_pong_delay") == 0) {
		target = get_effect_pointer(target_term);
		if (target == NULL) {
			return PL_type_error("effect", target_term);
		}
		if (strcmp(param, "delay") == 0) {
			setter = set_ping_pong_delay;
		} else {
			return PL_domain_error("ping_pong_delay_param", param_term);
		}
	} else if (strcmp(target_type, "granular") == 0) {
		if (!get_typed_handle(target_term, "granular", &target_handle)) {
			return PL_type_error("granular", target_term);
		}
		if (target_handle < 0 || target_handle >= MAX_GRANULAR_DELAYS || !g_granular_delays[target_handle].in_use) {
			return PL_existence_error("granular", target_term);
		}
		target = &g_granular_delays[target_handle];
		if (strcmp(param, "density") == 0) {
			setter = set_granular_density;
		} else if (strcmp(param, "pitch") == 0) {
			setter = set_granular_pitch;
		} else if (strcmp(param, "position") == 0) {
			setter = set_granular_position;
		} else if (strcmp(param, "size") == 0) {
			setter = set_granular_size;
		} else if (strcmp(param, "position_spray") == 0) {
			setter = set_granular_position_spray;
		} else if (strcmp(param, "size_spray") == 0) {
			setter = set_granular_size_spray;
		} else if (strcmp(param, "envelope") == 0) {
			setter = set_granular_envelope;
		} else if (strcmp(param, "regularity") == 0) {
			setter = set_granular_regularity;
		} else if (strcmp(param, "reverse") == 0) {
			setter = set_granular_reverse;
		} else if (strcmp(param, "pan") == 0) {
			setter = set_granular_pan;
		} else if (strcmp(param, "pan_spray") == 0) {
			setter = set_granular_pan_spray;
		} else if (strcmp(param, "recording") == 0) {
			setter = set_granular_recording;
		} else if (strcmp(param, "trigger") == 0) {
			setter = set_granular_trigger;
		} else if (strcmp(param, "reset") == 0) {
			setter = set_granular_reset;
		} else if (strcmp(param, "deviation_up") == 0) {
			setter = set_granular_deviation_up;
		} else if (strcmp(param, "deviation_down") == 0) {
			setter = set_granular_deviation_down;
		} else if (strcmp(param, "wet") == 0) {
			setter = set_granular_wet;
		} else {
			return PL_domain_error("granular_param", param_term);
		}
	} else if (strcmp(target_type, "reverb") == 0) {
		target = get_effect_pointer(target_term);
		if (target == NULL) {
			return PL_type_error("effect", target_term);
		}
		if (strcmp(param, "wet") == 0) {
			setter = set_reverb_wet;
		} else if (strcmp(param, "decay") == 0) {
			setter = set_reverb_decay;
		} else if (strcmp(param, "damping") == 0) {
			setter = set_reverb_damping;
		} else if (strcmp(param, "size") == 0) {
			setter = set_reverb_size;
		} else if (strcmp(param, "freeze") == 0) {
			setter = set_reverb_freeze;
		} else {
			return PL_domain_error("reverb_param", param_term);
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
	route->rate_mode = rate_mode;
	route->monitor = MA_FALSE;
	route->last_print_time = 0;
	route->last_printed_value = 0.0f;
	route->param_name = param;

	pthread_mutex_unlock(&g_mod_mutex);
	return unify_typed_handle(handle_term, "mod_route", slot);
}

/*
 * pl_mod_route_uninit()
 * Removes a modulation route.
 * mod_route_uninit(+Route)
 */
static foreign_t pl_mod_route_uninit(term_t handle_term)
{
	int slot;

	if (!get_typed_handle(handle_term, "mod_route", &slot)) {
		return PL_type_error("mod_route", handle_term);
	}
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

/*
 * pl_mod_route_monitor()
 * Enable or disable monitoring on a route.
 * mod_route_monitor(+Route, +Enable)
 */
static foreign_t pl_mod_route_monitor(term_t handle_term, term_t enable_term)
{
	int slot;
	int enable;

	if (!get_typed_handle(handle_term, "mod_route", &slot)) {
		return PL_type_error("mod_route", handle_term);
	}
	if (!PL_get_bool(enable_term, &enable)) {
		return PL_type_error("boolean", enable_term);
	}

	pthread_mutex_lock(&g_mod_mutex);
	if (slot < 0 || slot >= MAX_MOD_ROUTES || !g_mod_routes[slot].in_use) {
		pthread_mutex_unlock(&g_mod_mutex);
		return PL_existence_error("mod_route", handle_term);
	}
	g_mod_routes[slot].monitor = enable ? MA_TRUE : MA_FALSE;
	pthread_mutex_unlock(&g_mod_mutex);
	return TRUE;
}

/******************************************************************************
 * LFO
 *****************************************************************************/

/*
 * pl_mod_lfo_init()
 * Createas an LFO modulation source.
 * mod_lfo_init(+Type, +Freq, -Source)
 * Returns mod_source(N). Type is one of: sine, square, triangle, sawtooth
 */
static foreign_t pl_mod_lfo_init(term_t type_term, term_t freq_term, term_t handle_term)
{
	char *type_str;
	double freq;
	int slot;
	ma_waveform_type waveform_type;
	ma_waveform_config config;
	ma_result result;
	mod_source_t *src;

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
	return unify_typed_handle(handle_term, "mod_source", slot);
}

/*
 * set_lfo_frequency()
 * Setter for LFO frequency. Target is mod_source_t*.
 */
static void set_lfo_frequency(void *target, float value)
{
	mod_source_t *src = (mod_source_t*)target;
	ma_waveform_set_frequency(&src->source.waveform, value);
}

/*
 * pl_mod_lfo_set_frequency()
 * mod_lfo_set_frequency(+Source, +Freq)
 */
static foreign_t pl_mod_lfo_set_frequency(term_t handle_term, term_t freq_term)
{
	int slot;
	double freq;
	mod_source_t *src;

	if (!PL_get_float(freq_term, &freq)) return FALSE;
	GET_MOD_SOURCE(handle_term, src, slot);

	pthread_mutex_lock(&g_mod_mutex);
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
 * mod_lfo_get_frequency(+Source, -Freq)
 */
static foreign_t pl_mod_lfo_get_frequency(term_t handle_term, term_t freq_term)
{
	int slot;
	mod_source_t *src;
	float freq;

	GET_MOD_SOURCE(handle_term, src, slot);

	pthread_mutex_lock(&g_mod_mutex);
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
 * pl_mod_noise_init()
 * Creates a noise modulation source.
 * mod_noise_init(+Type, -Source)
 * Type is one of: white, pink, brownian
 */
static foreign_t pl_mod_noise_init(term_t type_term, term_t handle_term)
{
	char *type_str;
	int slot;
	ma_noise_type noise_type;
	ma_noise_config config;
	ma_result result;
	mod_source_t *src;

	ENSURE_ENGINE_INITIALIZED();

	if (!PL_get_atom_chars(type_term, &type_str)) return FALSE;

	if (strcmp(type_str, "white") == 0) {
		noise_type = ma_noise_type_white;
	} else if (strcmp(type_str, "pink") == 0) {
		noise_type = ma_noise_type_pink;
	} else if (strcmp(type_str, "brownian") == 0) {
		noise_type = ma_noise_type_brownian;
	} else {
		return PL_domain_error("noise_type", type_term);
	}

	pthread_mutex_lock(&g_mod_mutex);
	slot = allocate_source_slot();
	if (slot < 0) {
		pthread_mutex_unlock(&g_mod_mutex);
		return PL_resource_error("mod_source_slots");
	}

	src = &g_mod_sources[slot];
	src->type = MOD_SOURCE_NOISE;

	config = ma_noise_config_init(
			ma_format_f32,
			1,
			noise_type, 
			0,
			1.0);

	result = ma_noise_init(&config, NULL, &src->source.noise);
	if (result != MA_SUCCESS) {
		free_source_slot(slot);
		pthread_mutex_unlock(&g_mod_mutex);
		return FALSE;
	}

	pthread_mutex_unlock(&g_mod_mutex);
	return unify_typed_handle(handle_term, "mod_source", slot);
}


/******************************************************************************
 * SAMPLE & HOLD
 *****************************************************************************/

/*
 * pl_mod_source_set_sh()
 * Enables or disables sample & hold on a modulation source.
 * mod_source_set_sh(+Source, +IntervalMs)
 * IntervalMS of 0 disables S&H
 */
static foreign_t pl_mod_source_set_sh(term_t handle_term, term_t interval_term)
{
	int slot;
	double interval_ms;
	mod_source_t *src;
	ma_uint32 sample_rate;

	GET_MOD_SOURCE(handle_term, src, slot);
	if (!PL_get_float(interval_term, &interval_ms)) return FALSE;

	sample_rate = ma_engine_get_sample_rate(g_engine);

	pthread_mutex_lock(&g_mod_mutex);
	if (!src->in_use) {
		pthread_mutex_unlock(&g_mod_mutex);
		return PL_existence_error("mod_source", handle_term);
	}

	if (interval_ms <= 0) {
		src->sh_enabled = MA_FALSE;
		src->sh_interval = 0;
	} else {
		src->sh_enabled = MA_TRUE;
		src->sh_interval = (ma_uint32)(interval_ms * sample_rate / 1000.0);
		src->sh_counter = src->sh_interval; /* trigger immediate sample */
	}

	pthread_mutex_unlock(&g_mod_mutex);
	return TRUE;
}



/******************************************************************************
 * ENVELOPE
 *****************************************************************************/

/*
 * pl_mod_envelope_init()
 * Creates an ADBR envelope modulation source.
 * mod_envelope_init(+Attack, +Decay, +Break, +BreakLevel, +Release, +DurationMs, +Loop, -Source)
 * Returns mod_source(N). Attack, Decay, Break, Release are proportions (should sum to 1.0)
 * BreakLevel is the level at the break point (0.0-1.0)
 * DurationMs is the total envelope time in milliseconds
 * Loop is true/false
 */
static foreign_t pl_mod_envelope_init(
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
	mod_source_t *src;

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
	src->source.envelope.gate = MA_FALSE;
	src->source.envelope.release_from = 0.0f;

	pthread_mutex_unlock(&g_mod_mutex);
	return unify_typed_handle(handle_term, "mod_source", slot);
}

/*
 * pl_mod_envelope_trigger()
 * Triggers (restarts) an envelope from the beginning.
 * mod_envelope_trigger(+Source)
 */
static foreign_t pl_mod_envelope_trigger(term_t handle_term)
{
	int slot;
	mod_source_t *src;

	GET_MOD_SOURCE(handle_term, src, slot);

	pthread_mutex_lock(&g_mod_mutex);
	if (!src->in_use || src->type != MOD_SOURCE_ENVELOPE) {
		pthread_mutex_unlock(&g_mod_mutex);
		return FALSE;
	}

	src->source.envelope.stage = 0;
	src->source.envelope.stage_progress = 0.0f;

	pthread_mutex_unlock(&g_mod_mutex);
	return TRUE;
}

/*
 * envelope_gate_on()
 * Sets envelope gate to TRUE and restarts attack stage.
 * Called from keyboard event handler.
 */
void envelope_gate_on(int slot)
{
	mod_source_t *src;

	if (slot < 0 || slot >= MAX_MOD_SOURCES) return;

	pthread_mutex_lock(&g_mod_mutex);
	src = &g_mod_sources[slot];
	if (!src->in_use || src->type != MOD_SOURCE_ENVELOPE) {
		pthread_mutex_unlock(&g_mod_mutex);
		return;
	}

	src->source.envelope.gate = MA_TRUE;
	src->source.envelope.stage = 0;
	src->source.envelope.stage_progress = 0.0f;

	pthread_mutex_unlock(&g_mod_mutex);
}

/*
 * envelope_gate_off()
 * Sets envelope gate to FALSE and jumps to release stage.
 * Called from keyboard event handler.
 */
void envelope_gate_off(int slot)
{
	mod_source_t *src;
	float value;

	if (slot < 0 || slot >= MAX_MOD_SOURCES) return;

	pthread_mutex_lock(&g_mod_mutex);
	src = &g_mod_sources[slot];
	if (!src->in_use || src->type != MOD_SOURCE_ENVELOPE) {
		pthread_mutex_unlock(&g_mod_mutex);
		return;
	}

	/* calculate current value to release from */
	switch (src->source.envelope.stage) {
		case 0:
			value = src->source.envelope.stage_progress;
			break;
		case 1:
			value = 1.0f - src->source.envelope.stage_progress *
			        (1.0f - src->source.envelope.break_level);
			break;
		case 2:
			value = src->source.envelope.break_level;
			break;
		default:
			value = 0.0f;
			break;
	}

	src->source.envelope.gate = MA_FALSE;
	src->source.envelope.release_from = value;
	src->source.envelope.stage = 3;
	src->source.envelope.stage_progress = 0.0f;

	pthread_mutex_unlock(&g_mod_mutex);
}

/******************************************************************************
 * GAMEPAD CONTRL
 *****************************************************************************/

/*
 * pl_mod_gamepad_init()
 * mod_gamepad_init(+Gamepad, +Axis, -Source)
 * Creates a modulation source from a gamepad axis. Returns mod_source(N).
 */
static foreign_t pl_mod_gamepad_init(term_t gamepad_term, term_t axis_term, term_t handle_term)
{
	SDL_Gamepad *gp;
	atom_t axis_atom;
	SDL_GamepadAxis axis;
	int slot;
	int dpad_axis;
	mod_source_t *src;
	const char *axis_name;

	gp = get_gamepad_ptr(gamepad_term);
	if (gp == NULL) return FALSE;

	if (!PL_get_atom(axis_term, &axis_atom)) return FALSE;

	axis_name = PL_atom_chars(axis_atom);
	if (strcmp(axis_name, "dpad_x") == 0) {
		dpad_axis = 1;
		axis = 0;
	} else if (strcmp(axis_name, "dpad_y") == 0) {
		dpad_axis = 2;
		axis = 0;
	} else {
		if (!get_axis_from_atom(axis_atom, &axis)) return FALSE;
		dpad_axis = 0;
	}

	pthread_mutex_lock(&g_mod_mutex);

	slot = allocate_source_slot();
	if (slot < 0) {
		pthread_mutex_unlock(&g_mod_mutex);
		return PL_resource_error("mod_source_slots");
	}

	src = &g_mod_sources[slot];
	src->type = MOD_SOURCE_GAMEPAD;
	src->source.gamepad.gamepad = gp;
	src->source.gamepad.axis = axis;
	src->source.gamepad.dead_zone = 0.1f;
	src->source.gamepad.dpad_axis = dpad_axis;

	pthread_mutex_unlock(&g_mod_mutex);
	return unify_typed_handle(handle_term, "mod_source", slot);
}

/*
 * pl_mod_gamepad_button_init()
 * mod_gamepad_button_init(+Gamepad, +Button, +Mode, -Source)
 * Mode is: momentary, trigger, toggle, or a list of preset values for cycling.
 */
static foreign_t pl_mod_gamepad_button_init(term_t gamepad_term, term_t button_term,
                                            term_t mode_term, term_t handle_term)
{
	SDL_Gamepad *gp;
	atom_t button_atom;
	SDL_GamepadButton button;
	int slot;
	mod_source_t *src;
	atom_t mode_atom;
	term_t head, list;
	int i;

	gp = get_gamepad_ptr(gamepad_term);
	if (gp == NULL) return FALSE;

	if (!PL_get_atom(button_term, &button_atom)) return FALSE;
	if (!get_button_from_atom(button_atom, &button)) return FALSE;

	pthread_mutex_lock(&g_mod_mutex);

	slot = allocate_source_slot();
	if (slot < 0) {
		pthread_mutex_unlock(&g_mod_mutex);
		return PL_resource_error("mod_source_slots");
	}

	src = &g_mod_sources[slot];
	src->type = MOD_SOURCE_GAMEPAD_BUTTON;
	src->source.gamepad_button.gamepad = gp;
	src->source.gamepad_button.button = button;
	src->source.gamepad_button.prev_pressed = MA_FALSE;
	src->source.gamepad_button.toggle_state = MA_FALSE;
	src->source.gamepad_button.current_index = 0;
	src->source.gamepad_button.preset_count = 0;

	/* Parse mode: atom or list of floats */
	if (PL_get_atom(mode_term, &mode_atom)) {
		const char *mode_name = PL_atom_chars(mode_atom);
		if (strcmp(mode_name, "momentary") == 0) {
			src->source.gamepad_button.mode = BUTTON_MODE_MOMENTARY;
		} else if (strcmp(mode_name, "trigger") == 0) {
			src->source.gamepad_button.mode = BUTTON_MODE_TRIGGER;
		} else if (strcmp(mode_name, "toggle") == 0) {
			src->source.gamepad_button.mode = BUTTON_MODE_TOGGLE;
		} else {
			pthread_mutex_unlock(&g_mod_mutex);
			return PL_domain_error("button_mode", mode_term);
		}
	} else if (PL_is_list(mode_term)) {
		/* List of preset values for cycling mode */
		src->source.gamepad_button.mode = BUTTON_MODE_CYCLING;
		list = PL_copy_term_ref(mode_term);
		head = PL_new_term_ref();
		i = 0;
		while (PL_get_list(list, head, list) && i < 8) {
			double val;
			if (!PL_get_float(head, &val)) {
				pthread_mutex_unlock(&g_mod_mutex);
				return PL_type_error("float", head);
			}
			src->source.gamepad_button.presets[i++] = (float)val;
		}
		src->source.gamepad_button.preset_count = i;
		if (i == 0) {
			pthread_mutex_unlock(&g_mod_mutex);
			return PL_domain_error("non_empty_list", mode_term);
		}
	} else {
		pthread_mutex_unlock(&g_mod_mutex);
		return PL_type_error("button_mode_or_list", mode_term);
	}

	pthread_mutex_unlock(&g_mod_mutex);
	return unify_typed_handle(handle_term, "mod_source", slot);
}

/******************************************************************************
 * KEYBOARD CONTRL
 *****************************************************************************/

/*
 * pl_mod_keyboard_init()
 * mod_keyboard_init(+Key, +Options, -Source)
 * Creates a keyboard modulation source.
 * Key is atom: space, up, down, left, right, etc.
 * Options: attack=Ms, release=Ms, invert=Bool
 */

static foreign_t pl_mod_keyboard_init(term_t key_term, term_t opts_term, term_t handle_term)
{
	atom_t key_atom;
	SDL_Scancode scancode;
	int slot;
	mod_source_t *src;
	float attack_ms = 100.0f;
	float release_ms = 100.0f;
	ma_bool32 invert = MA_FALSE;

	if (!PL_get_atom(key_term, &key_atom)) return FALSE;
	if (!get_scancode_from_atom(key_atom, &scancode)) {
		return PL_domain_error("key_name", key_term);
	}

	get_param_float(opts_term, "attack", &attack_ms);
	get_param_float(opts_term, "release", &release_ms);
	get_param_bool(opts_term, "invert", &invert);

	pthread_mutex_lock(&g_mod_mutex);
	slot = allocate_source_slot();
	if (slot < 0) {
		pthread_mutex_unlock(&g_mod_mutex);
		return PL_resource_error("mod_source_slots");
	}

	src = &g_mod_sources[slot];
	src->type = MOD_SOURCE_KEYBOARD;
	src->source.keyboard.scancode = scancode;
	src->source.keyboard.attack_ms = attack_ms;
	src->source.keyboard.release_ms = release_ms;
	src->source.keyboard.value = 0.0f;
	src->source.keyboard.invert = invert;
	src->current_value = invert ? 1.0f : 0.0f;

	pthread_mutex_unlock(&g_mod_mutex);

	return PL_unify_term(handle_term,
			PL_FUNCTOR, PL_new_functor(PL_new_atom("mod_source"), 1),
			PL_INT, slot);
}

/******************************************************************************
 * REGISTRATION
 *****************************************************************************/

install_t mod_register_predicates(void)
{
	PL_register_foreign("mod_lfo_init", 3, pl_mod_lfo_init, 0);
	PL_register_foreign("mod_lfo_set_frequency", 2, pl_mod_lfo_set_frequency, 0);
	PL_register_foreign("mod_lfo_get_frequency", 2, pl_mod_lfo_get_frequency, 0);
	PL_register_foreign("mod_envelope_init", 8, pl_mod_envelope_init, 0);
	PL_register_foreign("mod_envelope_trigger", 1, pl_mod_envelope_trigger, 0);
	PL_register_foreign("mod_source_uninit", 1, pl_mod_source_uninit, 0);
	PL_register_foreign("mod_route_init", 9, pl_mod_route_init, 0);
	PL_register_foreign("mod_route_uninit", 1, pl_mod_route_uninit, 0);
	PL_register_foreign("mod_route_monitor", 2, pl_mod_route_monitor, 0);
	PL_register_foreign("mod_gamepad_init", 3, pl_mod_gamepad_init, 0);
	PL_register_foreign("mod_gamepad_button_init", 4, pl_mod_gamepad_button_init, 0);
	PL_register_foreign("mod_keyboard_init", 3, pl_mod_keyboard_init, 0);
	PL_register_foreign("mod_noise_init", 2, pl_mod_noise_init, 0);
	PL_register_foreign("mod_source_set_sh", 2, pl_mod_source_set_sh, 0);
}
