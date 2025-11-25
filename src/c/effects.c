/*
 * effects.c - Audio effects for granular sampler
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */


#if defined(__ARM_NEON) || defined(__aarch64__)
#include <arm_neon.h>
#endif


#include "sampler_internal.h"


/******************************************************************************
 * SHARED UTILITIES
 *****************************************************************************/

/* Macro to define parameter parsers with error checking */
#define DEFINE_GET_PARAM(suffix, type, get_code) \
static ma_bool32 get_param_##suffix(term_t params, const char* key, type* value) \
{ \
	term_t head = PL_new_term_ref(); \
	term_t tail = PL_new_term_ref(); \
	term_t tmp = PL_new_term_ref(); \
	functor_t equals_functor = PL_new_functor(PL_new_atom("="), 2); \
	\
	if (!PL_put_term(tmp, params)) \
		return MA_FALSE; \
	\
	while (PL_get_list(tmp, head, tail)) { \
		term_t arg1 = PL_new_term_ref(); \
		term_t arg2 = PL_new_term_ref(); \
		functor_t f; \
		char* key_str; \
		\
		if (PL_get_functor(head, &f) && f == equals_functor) { \
			if (!PL_get_arg(1, head, arg1) || !PL_get_arg(2, head, arg2)) \
				return MA_FALSE; \
			\
			if (PL_get_atom_chars(arg1, &key_str) && strcmp(key_str, key) == 0) { \
				get_code \
			} \
		} \
		if (!PL_put_term(tmp, tail)) \
			return MA_FALSE; \
	} \
	return MA_FALSE; \
}

DEFINE_GET_PARAM(int, int, {
	if (!PL_get_integer(arg2, value)) {
		PL_type_error("integer", arg2);
		return MA_FALSE;
	}
	return MA_TRUE;
})

DEFINE_GET_PARAM(float, float, {
	double dval;
	if (!PL_get_float(arg2, &dval)) {
		PL_type_error("float", arg2);
		return MA_FALSE;
	}
	*value = (float)dval;
	return MA_TRUE;
})

DEFINE_GET_PARAM(bool, ma_bool32, {
	int bval;
	if (!PL_get_bool(arg2, &bval)) {
		PL_type_error("bool", arg2);
		return MA_FALSE;
	}
	*value = bval ? MA_TRUE : MA_FALSE;
	return MA_TRUE;
})

DEFINE_GET_PARAM(double, double, {
  	if (!PL_get_float(arg2, value)) {
  		PL_type_error("float", arg2);
  		return MA_FALSE;
  	}
  	return MA_TRUE;
  })

/*
 * init_effect_node_base()
 * Initialize effect node base (common boilerplate)
 */
static ma_result init_effect_node_base(ma_node_base* node, ma_node_vtable* vtable)
{
	ma_node_config node_config;
	ma_uint32 channels[1];

	channels[0] = ma_engine_get_channels(g_engine);

	node_config = ma_node_config_init();
	node_config.vtable = vtable;
	node_config.pInputChannels = channels;
	node_config.pOutputChannels = channels;

	return ma_node_init(ma_engine_get_node_graph(g_engine), &node_config, NULL, node);
}

/* Macro to extract effect handle components */
#define GET_EFFECT_FROM_HANDLE(effect_handle, sound_slot_var, effect_ptr_var) \
  	do { \
  		term_t sound_handle_term = PL_new_term_ref(); \
  		term_t effect_ptr_term = PL_new_term_ref(); \
  		functor_t effect_functor = PL_new_functor(PL_new_atom("effect"), 2); \
  		\
  		if (!PL_is_functor(effect_handle, effect_functor)) { \
  			return PL_type_error("effect_handle", effect_handle); \
  		} \
  		if (!PL_get_arg(1, effect_handle, sound_handle_term)) { \
  			return FALSE; \
  		} \
  		if (!PL_get_arg(2, effect_handle, effect_ptr_term)) { \
  			return FALSE; \
  		} \
  		if (!PL_get_integer(sound_handle_term, &sound_slot_var)) { \
  			return PL_type_error("integer", sound_handle_term); \
  		} \
  		if (sound_slot_var < 0 || sound_slot_var >= MAX_SOUNDS || !g_sounds[sound_slot_var].in_use) { \
  			return PL_existence_error("sound", sound_handle_term); \
  		} \
  		if (!PL_get_pointer(effect_ptr_term, &effect_ptr_var)) { \
  			return PL_type_error("pointer", effect_ptr_term); \
  		} \
  	} while(0)


/******************************************************************************
 * BITCRUSH EFFECT
 *****************************************************************************/

/*
 * bitcrush_process_pcm_frames()
 * Process audio through bitcrush effect
 */
static void bitcrush_process_pcm_frames(
		ma_node* node,
		const float** frames_in,
		ma_uint32* frame_count_in,
		float** frames_out,
		ma_uint32* frame_count_out)
{
	bitcrush_node_t* bitcrush;
  	ma_uint32 channels;
  	ma_uint32 frame_count;
  	ma_uint32 total_samples;
  	float levels;
  	ma_uint32 frame;
  	ma_uint32 channel;
  	const float* input;
  	float* output;

	bitcrush = (bitcrush_node_t*)node;
	channels = ma_node_get_output_channels(node, 0);
	frame_count = *frame_count_out;
	total_samples = frame_count * channels;
	input = frames_in[0];
	output = frames_out[0];

	/* bit depth reduction */
	if (bitcrush->target_bits < 16 && bitcrush->target_bits > 0) {
		levels = (float)(1 << (bitcrush->target_bits - 1));

#if defined(__ARM_NEON) || defined(__aarch64__)
		/* NEON-optimized path; process 4 samples at a time */
		float32x4_t levels_vec = vdupq_n_f32(levels);
		float32x4_t inv_levels_vec = vdupq_n_f32(1.0f / levels);
		ma_uint32 vec_count = total_samples / 4;
		ma_uint32 vec_samples = vec_count * 4;
		ma_uint32 i;

		for (i = 0; i < vec_samples; i += 4) {
			float32x4_t samples = vld1q_f32(&input[i]);
			samples = vmulq_f32(samples, levels_vec);
			samples = vrndnq_f32(samples);
			samples = vmulq_f32(samples, inv_levels_vec);
			vst1q_f32(&output[i], samples);
		}

		/* handle remaining samples with scalar code */
		for (i = vec_samples; i < total_samples; i++) {
			output[i] = roundf(input[i] * levels) / levels;
		}
#else
		/* scalar fallback */
		for (ma_uint32 i = 0; i < total_samples; i++) {
			output[i] = roundf(input[i] * levels) / levels;
		}
#endif
	} else {
		/* no bit reduction, just copy through */
		memcpy(output, input, total_samples * sizeof(float));
	}

	/* sample rate reduction (sample-and-hold) */
	if (bitcrush->target_sample_rate > 0 && bitcrush->hold_interval > 0) {
		for (frame = 0; frame < frame_count; frame++) {
			if (bitcrush->hold_counter >= bitcrush->hold_interval) {
				/* capture new sample */
				for (channel = 0; channel < channels; channel++) {
					bitcrush->hold_samples[channel] = output[frame * channels + channel];
				}
				bitcrush->hold_counter = 0;
			} else {
				/* use held sample */
				for (channel = 0; channel < channels; channel++) {
					output[frame * channels + channel] = bitcrush->hold_samples[channel];
				}
			}
			bitcrush->hold_counter++;
		}
	}

	*frame_count_in = frame_count;
	*frame_count_out = frame_count;
}

static ma_node_vtable bitcrush_vtable =
{
	bitcrush_process_pcm_frames,
	NULL, 	/* onGetRequiredInputFrameCount */
	1,		/* 1 input */
	1,		/* 1 output */
	0		/* default flags */
};

/*
 * init_bitcrush_node()
 * Initialize bitcrush effect node
 */
static ma_result init_bitcrush_node(bitcrush_node_t* node, int bits, int sample_rate)
{
	ma_uint32 channels;
	ma_result result;

	result = init_effect_node_base(&node->base, &bitcrush_vtable);
	if (result != MA_SUCCESS) {
		return result;
	}

	channels = ma_engine_get_channels(g_engine);

	node->target_bits = (ma_uint32)bits;
	node->target_sample_rate = (ma_uint32)sample_rate;

	if (sample_rate > 0) {
		node->hold_samples = (float *)malloc(channels * sizeof(float));
		if (node->hold_samples == NULL) {
			ma_node_uninit(&node->base, NULL);
			return MA_OUT_OF_MEMORY;
		}
		memset(node->hold_samples, 0, channels * sizeof(float));
		node->hold_interval = ma_engine_get_sample_rate(g_engine) / sample_rate;
		node->hold_counter = 0;
	} else {
		node->hold_samples = NULL;
		node->hold_interval = 0;
		node->hold_counter = 0;
	}

	return MA_SUCCESS;
}

/*
 * attach_bitcrush_effect()
 * Validate parameters and attach bitcrush effect to sound
 */
static ma_result attach_bitcrush_effect(term_t params, sound_slot_t* sound_slot, ma_node_base** out_effect_node)
{
	int bits, sample_rate;
	bitcrush_node_t* bitcrush;
	ma_result result;

	if (!get_param_int(params, "bits", &bits)) {
		PL_existence_error("parameter", PL_new_atom("bits"));
		return MA_ERROR;
	}

	if (bits < 1 || bits > 16) {
		PL_domain_error("bit_range_1_to_16", params);
		return MA_ERROR;
	}

	if (!get_param_int(params, "sample_rate", &sample_rate)) {
		sample_rate = 0;
	}

	if (sample_rate < 0) {
		PL_domain_error("non_negative_integer", params);
		return MA_ERROR;
	}

	bitcrush = (bitcrush_node_t*)malloc(sizeof(bitcrush_node_t));
	if (!bitcrush) {
		return MA_OUT_OF_MEMORY;
	}

	result = init_bitcrush_node(bitcrush, bits, sample_rate);
	if (result != MA_SUCCESS) {
		free(bitcrush);
		return result;
	}

	result = attach_effect_node_to_sound(sound_slot, &bitcrush->base, EFFECT_BITCRUSH);
	if (result != MA_SUCCESS) {
		if (bitcrush->hold_samples) {
			free(bitcrush->hold_samples);
		}
		ma_node_uninit(&bitcrush->base, NULL);
		free(bitcrush);
		return result;
	}

	*out_effect_node = &bitcrush->base;
	return MA_SUCCESS;
}

/*
 * query_bitcrush_params()
 * Build parameter list for bitcrush effect
 */
static int query_bitcrush_params(bitcrush_node_t* bitcrush, term_t params_list)
{
	term_t param_args = PL_new_term_refs(2);
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);
	term_t param_term = PL_new_term_ref();

	PL_put_atom_chars(param_args+0, "sample_rate");
	if (!PL_put_integer(param_args+1, bitcrush->target_sample_rate)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "bits");
	if (!PL_put_integer(param_args+1, bitcrush->target_bits)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	return TRUE;
}

/*
 * set_bitcrush_parameters()
 * Set parameters for bitcrush effect from Prolog term list
 */
static foreign_t set_bitcrush_parameters(bitcrush_node_t* bitcrush, term_t params_list)
{
	term_t list = PL_copy_term_ref(params_list);
	term_t head = PL_new_term_ref();
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);

	while (PL_get_list(list, head, list)) {
		term_t key_term = PL_new_term_ref();
		term_t value_term = PL_new_term_ref();
		char* param_name;

		if (!PL_is_functor(head, eq_functor)) {
			return PL_type_error("key_value_pair", head);
		}
		if (!PL_get_arg(1, head, key_term)) {
			return FALSE;
		}
		if (!PL_get_arg(2, head, value_term)) {
			return FALSE;
		}
		if (!PL_get_atom_chars(key_term, &param_name)) {
			return PL_type_error("atom", key_term);
		}

		if (strcmp(param_name, "bits") == 0) {
			int bits;
			if (!PL_get_integer(value_term, &bits)) {
				return PL_type_error("integer", value_term);
			}
			if (bits < 1 || bits > 16) {
				return PL_domain_error("bits_range", value_term);
			}
			bitcrush->target_bits = bits;
		} else if (strcmp(param_name, "sample_rate") == 0) {
			int sample_rate;
			if (!PL_get_integer(value_term, &sample_rate)) {
				return PL_type_error("integer", value_term);
			}
			if (sample_rate < 0) {
				return PL_domain_error("sample_rate_range", value_term);
			}
			bitcrush->target_sample_rate = sample_rate;
			if (sample_rate > 0) {
				ma_uint32 engine_sample_rate = ma_engine_get_sample_rate(g_engine);
				bitcrush->hold_interval = engine_sample_rate / sample_rate;
			}
		} else {
			return PL_domain_error("bitcrush_parameter", key_term);
		}
	}

	if (!PL_get_nil(list)) {
		return PL_type_error("list", params_list);
	}

	return TRUE;
}


/******************************************************************************
 * ADBR ENVELOPE EFFECT
 *****************************************************************************/

/*
 * adbr_process_pcm_frames()
 * Process audio through ADBR envelope effect
 */
static void adbr_process_pcm_frames(
	ma_node* node,
	const float** frames_in,
	ma_uint32* frame_count_in,
	float** frames_out,
	ma_uint32* frame_count_out)
{
	adbr_envelope_node_t* envelope;
	ma_uint32 channels;
	ma_uint32 frame_count;
	ma_uint32 sample_rate;
	const float* input;
	float* output;
	ma_uint32 frame;
	ma_uint32 channel;
	float envelope_value;
	float attack_frames;
	float decay_frames;
	float break_frames;
	float release_frames;
	float attack_increment;
	float decay_increment;
	float break_increment;
	float release_increment;

	envelope = (adbr_envelope_node_t*)node;
	channels = ma_node_get_output_channels(node, 0);
	frame_count = *frame_count_out;
	sample_rate = ma_engine_get_sample_rate(g_engine);
	input = frames_in[0];
	output = frames_out[0];

	/* Pre-calculate stage durations and increments */
	attack_frames = (envelope->duration_ms / 1000.0f) * envelope->attack * sample_rate;
	decay_frames = (envelope->duration_ms / 1000.0f) * envelope->decay * sample_rate;
	break_frames = (envelope->duration_ms / 1000.0f) * envelope->Break * sample_rate;
	release_frames = (envelope->duration_ms / 1000.0f) * envelope->release * sample_rate;

	attack_increment = attack_frames > 0.0f ? 1.0f / attack_frames : 0.0f;
	decay_increment = decay_frames > 0.0f ? 1.0f / decay_frames : 0.0f;
	break_increment = break_frames > 0.0f ? 1.0f / break_frames : 0.0f;
	release_increment = release_frames > 0.0f ? 1.0f / release_frames : 0.0f;

	for (frame = 0; frame < frame_count; frame++) {
		/* Calculate envelope value based on current stage */
		switch (envelope->stage) {
			case 0: /* Attack: 0.0 -> 1.0 */
				envelope_value = envelope->stage_progress;
				break;

			case 1: /* Decay: 1.0 -> break_level */
				envelope_value = 1.0f - envelope->stage_progress * (1.0f - envelope->break_level);
				break;

			case 2: /* Break: hold at break_level */
				envelope_value = envelope->break_level;
				break;

			case 3: /* Release: break_level -> 0.0 */
				envelope_value = envelope->break_level * (1.0f - envelope->stage_progress);
				break;

			default: /* Done */
				envelope_value = 0.0f;
				break;
		}

		/* Apply envelope to all channels */
		for (channel = 0; channel < channels; channel++) {
			output[frame * channels + channel] = input[frame * channels + channel] * envelope_value;
		}

		/* Update stage progress */
		switch (envelope->stage) {
			case 0:
				envelope->stage_progress += attack_increment;
				break;
			case 1:
				envelope->stage_progress += decay_increment;
				break;
			case 2:
				envelope->stage_progress += break_increment;
				break;
			case 3:
				envelope->stage_progress += release_increment;
				break;
			default:
				break;
		}

		/* Check for stage transition */
		if (envelope->stage_progress >= 1.0f) {
			envelope->stage++;
			envelope->stage_progress = 0.0f;
			if (envelope->stage > 3 && envelope->loop) {
				envelope->stage = 0;
			}
		}
	}

	*frame_count_in = frame_count;
	*frame_count_out = frame_count;
}

static ma_node_vtable adbr_envelope_vtable =
{
	adbr_process_pcm_frames,
	NULL,
	1,
	1,
	0
};

/*
 * init_adbr_envelope_node()
 * Initialize ADBR envelope node
 */
static ma_result init_adbr_envelope_node(adbr_envelope_node_t* node,
		float attack,
		float decay,
		float Break,
		float release,
		float break_level,
		float duration_ms,
		ma_bool32 loop)
{
	ma_result result;

	result = init_effect_node_base(&node->base, &adbr_envelope_vtable);
	if (result != MA_SUCCESS) {
		return result;
	}

	node->attack = attack;
	node->decay = decay;
	node->Break = Break;
	node->release = release;
	node->break_level = break_level;
	node->duration_ms = duration_ms;
	node->loop = loop;

	node->stage = 0;
	node->stage_progress = 0;

	return MA_SUCCESS;
}

/*
 * attach_adbr_envelope()
 * Validate parameters and attach ADBR envelope to sound
 */
static ma_result attach_adbr_envelope(term_t params, sound_slot_t* sound_slot, ma_node_base** out_effect_node)
{
	float attack, decay, Break, release, break_level, duration_ms;
	ma_bool32 loop;
	adbr_envelope_node_t* adbr_envelope;
	ma_result result;

	if (!get_param_float(params, "attack", &attack)) {
		PL_existence_error("parameter", PL_new_atom("attack"));
		return MA_ERROR;
	}

	if (!get_param_float(params, "decay", &decay)) {
		PL_existence_error("parameter", PL_new_atom("decay"));
		return MA_ERROR;
	}

	if (!get_param_float(params, "break", &Break)) {
		PL_existence_error("parameter", PL_new_atom("break"));
		return MA_ERROR;
	}

	if (!get_param_float(params, "break_level", &break_level)) {
		PL_existence_error("parameter", PL_new_atom("break_level"));
		return MA_ERROR;
	}

	if (!get_param_float(params, "duration_ms", &duration_ms)) {
		PL_existence_error("parameter", PL_new_atom("duration_ms"));
		return MA_ERROR;
	}

	if (!get_param_bool(params, "loop", &loop)) {
		loop = MA_FALSE;
	}

	if (attack < 0.0f || decay < 0.0f || Break < 0.0f || break_level < 0.0f || duration_ms < 0.0f) {
		PL_domain_error("params_positive", params);
		return MA_ERROR;
	}

	if (attack + decay + Break > 1.0f || break_level > 1.0f) {
		PL_domain_error("envelope_params_as_proportions", params);
		return MA_ERROR;
	}

	release = 1.0f - attack - decay - Break;

	adbr_envelope = (adbr_envelope_node_t*)malloc(sizeof(adbr_envelope_node_t));
	if (!adbr_envelope) {
		return MA_OUT_OF_MEMORY;
	}

	result = init_adbr_envelope_node(adbr_envelope, attack, decay, Break, release, break_level, duration_ms, loop);
	if (result != MA_SUCCESS) {
		free(adbr_envelope);
		return result;
	}

	result = attach_effect_node_to_sound(sound_slot, &adbr_envelope->base, EFFECT_ENVELOPE);
	if (result != MA_SUCCESS) {
		ma_node_uninit(&adbr_envelope->base, NULL);
		free(adbr_envelope);
		return result;
	}

	*out_effect_node = &adbr_envelope->base;
	return MA_SUCCESS;
}

/*
 * query_envelope_params()
 * Build parameter list for envelope effect
 */
static int query_envelope_params(adbr_envelope_node_t* envelope, term_t params_list)
{
	term_t param_args = PL_new_term_refs(2);
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);
	term_t param_term = PL_new_term_ref();

	PL_put_atom_chars(param_args+0, "loop");
	if (!PL_put_integer(param_args+1, envelope->loop)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "duration_ms");
	if (!PL_put_float(param_args+1, envelope->duration_ms)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "break_level");
	if (!PL_put_float(param_args+1, envelope->break_level)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "break");
	if (!PL_put_float(param_args+1, envelope->Break)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "decay");
	if (!PL_put_float(param_args+1, envelope->decay)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "attack");
	if (!PL_put_float(param_args+1, envelope->attack)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	return TRUE;
}

/*
 * set_envelope_parameters()
 * Set parameters for envelope effect from Prolog term list
 */
static foreign_t set_envelope_parameters(adbr_envelope_node_t* envelope, term_t params_list)
{
	term_t list = PL_copy_term_ref(params_list);
	term_t head = PL_new_term_ref();
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);

	while (PL_get_list(list, head, list)) {
		term_t key_term = PL_new_term_ref();
		term_t value_term = PL_new_term_ref();
		char* param_name;

		if (!PL_is_functor(head, eq_functor)) {
			return PL_type_error("key_value_pair", head);
		}
		if (!PL_get_arg(1, head, key_term)) {
			return FALSE;
		}
		if (!PL_get_arg(2, head, value_term)) {
			return FALSE;
		}
		if (!PL_get_atom_chars(key_term, &param_name)) {
			return PL_type_error("atom", key_term);
		}

		if (strcmp(param_name, "attack") == 0) {
			double value;
			if (!PL_get_float(value_term, &value)) {
				return PL_type_error("float", value_term);
			}
			envelope->attack = (float)value;
		} else if (strcmp(param_name, "decay") == 0) {
			double value;
			if (!PL_get_float(value_term, &value)) {
				return PL_type_error("float", value_term);
			}
			envelope->decay = (float)value;
		} else if (strcmp(param_name, "break") == 0) {
			double value;
			if (!PL_get_float(value_term, &value)) {
				return PL_type_error("float", value_term);
			}
			envelope->Break = (float)value;
		} else if (strcmp(param_name, "break_level") == 0) {
			double value;
			if (!PL_get_float(value_term, &value)) {
				return PL_type_error("float", value_term);
			}
			envelope->break_level = (float)value;
		} else if (strcmp(param_name, "duration_ms") == 0) {
			double value;
			if (!PL_get_float(value_term, &value)) {
				return PL_type_error("float", value_term);
			}
			envelope->duration_ms = (float)value;
		} else if (strcmp(param_name, "loop") == 0) {
			int value;
			if (!PL_get_bool(value_term, &value)) {
				return PL_type_error("boolean", value_term);
			}
			envelope->loop = value ? MA_TRUE : MA_FALSE;
		} else {
			return PL_domain_error("envelope_parameter", key_term);
		}
	}

	if (!PL_get_nil(list)) {
		return PL_type_error("list", params_list);
	}

	/* Validate envelope proportions after all parameters are updated */
	float sum = envelope->attack + envelope->decay + envelope->Break;
	if (sum > 1.0f) {
		return PL_domain_error("envelope_proportions_exceed_1.0", params_list);
	}
	if (envelope->break_level < 0.0f || envelope->break_level > 1.0f) {
		return PL_domain_error("break_level_out_of_range", params_list);
	}
	if (envelope->duration_ms < 0.0f) {
		return PL_domain_error("duration_negative", params_list);
	}

	return TRUE;
}


/******************************************************************************
 * LOW-PASS FILTER EFFECT
 *****************************************************************************/

/*
 * init_lpf_node()
 * Initialize low pass filter node
 */
static ma_result init_lpf_node(lpf_node_t* lpf, double cutoff, ma_uint32 order)
{
	ma_lpf_node_config lpf_config;
  	ma_uint32 channels;
  	ma_uint32 sample_rate;
  	ma_result result;

	get_engine_format_info(NULL, &channels, &sample_rate);

	lpf_config = ma_lpf_node_config_init(channels, sample_rate, cutoff, order);
	result = ma_lpf_node_init(ma_engine_get_node_graph(g_engine), &lpf_config, NULL, &lpf->node);

	if (result == MA_SUCCESS) {
		lpf->cutoff_frequency = cutoff;
		lpf->order = order;
	}

	return result;
}

/*
 * attach_lpf_effect()
 * Validate parameters and attach LPF to sound
 */
static ma_result attach_lpf_effect(term_t params, sound_slot_t* sound_slot, ma_node_base** out_effect_node)
{
	double cutoff;
  	int order_int;
  	lpf_node_t* lpf;
  	ma_result result;

  	if (!get_param_double(params, "cutoff", &cutoff)) {
  		PL_existence_error("parameter", PL_new_atom("cutoff"));
  		return MA_ERROR;
  	}

  	if (cutoff < 0.0) {
  		PL_domain_error("non-negative_frequency", params);
  		return MA_ERROR;
  	}

  	if (!get_param_int(params, "order", &order_int)) {
  		order_int = 2;  /* Default to 2nd order */
  	}

  	if (order_int < 0) {
  		PL_domain_error("non_negative_integer", params);
  		return MA_ERROR;
  	}

	lpf = (lpf_node_t*)malloc(sizeof(lpf_node_t));
	if (!lpf) {
		return MA_OUT_OF_MEMORY;
	}

	result = init_lpf_node(lpf, cutoff, (ma_uint32)order_int);
	if (result != MA_SUCCESS) {
		free(lpf);
		return result;
	}

	result = attach_effect_node_to_sound(sound_slot, &lpf->node.baseNode, EFFECT_LPF);
	if (result != MA_SUCCESS) {
		ma_lpf_node_uninit(&lpf->node, NULL);
		free(lpf);
		return result;
	}

	*out_effect_node = &lpf->node.baseNode;
	return MA_SUCCESS;
}

/*
 * query_lpf_params()
 * Build parameter list for lpf effect
 */
static int query_lpf_params(lpf_node_t* lpf, term_t params_list)
{
	term_t param_args = PL_new_term_refs(2);
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);
	term_t param_term = PL_new_term_ref();

	PL_put_atom_chars(param_args+0, "order");
	if (!PL_put_integer(param_args+1, lpf->order)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "cutoff");
	if (!PL_put_float(param_args+1, lpf->cutoff_frequency)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	return TRUE;
}

/*
 * set_lpf_parameters()
 * Set parameters for lpf effect from Prolog term list
 */
static foreign_t set_lpf_parameters(lpf_node_t* lpf, term_t params_list)
{
	term_t list = PL_copy_term_ref(params_list);
	term_t head = PL_new_term_ref();
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);

	while (PL_get_list(list, head, list)) {
		term_t key_term = PL_new_term_ref();
		term_t value_term = PL_new_term_ref();
		char* param_name;

		if (!PL_is_functor(head, eq_functor)) {
			return PL_type_error("key_value_pair", head);
		}
		if (!PL_get_arg(1, head, key_term)) {
			return FALSE;
		}
		if (!PL_get_arg(2, head, value_term)) {
			return FALSE;
		}
		if (!PL_get_atom_chars(key_term, &param_name)) {
			return PL_type_error("atom", key_term);
		}

		if (strcmp(param_name, "cutoff") == 0) {
			double cutoff;
			ma_uint32 channels, sample_rate;

			if (!PL_get_float(value_term, &cutoff)) {
				return PL_type_error("float", value_term);
			}
			if (cutoff < 0.0) {
				return PL_domain_error("non-negative_frequency", value_term);
			}
			lpf->cutoff_frequency = cutoff;

			get_engine_format_info(NULL, &channels, &sample_rate);
			ma_lpf_config config = ma_lpf_config_init(
					ma_format_f32,
					channels,
					sample_rate,
					cutoff,
					lpf->order
					);
			ma_lpf_node_reinit(&config, &lpf->node);
		} else if (strcmp(param_name, "order") == 0) {
			int order_int;
			ma_uint32 channels, sample_rate;

			if (!PL_get_integer(value_term, &order_int)) {
				return PL_type_error("integer", value_term);
			}
			if (order_int < 0) {
				return PL_domain_error("non_negative_integer", value_term);
			}
			lpf->order = (ma_uint32)order_int;

			get_engine_format_info(NULL, &channels, &sample_rate);
			ma_lpf_config config = ma_lpf_config_init(
					ma_format_f32,
					channels,
					sample_rate,
					lpf->cutoff_frequency,
					lpf->order
					);
			ma_lpf_node_reinit(&config, &lpf->node);
		} else {
			return PL_domain_error("lpf_parameter", key_term);
		}
	}

	if (!PL_get_nil(list)) {
		return PL_type_error("list", params_list);
	}

	return TRUE;
}


/******************************************************************************
 * HIGH-PASS FILTER EFFECT
 *****************************************************************************/

/*
 * init_hpf_node()
 * Initialize high pass filter node
 */
static ma_result init_hpf_node(hpf_node_t* hpf, double cutoff, ma_uint32 order)
{
	ma_hpf_node_config hpf_config;
  	ma_uint32 channels;
  	ma_uint32 sample_rate;
  	ma_result result;

	get_engine_format_info(NULL, &channels, &sample_rate);

	hpf_config = ma_hpf_node_config_init(channels, sample_rate, cutoff, order);
	result = ma_hpf_node_init(ma_engine_get_node_graph(g_engine), &hpf_config, NULL, &hpf->node);

	if (result == MA_SUCCESS) {
		hpf->cutoff_frequency = cutoff;
		hpf->order = order;
	}

	return result;
}

/*
 * attach_hpf_effect()
 * Validate parameters and attach HPF to sound
 */
static ma_result attach_hpf_effect(term_t params, sound_slot_t* sound_slot, ma_node_base** out_effect_node)
{
	double cutoff;
  	int order_int;
  	hpf_node_t* hpf;
  	ma_result result;

  	if (!get_param_double(params, "cutoff", &cutoff)) {
  		PL_existence_error("parameter", PL_new_atom("cutoff"));
  		return MA_ERROR;
  	}

  	if (cutoff < 0.0) {
  		PL_domain_error("non-negative_frequency", params);
  		return MA_ERROR;
  	}

  	if (!get_param_int(params, "order", &order_int)) {
  		order_int = 2;  /* Default to 2nd order */
  	}

  	if (order_int < 0) {
  		PL_domain_error("non_negative_integer", params);
  		return MA_ERROR;
  	}

	hpf = (hpf_node_t*)malloc(sizeof(hpf_node_t));
	if (!hpf) {
		return MA_OUT_OF_MEMORY;
	}

	result = init_hpf_node(hpf, cutoff, (ma_uint32)order_int);
	if (result != MA_SUCCESS) {
		free(hpf);
		return result;
	}

	result = attach_effect_node_to_sound(sound_slot, &hpf->node.baseNode, EFFECT_HPF);
	if (result != MA_SUCCESS) {
		ma_hpf_node_uninit(&hpf->node, NULL);
		free(hpf);
		return result;
	}

	*out_effect_node = &hpf->node.baseNode;
	return MA_SUCCESS;
}

/*
 * query_hpf_params()
 * Build parameter list for hpf effect
 */
static int query_hpf_params(hpf_node_t* hpf, term_t params_list)
{
	term_t param_args = PL_new_term_refs(2);
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);
	term_t param_term = PL_new_term_ref();

	PL_put_atom_chars(param_args+0, "order");
	if (!PL_put_integer(param_args+1, hpf->order)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "cutoff");
	if (!PL_put_float(param_args+1, hpf->cutoff_frequency)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	return TRUE;
}

/*
 * set_hpf_parameters()
 * Set parameters for hpf effect from Prolog term list
 */
static foreign_t set_hpf_parameters(hpf_node_t* hpf, term_t params_list)
{
	term_t list = PL_copy_term_ref(params_list);
	term_t head = PL_new_term_ref();
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);

	while (PL_get_list(list, head, list)) {
		term_t key_term = PL_new_term_ref();
		term_t value_term = PL_new_term_ref();
		char* param_name;

		if (!PL_is_functor(head, eq_functor)) {
			return PL_type_error("key_value_pair", head);
		}
		if (!PL_get_arg(1, head, key_term)) {
			return FALSE;
		}
		if (!PL_get_arg(2, head, value_term)) {
			return FALSE;
		}
		if (!PL_get_atom_chars(key_term, &param_name)) {
			return PL_type_error("atom", key_term);
		}

		if (strcmp(param_name, "cutoff") == 0) {
			double cutoff;
			ma_uint32 channels, sample_rate;

			if (!PL_get_float(value_term, &cutoff)) {
				return PL_type_error("float", value_term);
			}
			if (cutoff < 0.0) {
				return PL_domain_error("non-negative_frequency", value_term);
			}
			hpf->cutoff_frequency = cutoff;

			get_engine_format_info(NULL, &channels, &sample_rate);
			ma_hpf_config config = ma_hpf_config_init(
					ma_format_f32,
					channels,
					sample_rate,
					cutoff,
					hpf->order
					);
			ma_hpf_node_reinit(&config, &hpf->node);
		} else if (strcmp(param_name, "order") == 0) {
			int order_int;
			ma_uint32 channels, sample_rate;

			if (!PL_get_integer(value_term, &order_int)) {
				return PL_type_error("integer", value_term);
			}
			if (order_int < 0) {
				return PL_domain_error("non_negative_integer", value_term);
			}
			hpf->order = (ma_uint32)order_int;

			get_engine_format_info(NULL, &channels, &sample_rate);
			ma_hpf_config config = ma_hpf_config_init(
					ma_format_f32,
					channels,
					sample_rate,
					hpf->cutoff_frequency,
					hpf->order
					);
			ma_hpf_node_reinit(&config, &hpf->node);
		} else {
			return PL_domain_error("hpf_parameter", key_term);
		}
	}

	if (!PL_get_nil(list)) {
		return PL_type_error("list", params_list);
	}

	return TRUE;
}


/******************************************************************************
 * BAND-PASS FILTER EFFECT
 *****************************************************************************/

/*
 * init_bpf_node()
 * Initialize band pass filter node
 */
static ma_result init_bpf_node(bpf_node_t* bpf, double cutoff, ma_uint32 order)
{
	ma_bpf_node_config bpf_config;
	ma_uint32 channels;
	ma_uint32 sample_rate;
	ma_result result;

	get_engine_format_info(NULL, &channels, &sample_rate);

	bpf_config = ma_bpf_node_config_init(channels, sample_rate, cutoff, order);
	result = ma_bpf_node_init(ma_engine_get_node_graph(g_engine), &bpf_config, NULL, &bpf->node);

	if (result == MA_SUCCESS) {
		bpf->cutoff_frequency = cutoff;
		bpf->order = order;
	}

	return result;
}

/*
 * attach_bpf_effect()
 * Validate parameters and attach BPF to sound
 */
static ma_result attach_bpf_effect(term_t params, sound_slot_t* sound_slot, ma_node_base** out_effect_node)
{
	double cutoff;
	int order_int;
	bpf_node_t* bpf;
	ma_result result;

	if (!get_param_double(params, "cutoff", &cutoff)) {
		PL_existence_error("parameter", PL_new_atom("cutoff"));
		return MA_ERROR;
	}

	if (cutoff < 0.0) {
		PL_domain_error("non-negative_frequency", params);
		return MA_ERROR;
	}

	if (!get_param_int(params, "order", &order_int)) {
		order_int = 2;  /* Default to 2nd order */
	}

	if (order_int < 0) {
		PL_domain_error("non_negative_integer", params);
		return MA_ERROR;
	}

	bpf = (bpf_node_t*)malloc(sizeof(bpf_node_t));
	if (!bpf) {
		return MA_OUT_OF_MEMORY;
	}

	result = init_bpf_node(bpf, cutoff, (ma_uint32)order_int);
	if (result != MA_SUCCESS) {
		free(bpf);
		return result;
	}

	result = attach_effect_node_to_sound(sound_slot, &bpf->node.baseNode, EFFECT_BPF);
	if (result != MA_SUCCESS) {
		ma_bpf_node_uninit(&bpf->node, NULL);
		free(bpf);
		return result;
	}

	*out_effect_node = &bpf->node.baseNode;
	return MA_SUCCESS;
}

/*
 * query_bpf_params()
 * Build parameter list for bpf effect
 */
static int query_bpf_params(bpf_node_t* bpf, term_t params_list)
{
	term_t param_args = PL_new_term_refs(2);
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);
	term_t param_term = PL_new_term_ref();

	PL_put_atom_chars(param_args+0, "order");
	if (!PL_put_integer(param_args+1, bpf->order)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "cutoff");
	if (!PL_put_float(param_args+1, bpf->cutoff_frequency)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	return TRUE;
}

/*
 * set_bpf_parameters()
 * Set parameters for bpf effect from Prolog term list
 */
static foreign_t set_bpf_parameters(bpf_node_t* bpf, term_t params_list)
{
	term_t list = PL_copy_term_ref(params_list);
	term_t head = PL_new_term_ref();
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);

	while (PL_get_list(list, head, list)) {
		term_t key_term = PL_new_term_ref();
		term_t value_term = PL_new_term_ref();
		char* param_name;

		if (!PL_is_functor(head, eq_functor)) {
			return PL_type_error("key_value_pair", head);
		}
		if (!PL_get_arg(1, head, key_term)) {
			return FALSE;
		}
		if (!PL_get_arg(2, head, value_term)) {
			return FALSE;
		}
		if (!PL_get_atom_chars(key_term, &param_name)) {
			return PL_type_error("atom", key_term);
		}

		if (strcmp(param_name, "cutoff") == 0) {
			double cutoff;
			ma_uint32 channels, sample_rate;

			if (!PL_get_float(value_term, &cutoff)) {
				return PL_type_error("float", value_term);
			}
			if (cutoff < 0.0) {
				return PL_domain_error("non-negative_frequency", value_term);
			}
			bpf->cutoff_frequency = cutoff;

			get_engine_format_info(NULL, &channels, &sample_rate);
			ma_bpf_config config = ma_bpf_config_init(
					ma_format_f32,
					channels,
					sample_rate,
					cutoff,
					bpf->order
					);
			ma_bpf_node_reinit(&config, &bpf->node);
		} else if (strcmp(param_name, "order") == 0) {
			int order_int;
			ma_uint32 channels, sample_rate;

			if (!PL_get_integer(value_term, &order_int)) {
				return PL_type_error("integer", value_term);
			}
			if (order_int < 0) {
				return PL_domain_error("non_negative_integer", value_term);
			}
			bpf->order = (ma_uint32)order_int;

			get_engine_format_info(NULL, &channels, &sample_rate);
			ma_bpf_config config = ma_bpf_config_init(
					ma_format_f32,
					channels,
					sample_rate,
					bpf->cutoff_frequency,
					bpf->order
					);
			ma_bpf_node_reinit(&config, &bpf->node);
		} else {
			return PL_domain_error("bpf_parameter", key_term);
		}
	}

	if (!PL_get_nil(list)) {
		return PL_type_error("list", params_list);
	}

	return TRUE;
}


/******************************************************************************
 * DELAY EFFECT
 *****************************************************************************/

/*
 * init_delay_node()
 * Initialize delay node
 */
static ma_result init_delay_node(delay_node_t* delay, ma_uint32 delay_in_frames, float decay, float wet, float dry)
{
	ma_delay_node_config delay_config;
	ma_uint32 channels;
	ma_uint32 sample_rate;
	ma_result result;

	get_engine_format_info(NULL, &channels, &sample_rate);

	delay_config = ma_delay_node_config_init(channels, sample_rate, delay_in_frames, decay);
	delay_config.delay.wet = wet;
	delay_config.delay.dry = dry;

	result = ma_delay_node_init(ma_engine_get_node_graph(g_engine), &delay_config, NULL, &delay->node);

	if (result == MA_SUCCESS) {
		delay->delay_in_frames = delay_in_frames;
		delay->wet = wet;
		delay->dry = dry;
		delay->decay = decay;
	}

	return result;
}

/*
 * attach_delay_effect()
 * Validate parameters and attach delay to sound
 */
static ma_result attach_delay_effect(term_t params, sound_slot_t* sound_slot, ma_node_base** out_effect_node)
{
	int delay_in_frames_int;
	float decay, wet, dry;
	delay_node_t* delay;
	ma_result result;

	if (!get_param_int(params, "delay_in_frames", &delay_in_frames_int)) {
		PL_existence_error("parameter", PL_new_atom("delay_in_frames"));
		return MA_ERROR;
	}

	if (delay_in_frames_int < 0) {
		PL_domain_error("non_negative_integer", params);
		return MA_ERROR;
	}

	if (!get_param_float(params, "decay", &decay)) {
		decay = 0.0f;
	}

	if (decay < 0.0f || decay > 1.0f) {
		PL_domain_error("decay_range_0_to_1", params);
		return MA_ERROR;
	}

	if (!get_param_float(params, "wet", &wet)) {
		wet = 1.0f;
	}

	if (wet < 0.0f || wet > 1.0f) {
		PL_domain_error("wet_range_0_to_1", params);
		return MA_ERROR;
	}

	if (!get_param_float(params, "dry", &dry)) {
		dry = 1.0f;
	}

	if (dry < 0.0f || dry > 1.0f) {
		PL_domain_error("dry_range_0_to_1", params);
		return MA_ERROR;
	}

	delay = (delay_node_t*)malloc(sizeof(delay_node_t));
	if (!delay) {
		return MA_OUT_OF_MEMORY;
	}

	result = init_delay_node(delay, (ma_uint32)delay_in_frames_int, decay, wet, dry);
	if (result != MA_SUCCESS) {
		free(delay);
		return result;
	}

	result = attach_effect_node_to_sound(sound_slot, &delay->node.baseNode, EFFECT_DELAY);
	if (result != MA_SUCCESS) {
		ma_delay_node_uninit(&delay->node, NULL);
		free(delay);
		return result;
	}

	*out_effect_node = &delay->node.baseNode;
	return MA_SUCCESS;
}

/*
 * query_delay_params()
 * Build parameter list for delay effect
 */
static int query_delay_params(delay_node_t* delay, term_t params_list)
{
	term_t param_args = PL_new_term_refs(2);
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);
	term_t param_term = PL_new_term_ref();

	PL_put_atom_chars(param_args+0, "dry");
	if (!PL_put_float(param_args+1, delay->dry)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "wet");
	if (!PL_put_float(param_args+1, delay->wet)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "decay");
	if (!PL_put_float(param_args+1, delay->decay)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "delay_in_frames");
	if (!PL_put_integer(param_args+1, delay->delay_in_frames)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	return TRUE;
}

/*
 * set_delay_parameters()
 * Set parameters for delay effect from Prolog term list
 * Note: delay_in_frames cannot be changed after init
 */
static foreign_t set_delay_parameters(delay_node_t* delay, term_t params_list)
{
	term_t list = PL_copy_term_ref(params_list);
	term_t head = PL_new_term_ref();
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);

	while (PL_get_list(list, head, list)) {
		term_t key_term = PL_new_term_ref();
		term_t value_term = PL_new_term_ref();
		char* param_name;

		if (!PL_is_functor(head, eq_functor)) {
			return PL_type_error("key_value_pair", head);
		}
		if (!PL_get_arg(1, head, key_term)) {
			return FALSE;
		}
		if (!PL_get_arg(2, head, value_term)) {
			return FALSE;
		}
		if (!PL_get_atom_chars(key_term, &param_name)) {
			return PL_type_error("atom", key_term);
		}

		if (strcmp(param_name, "wet") == 0) {
			double value;
			if (!PL_get_float(value_term, &value)) {
				return PL_type_error("float", value_term);
			}
			if (value < 0.0 || value > 1.0) {
				return PL_domain_error("wet_range_0_to_1", value_term);
			}
			delay->wet = (float)value;
			ma_delay_node_set_wet(&delay->node, (float)value);
		} else if (strcmp(param_name, "dry") == 0) {
			double value;
			if (!PL_get_float(value_term, &value)) {
				return PL_type_error("float", value_term);
			}
			if (value < 0.0 || value > 1.0) {
				return PL_domain_error("dry_range_0_to_1", value_term);
			}
			delay->dry = (float)value;
			ma_delay_node_set_dry(&delay->node, (float)value);
		} else if (strcmp(param_name, "decay") == 0) {
			double value;
			if (!PL_get_float(value_term, &value)) {
				return PL_type_error("float", value_term);
			}
			if (value < 0.0 || value > 1.0) {
				return PL_domain_error("decay_range_0_to_1", value_term);
			}
			delay->decay = (float)value;
			ma_delay_node_set_decay(&delay->node, (float)value);
		} else if (strcmp(param_name, "delay_in_frames") == 0) {
			return PL_permission_error("modify", "delay_in_frames", value_term);
		} else {
			return PL_domain_error("delay_parameter", key_term);
		}
	}

	if (!PL_get_nil(list)) {
		return PL_type_error("list", params_list);
	}

	return TRUE;
}

/******************************************************************************
 * PING-PONG DELAY EFFECT
 *****************************************************************************/

/*
 * ping_pong_process_pcm_frames()
 * Process audio through ping-pong delay effect
 */
static void ping_pong_process_pcm_frames(
		ma_node* node,
		const float** frames_in,
		ma_uint32* frame_count_in,
		float** frames_out,
		ma_uint32* frame_count_out)
{
	ping_pong_delay_node_t* pp;
	ma_uint32 frame_count;
	const float* input;
	float* output;
	ma_uint32 frame;
	ma_uint32 read_pos;
	ma_uint32 old_read_pos;
	float mono_in;
	float delay_l;
	float delay_r;
	float blend;

	pp = (ping_pong_delay_node_t*)node;
	frame_count = *frame_count_out;
	input = frames_in[0];
	output = frames_out[0];

	for (frame = 0; frame < frame_count; frame++) {
		/* handle delay smoothing (pitch-shift mode) */
		if (pp->smoothing_mode == 1) {
			float target = (float)pp->target_delay_in_frames;
			if (pp->smooth_delay_position < target) {
				pp->smooth_delay_position += pp->smoothing_speed;
				if (pp->smooth_delay_position > target)
					pp->smooth_delay_position = target;
			} else if (pp->smooth_delay_position > target) {
				pp->smooth_delay_position -= pp->smoothing_speed;
				if (pp->smooth_delay_position < target)
					pp->smooth_delay_position = target;
			}
			pp->delay_in_frames = (ma_uint32)pp->smooth_delay_position;
		}

		/* calculate read position */
		read_pos = pp->cursor + pp->buffer_size - pp->delay_in_frames;
		if (read_pos >= pp->buffer_size) {
			read_pos -= pp->buffer_size;
		}

		/* sum input to mono */
		mono_in = (input[frame * 2 + 0] + input[frame * 2 + 1]) * 0.5f;

		/* read from delay buffers */
		if (pp->smoothing_mode == 2 && pp->crossfade_counter > 0) {
			/* crossfade mode: blend old and new positions */
			old_read_pos = pp->cursor + pp->buffer_size - pp->old_delay_in_frames;
			if (old_read_pos >= pp->buffer_size) {
				old_read_pos -= pp->buffer_size;
			}

			blend = (float)pp->crossfade_counter / (float)pp->crossfade_length;
			delay_l = pp->buffer_l[old_read_pos] * blend + pp->buffer_l[read_pos] * (1.0f - blend);
			delay_r = pp->buffer_r[old_read_pos] * blend + pp->buffer_r[read_pos] * (1.0f - blend);

			pp->crossfade_counter--;
		} else {
			delay_l = pp->buffer_l[read_pos];
			delay_r = pp->buffer_r[read_pos];
		}

		/* write to delay buffers with cross-feedback */
		pp->buffer_l[pp->cursor] = mono_in + (delay_r * pp->feedback);
		pp->buffer_r[pp->cursor] = delay_l;

		/* advance cursor */
		pp->cursor++;
		if (pp->cursor >= pp->buffer_size) {
			pp->cursor = 0;
		}

		/* output: dry original + wet delayed */
		output[frame * 2 + 0] = (input[frame * 2 + 0] * pp->dry) + (delay_l * pp->wet);
		output[frame * 2 + 1] = (input[frame * 2 + 1] * pp->dry) + (delay_r * pp->wet);
	}

	*frame_count_in = frame_count;
	*frame_count_out = frame_count;
}

static ma_node_vtable ping_pong_vtable = {
	ping_pong_process_pcm_frames,
	NULL,
	1,
	1,
	0
};

static ma_result init_ping_pong_delay_node(
		ping_pong_delay_node_t* node,
		ma_uint32 max_delay_in_frames,
		ma_uint32 delay_in_frames,
		float feedback,
		float wet,
		float dry)
{
	ma_result result;

	result = init_effect_node_base(&node->base, &ping_pong_vtable);
	if (result != MA_SUCCESS) {
		return result;
	}

	node->buffer_size = max_delay_in_frames;
	node->buffer_l = (float *)malloc(max_delay_in_frames *sizeof(float));
	node->buffer_r = (float *)malloc(max_delay_in_frames *sizeof(float));

	if (node->buffer_l == NULL || node->buffer_r == NULL) {
		if (node->buffer_l) free(node->buffer_l);
		if (node->buffer_r) free(node->buffer_r);
		ma_node_uninit(&node->base, NULL);
		return MA_OUT_OF_MEMORY;
	}

	memset(node->buffer_l, 0, max_delay_in_frames * sizeof(float));
	memset(node->buffer_r, 0, max_delay_in_frames * sizeof(float));

	node->cursor = 0;
	node->delay_in_frames = delay_in_frames;
	node->feedback = feedback;
	node->wet = wet;
	node->dry = dry;

	/* smoothing/crossfade defaults */
	node->smoothing_mode = 1;
	node->target_delay_in_frames = delay_in_frames;
	node->old_delay_in_frames = delay_in_frames;
	node->crossfade_counter = 0;
	node->crossfade_length = 128;
	node->smoothing_speed = 1.0f;
	node->smooth_delay_position = (float)delay_in_frames;

	return MA_SUCCESS;
}

/*
 * attach_ping_pong_delay_effect()
 * Validate parameters and attach ping-pong delay to sound
 */
static ma_result attach_ping_pong_delay_effect(term_t params, sound_slot_t* sound_slot, ma_node_base** out_effect_node)
{
	int max_delay_int, delay_int;
	int smoothing_mode_int, crossfade_length_int;
	float feedback, wet, dry, smoothing_speed_float;
	ping_pong_delay_node_t* pp;
	ma_result result;

	if (!get_param_int(params, "max_delay_in_frames", &max_delay_int)) {
		PL_existence_error("parameter", PL_new_atom("max_delay_in_frames"));
		return MA_ERROR;
	}

	if (max_delay_int < 1) {
		PL_domain_error("positive_integer", params);
		return MA_ERROR;
	}

	if (!get_param_int(params, "delay_in_frames", &delay_int)) {
		delay_int = max_delay_int;  /* default to max */
	}

	if (delay_int < 0 || delay_int > max_delay_int) {
		PL_domain_error("delay_in_range", params);
		return MA_ERROR;
	}

	if (!get_param_float(params, "feedback", &feedback)) {
		feedback = 0.5f;
	}

	if (feedback < 0.0f || feedback > 1.0f) {
		PL_domain_error("feedback_range_0_to_1", params);
		return MA_ERROR;
	}

	if (!get_param_float(params, "wet", &wet)) {
		wet = 1.0f;
	}

	if (wet < 0.0f || wet > 1.0f) {
		PL_domain_error("wet_range_0_to_1", params);
		return MA_ERROR;
	}

	if (!get_param_float(params, "dry", &dry)) {
		dry = 1.0f;
	}

	if (dry < 0.0f || dry > 1.0f) {
		PL_domain_error("dry_range_0_to_1", params);
		return MA_ERROR;
	}

	pp = (ping_pong_delay_node_t*)malloc(sizeof(ping_pong_delay_node_t));
	if (!pp) {
		return MA_OUT_OF_MEMORY;
	}

	result = init_ping_pong_delay_node(pp, (ma_uint32)max_delay_int, (ma_uint32)delay_int, feedback, wet, dry);
	if (result != MA_SUCCESS) {
		free(pp);
		return result;
	}

	/* optional smoothing parameters (override defaults from init) */
	if (get_param_int(params, "smoothing_mode", &smoothing_mode_int)) {
		if (smoothing_mode_int >= 0 && smoothing_mode_int <= 2) {
			pp->smoothing_mode = (ma_uint32)smoothing_mode_int;
		}
	}
	if (get_param_float(params, "smoothing_speed", &smoothing_speed_float)) {
		if (smoothing_speed_float > 0.0f) {
			pp->smoothing_speed = smoothing_speed_float;
		}
	}
	if (get_param_int(params, "crossfade_length", &crossfade_length_int)) {
		if (crossfade_length_int >= 1) {
			pp->crossfade_length = (ma_uint32)crossfade_length_int;
		}
	}

	result = attach_effect_node_to_sound(sound_slot, &pp->base, EFFECT_PING_PONG_DELAY);
	if (result != MA_SUCCESS) {
		free(pp->buffer_l);
		free(pp->buffer_r);
		ma_node_uninit(&pp->base, NULL);
		free(pp);
		return result;
	}

	*out_effect_node = &pp->base;
	return MA_SUCCESS;
}

/*
 * query_ping_pong_delay_params()
 * Build parameter list for ping-pong delay effect
 */
static int query_ping_pong_delay_params(ping_pong_delay_node_t* pp, term_t params_list)
{
	term_t param_args = PL_new_term_refs(2);
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);
	term_t param_term = PL_new_term_ref();

	PL_put_atom_chars(param_args+0, "dry");
	if (!PL_put_float(param_args+1, pp->dry)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "wet");
	if (!PL_put_float(param_args+1, pp->wet)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "feedback");
	if (!PL_put_float(param_args+1, pp->feedback)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "delay_in_frames");
	if (!PL_put_integer(param_args+1, pp->delay_in_frames)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "max_delay_in_frames");
	if (!PL_put_integer(param_args+1, pp->buffer_size)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "smoothing_mode");
	if (!PL_put_integer(param_args+1, pp->smoothing_mode)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "smoothing_speed");
	if (!PL_put_float(param_args+1, pp->smoothing_speed)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "crossfade_length");
	if (!PL_put_integer(param_args+1, pp->crossfade_length)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	return TRUE;
}

/*
 * set_ping_pong_delay_parameters()
 * Set parameters for ping-pong delay effect from Prolog term list
 */
static foreign_t set_ping_pong_delay_parameters(ping_pong_delay_node_t* pp, term_t params_list)
{
	term_t list = PL_copy_term_ref(params_list);
	term_t head = PL_new_term_ref();
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);

	while (PL_get_list(list, head, list)) {
		term_t key_term = PL_new_term_ref();
		term_t value_term = PL_new_term_ref();
		char* param_name;

		if (!PL_is_functor(head, eq_functor)) {
			return PL_type_error("key_value_pair", head);
		}
		if (!PL_get_arg(1, head, key_term)) {
			return FALSE;
		}
		if (!PL_get_arg(2, head, value_term)) {
			return FALSE;
		}
		if (!PL_get_atom_chars(key_term, &param_name)) {
			return PL_type_error("atom", key_term);
		}

		if (strcmp(param_name, "wet") == 0) {
			double value;
			if (!PL_get_float(value_term, &value)) {
				return PL_type_error("float", value_term);
			}
			if (value < 0.0 || value > 1.0) {
				return PL_domain_error("wet_range_0_to_1", value_term);
			}
			pp->wet = (float)value;
		} else if (strcmp(param_name, "dry") == 0) {
			double value;
			if (!PL_get_float(value_term, &value)) {
				return PL_type_error("float", value_term);
			}
			if (value < 0.0 || value > 1.0) {
				return PL_domain_error("dry_range_0_to_1", value_term);
			}
			pp->dry = (float)value;
		} else if (strcmp(param_name, "feedback") == 0) {
			double value;
			if (!PL_get_float(value_term, &value)) {
				return PL_type_error("float", value_term);
			}
			if (value < 0.0 || value > 1.0) {
				return PL_domain_error("feedback_range_0_to_1", value_term);
			}
			pp->feedback = (float)value;
		} else if (strcmp(param_name, "delay_in_frames") == 0) {
			int value;
			if (!PL_get_integer(value_term, &value)) {
				return PL_type_error("integer", value_term);
			}
			if (value < 0 || (ma_uint32)value > pp->buffer_size) {
				return PL_domain_error("delay_in_range", value_term);
			}
			if (pp->smoothing_mode == 1) {
				/* pitch-shift: set target */
				pp->target_delay_in_frames = (ma_uint32)value;
			} else if (pp->smoothing_mode == 2) {
				/* crossfade: trigger blend */
				pp->old_delay_in_frames = pp->delay_in_frames;
				pp->delay_in_frames = (ma_uint32)value;
				pp->crossfade_counter = pp->crossfade_length;
			} else {
				/* direct (clicks) */
				pp->delay_in_frames = (ma_uint32)value;
			}
		} else if (strcmp(param_name, "max_delay_in_frames") == 0) {
			return PL_permission_error("modify", "max_delay_in_frames", value_term);
		} else if (strcmp(param_name, "smoothing_mode") == 0) {
			int value;
			if (!PL_get_integer(value_term, &value)) {
				return PL_type_error("integer", value_term);
			}
			if (value < 0 || value > 2) {
				return PL_domain_error("smoothing_mode_0_to_2", value_term);
			}
			pp->smoothing_mode = (ma_uint32)value;
		} else if (strcmp(param_name, "smoothing_speed") == 0) {
			double value;
			if (!PL_get_float(value_term, &value)) {
				return PL_type_error("float", value_term);
			}
			if (value <= 0.0) {
				return PL_domain_error("positive_number", value_term);
			}
			pp->smoothing_speed = (float)value;
		} else if (strcmp(param_name, "crossfade_length") == 0) {
			int value;
			if (!PL_get_integer(value_term, &value)) {
				return PL_type_error("integer", value_term);
			}
			if (value < 1) {
				return PL_domain_error("positive_integer", value_term);
			}
			pp->crossfade_length = (ma_uint32)value;
		} else {
			return PL_domain_error("ping_pong_delay_parameter", key_term);
		}
	}

	if (!PL_get_nil(list)) {
		return PL_type_error("list", params_list);
	}

	return TRUE;
}


/******************************************************************************
 * PROLOG INTERFACE
 *****************************************************************************/

/*
 * pl_sampler_sound_attach_effect()
 * Attach an effect to a sound's processing chain
 */
static foreign_t pl_sampler_sound_attach_effect(term_t sound_handle, term_t effect_type, term_t params, term_t effect_handle)
{
	ma_sound* sound;
	int slot;
	char* type_str;
	ma_result result;
	ma_node_base* effect_node;
	term_t effect_term;
	functor_t effect_functor;

	GET_SOUND_WITH_SLOT(sound_handle, sound, slot);

	if (!PL_get_atom_chars(effect_type, &type_str)) {
		return PL_type_error("atom", effect_type);
	}

	if (strcmp(type_str, "bitcrush") == 0) {
		result = attach_bitcrush_effect(params, &g_sounds[slot], &effect_node);
	} else if (strcmp(type_str, "envelope") == 0) {
		result = attach_adbr_envelope(params, &g_sounds[slot], &effect_node);
	} else if (strcmp(type_str, "lpf") == 0) {
		result = attach_lpf_effect(params, &g_sounds[slot], &effect_node);
	} else if (strcmp(type_str, "hpf") == 0) {
		result = attach_hpf_effect(params, &g_sounds[slot], &effect_node);
	} else if (strcmp(type_str, "bpf") == 0) {
		result = attach_bpf_effect(params, &g_sounds[slot], &effect_node);
	} else if (strcmp(type_str, "delay") == 0) {
		result = attach_delay_effect(params, &g_sounds[slot], &effect_node);
	} else if (strcmp(type_str, "ping_pong_delay") == 0) {
		result = attach_ping_pong_delay_effect(params, &g_sounds[slot], &effect_node);
	} else {
		return PL_domain_error("effect_type", effect_type);
	}

	if (result != MA_SUCCESS) {
		return FALSE;
	}

	/* Build effect(SoundHandle, EffectPointer) term */
	effect_term = PL_new_term_ref();
	effect_functor = PL_new_functor(PL_new_atom("effect"), 2);

	if (!PL_unify_functor(effect_term, effect_functor)) {
		return FALSE;
	}

	term_t arg = PL_new_term_ref();
	if (!PL_get_arg(1, effect_term, arg) || !PL_unify(arg, sound_handle)) {
		return FALSE;
	}

	if (!PL_get_arg(2, effect_term, arg) || !PL_unify_pointer(arg, effect_node)) {
		return FALSE;
	}

	return PL_unify(effect_handle, effect_term);
}

/*
 * pl_sampler_sound_effects()
 * Query all effects attached to a sound
 */
static foreign_t pl_sampler_sound_effects(term_t sound_handle, term_t effects_list)
{
	ma_sound* sound;
	int slot;
	effect_node_t* effect;
	int count = 0;

	GET_SOUND_WITH_SLOT(sound_handle, sound, slot);

	for (effect = g_sounds[slot].effect_chain; effect != NULL; effect = effect->next) {
		count++;
	}

	term_t list = PL_new_term_ref();
	PL_put_nil(list);

	if (count > 0) {
		effect_node_t** effects_array = malloc(count * sizeof(effect_node_t*));
		int i = 0;
		for (effect = g_sounds[slot].effect_chain; effect != NULL; effect = effect->next) {
			effects_array[i++] = effect;
		}

		term_t effect_term = PL_new_term_ref();
		term_t args = PL_new_term_refs(3);
		functor_t effect_functor = PL_new_functor(PL_new_atom("effect"), 3);

		for (i = count - 1; i >= 0; i--) {
			effect = effects_array[i];

			const char* type_str;
			term_t params_list = PL_new_term_ref();
			PL_put_nil(params_list);
			int query_result = TRUE;

			switch (effect->type) {
				case EFFECT_BITCRUSH:
					type_str = "bitcrush";
					query_result = query_bitcrush_params((bitcrush_node_t*)effect->effect_node, params_list);
					break;
				case EFFECT_ENVELOPE:
					type_str = "envelope";
					query_result = query_envelope_params((adbr_envelope_node_t*)effect->effect_node, params_list);
					break;
				case EFFECT_LPF:
					type_str = "lpf";
					query_result = query_lpf_params((lpf_node_t*)effect->effect_node, params_list);
					break;
				case EFFECT_HPF:
					type_str = "hpf";
					query_result = query_hpf_params((hpf_node_t*)effect->effect_node, params_list);
					break;
				case EFFECT_BPF:
					type_str = "bpf";
					query_result = query_bpf_params((bpf_node_t*)effect->effect_node, params_list);
					break;
				case EFFECT_DELAY:
					type_str = "delay";
					query_result = query_delay_params((delay_node_t*)effect->effect_node, params_list);
					break;
				case EFFECT_PING_PONG_DELAY:
					type_str = "ping_pong_delay";
					query_result = query_ping_pong_delay_params((ping_pong_delay_node_t*)effect->effect_node, params_list);
					break;
				default:
					type_str = "unknown";
					break;
			}

			if (!query_result) {
				free(effects_array);
				return FALSE;
			}

			PL_put_atom_chars(args+0, type_str);
			if (!PL_put_pointer(args+1, effect->effect_node)) {
				free(effects_array);
				return FALSE;
			}
			if (!PL_put_term(args+2, params_list)) {
				free(effects_array);
				return FALSE;
			}

			if (!PL_cons_functor_v(effect_term, effect_functor, args)) {
				free(effects_array);
				return FALSE;
			}

			if (!PL_cons_list(list, effect_term, list)) {
				free(effects_array);
				return FALSE;
			}
		}

		free(effects_array);
	}

	return PL_unify(effects_list, list);
}

/*
 * pl_sampler_effect_set_parameters()
 * Set parameters on an effect
 */
static foreign_t pl_sampler_effect_set_parameters(term_t effect_handle, term_t params_list)
{
	term_t sound_handle_term = PL_new_term_ref();
	term_t effect_ptr_term = PL_new_term_ref();
	int sound_slot;
	void* effect_ptr;

	GET_EFFECT_FROM_HANDLE(effect_handle, sound_slot, effect_ptr);

	/* Find the effect in the chain */
	effect_node_t* node = g_sounds[sound_slot].effect_chain;
	while (node) {
		if (node->effect_node == effect_ptr) {
			break;
		}
		node = node->next;
	}

	if (!node) {
		return PL_existence_error("effect", effect_handle);
	}

	/* Dispatch to type-specific set parameter function */
	switch (node->type) {
		case EFFECT_BITCRUSH:
			return set_bitcrush_parameters((bitcrush_node_t*)node->effect_node, params_list);
		case EFFECT_ENVELOPE:
			return set_envelope_parameters((adbr_envelope_node_t*)node->effect_node, params_list);
		case EFFECT_LPF:
			return set_lpf_parameters((lpf_node_t*)node->effect_node, params_list);
		case EFFECT_HPF:
			return set_hpf_parameters((hpf_node_t*)node->effect_node, params_list);
		case EFFECT_BPF:
			return set_bpf_parameters((bpf_node_t*)node->effect_node, params_list);
		case EFFECT_DELAY:
			return set_delay_parameters((delay_node_t*)node->effect_node, params_list);
		case EFFECT_PING_PONG_DELAY:
			return set_ping_pong_delay_parameters((ping_pong_delay_node_t*)node->effect_node, params_list);
		default:
			return PL_domain_error("effect_type", effect_handle);
	}
}

/*
 * pl_sampler_effect_detach()
 * Remove an effect from the sound's effect chain
 */
static foreign_t pl_sampler_effect_detach(term_t effect_handle)
{
  	int sound_slot;
  	void* effect_ptr;

	GET_EFFECT_FROM_HANDLE(effect_handle, sound_slot, effect_ptr);

	/* Find and remove the effect from the chain */
	effect_node_t* node = g_sounds[sound_slot].effect_chain;
	effect_node_t* prev = NULL;

	while (node) {
		if (node->effect_node == effect_ptr) {
			ma_node_detach_output_bus(node->effect_node, 0);

			if (node->type == EFFECT_BITCRUSH) {
				bitcrush_node_t* bitcrush = (bitcrush_node_t*)node->effect_node;
				if (bitcrush->hold_samples) {
					free(bitcrush->hold_samples);
				}
			}
			ma_node_uninit(node->effect_node, NULL);
			free(node->effect_node);

			if (prev) {
				prev->next = node->next;
			} else {
				g_sounds[sound_slot].effect_chain = node->next;
			}

			free(node);

			/* reconnect the chain: if there are remaining effects, reconnect them */
			effect_node_t* first_effect = g_sounds[sound_slot].effect_chain;
			if (first_effect) {
				/* reconnect sound -> first_effect -> endpoint */
				ma_node_attach_output_bus(g_sounds[sound_slot].sound, 0, first_effect-> effect_node, 0);

				effect_node_t* current = first_effect;
				while (current->next) {
					ma_node_attach_output_bus(current->effect_node, 0, current->next->effect_node, 0);
					current = current->next;
				}
				ma_node_attach_output_bus(current->effect_node, 0, ma_engine_get_endpoint(g_engine), 0);
			} else {
				ma_node_attach_output_bus(g_sounds[sound_slot].sound, 0, ma_engine_get_endpoint(g_engine), 0);
			}

			return TRUE;
		}
		prev = node;
		node = node->next;
	}

	return PL_existence_error("effect", effect_handle);
}

/*
 * effects_register_predicates()
 * Register effects foreign predicates with SWI-Prolog
 */
install_t effects_register_predicates(void)
{
	PL_register_foreign("sampler_sound_attach_effect", 4, pl_sampler_sound_attach_effect, 0);
	PL_register_foreign("sampler_sound_effects", 2, pl_sampler_sound_effects, 0);
	PL_register_foreign("sampler_effect_set_parameters", 2, pl_sampler_effect_set_parameters, 0);
	PL_register_foreign("sampler_effect_detach", 1, pl_sampler_effect_detach, 0);
}

/*
 * uninstall_effects()
 * Called when the foreign library is unloaded.
 * Effects have no global state - they are owned by sounds and cleaned up via sampler.
 */
install_t uninstall_effects(void)
{
	/* No-op: effects are cleaned up when their owning sounds are freed */
}
