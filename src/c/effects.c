/*
 * effects.c - Audio effects for granular sampler
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */


#if defined(__ARM_NEON) || defined(__aarch64__)
#include <arm_neon.h>
#endif


#include "promini.h"


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

/*
 * get_effect_chain_tail()
 * Returns the last effect node in a chain, or NULL if chain is empty.
 */
ma_node* get_effect_chain_tail(effect_node_t* chain)
{
	if (chain == NULL) return NULL;
	while (chain->next != NULL) {
		chain = chain->next;
	}
	return (ma_node*)chain->effect_node;
}

/*
 * attach_effect_node()
 * Attach an effect node to a source node's processing chain.
 * Works with any ma_node (sounds, sound groups, voices, etc.)
 */
ma_result attach_effect_node(
		ma_node* source_node,
		effect_node_t** effect_chain,
		ma_node_base* effect_node,
		effect_type_t type)
{
	effect_node_t* new_effect;
	effect_node_t* tail;
	ma_node* endpoint;
	ma_result result;

	new_effect = (effect_node_t*)malloc(sizeof(effect_node_t));
	if (new_effect == NULL) {
		return MA_OUT_OF_MEMORY;
	}

	new_effect->type = type;
	new_effect->effect_node = effect_node;
	new_effect->next = NULL;

	endpoint = ma_node_graph_get_endpoint(ma_engine_get_node_graph(g_engine));

	/* if no effects yet, attach source -> effect -> endpoint */
	if (*effect_chain == NULL) {
		result = ma_node_attach_output_bus(source_node, 0, (ma_node*)effect_node, 0);
		if (result != MA_SUCCESS) {
			free(new_effect);
			return result;
		}

		result = ma_node_attach_output_bus((ma_node*)effect_node, 0, endpoint, 0);
		if (result != MA_SUCCESS) {
			ma_node_detach_output_bus(source_node, 0);
			ma_node_attach_output_bus(source_node, 0, endpoint, 0);
			free(new_effect);
			return result;
		}

		*effect_chain = new_effect;
	} else {
		/* find tail and reconnect: source -> ... existing ... -> new_effect -> endpoint */
		tail = *effect_chain;
		while (tail->next != NULL) {
			tail = tail->next;
		}

		result = ma_node_attach_output_bus((ma_node*)tail->effect_node, 0, (ma_node*)effect_node, 0);
		if (result != MA_SUCCESS) {
			free(new_effect);
			return result;
		}

		result = ma_node_attach_output_bus((ma_node*)effect_node, 0, endpoint, 0);
		if (result != MA_SUCCESS) {
			ma_node_detach_output_bus((ma_node*)tail->effect_node, 0);
			ma_node_attach_output_bus((ma_node*)tail->effect_node, 0, endpoint, 0);
			free(new_effect);
			return result;
		}

		tail->next = new_effect;
	}

	return MA_SUCCESS;
}

/*
 * get_effect_info_from_handle()
 * Extract source node, effect chain, and effect pointer from handle.
 * Handle format: effect(sound(N), Ptr) or effect(voice(N), Ptr)
 */
static ma_bool32 get_effect_info_from_handle(term_t effect_handle, ma_sound** source_out, effect_node_t*** chain_out, void** ptr_out)
{
	term_t source_term = PL_new_term_ref();
	term_t ptr_term = PL_new_term_ref();
	term_t slot_term = PL_new_term_ref();
	functor_t f;
	int slot;

	if (!PL_is_functor(effect_handle, PL_new_functor(PL_new_atom("effect"), 2))) return MA_FALSE;
	if (!PL_get_arg(1, effect_handle, source_term)) return MA_FALSE;
	if (!PL_get_arg(2, effect_handle, ptr_term)) return MA_FALSE;
	if (!PL_get_pointer(ptr_term, ptr_out)) return MA_FALSE;
	if (!PL_get_functor(source_term, &f)) return MA_FALSE;
	if (!PL_get_arg(1, source_term, slot_term)) return MA_FALSE;
	if (!PL_get_integer(slot_term, &slot)) return MA_FALSE;

	if (f == PL_new_functor(PL_new_atom("sound"), 1)) {
		if (slot < 0 || slot >= MAX_SOUNDS || !g_sounds[slot].in_use) return MA_FALSE;
		*source_out = g_sounds[slot].sound;
		*chain_out = &g_sounds[slot].effect_chain;
		return MA_TRUE;
	}
	if (f == PL_new_functor(PL_new_atom("voice"), 1)) {
		if (slot < 0 || slot >= MAX_VOICES || !g_voices[slot].in_use) return MA_FALSE;
		*source_out = &g_voices[slot].group;
		*chain_out = &g_voices[slot].effect_chain;
		return MA_TRUE;
	}
	if (f == PL_new_functor(PL_new_atom("summing_node"), 1)) {
		if (slot < 0 || slot >= MAX_SUMMING_NODES || !g_summing_nodes[slot].in_use) return MA_FALSE;
		*source_out = (ma_sound*)&g_summing_nodes[slot].base;
		*chain_out = &g_summing_nodes[slot].effect_chain;
		return MA_TRUE;
	}
	return MA_FALSE;
}


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
 * Validate parameters and attach bitcrush effect to a node
 */
static ma_result attach_bitcrush_effect(term_t params, ma_node* source_node, effect_node_t** effect_chain, ma_node_base** out_effect_node)
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

	result = attach_effect_node(source_node, effect_chain, &bitcrush->base, EFFECT_BITCRUSH);
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
 * Validate parameters and attach LPF to a node
 */
static ma_result attach_lpf_effect(term_t params, ma_node* source_node, effect_node_t** effect_chain, ma_node_base** out_effect_node)
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

	result = attach_effect_node(source_node, effect_chain, &lpf->node.baseNode, EFFECT_LPF);
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
 * Validate parameters and attach HPF to a node
 */
static ma_result attach_hpf_effect(term_t params, ma_node* source_node, effect_node_t** effect_chain, ma_node_base** out_effect_node)
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

	result = attach_effect_node(source_node, effect_chain, &hpf->node.baseNode, EFFECT_HPF);
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
static ma_result attach_bpf_effect(term_t params, ma_node* source_node, effect_node_t** effect_chain, ma_node_base** out_effect_node)
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

	result = attach_effect_node(source_node, effect_chain, &bpf->node.baseNode, EFFECT_BPF);
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
static ma_result attach_delay_effect(term_t params, ma_node* source_node, effect_node_t** effect_chain, ma_node_base** out_effect_node)
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

	result = attach_effect_node(source_node, effect_chain, &delay->node.baseNode, EFFECT_DELAY);
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
static ma_result attach_ping_pong_delay_effect(term_t params, ma_node* source_node, effect_node_t** effect_chain, ma_node_base** out_effect_node)
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

	result = attach_effect_node(source_node, effect_chain, &pp->base, EFFECT_PING_PONG_DELAY);
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
 * REVERB EFFECT
 *****************************************************************************/

/*
 * attach_reverb_effect()
 * Parse parameters and attach reverb effect to sound.
 * All parameters are optional with sensible defaults.
 * Returns MA_SUCCESS or error code.
 */
static ma_result attach_reverb_effect(term_t params, ma_node* source_node, effect_node_t** effect_chain, ma_node_base** out_effect_node)
{
	reverb_node_t* reverb;
	ma_result result;
	ma_uint32 sample_rate;
	float value;

	reverb = (reverb_node_t*)malloc(sizeof(reverb_node_t));
	if (!reverb) {
		return MA_OUT_OF_MEMORY;
	}

	get_engine_format_info(NULL, NULL, &sample_rate);

	result = init_reverb_node(reverb, sample_rate);
	if (result != MA_SUCCESS) {
		free(reverb);
		return result;
	}

	result = init_effect_node_base(&reverb->base, &reverb_vtable);
	if (result != MA_SUCCESS) {
		free_reverb_node(reverb);
		free(reverb);
		return result;
	}

	/* Override defaults with any provided parameters */
	if (get_param_float(params, "predelay_ms", &value)) {
		reverb->predelay_ms = value;
	}
	if (get_param_float(params, "bandwidth", &value)) {
		reverb->bandwidth = value;
	}
	if (get_param_float(params, "decay", &value)) {
		reverb->decay = value;
	}
	if (get_param_float(params, "damping", &value)) {
		reverb->damping = value;
	}
	if (get_param_float(params, "mod_rate", &value)) {
		reverb->mod_rate = value;
	}
	if (get_param_float(params, "mod_depth", &value)) {
		reverb->mod_depth = value;
	}
	if (get_param_float(params, "shimmer1_shift", &value)) {
		reverb->shimmer1_shift = value;
	}
	if (get_param_float(params, "shimmer1_mix", &value)) {
		reverb->shimmer1_mix = value;
	}
	if (get_param_float(params, "shimmer2_shift", &value)) {
		reverb->shimmer2_shift = value;
	}
	if (get_param_float(params, "shimmer2_mix", &value)) {
		reverb->shimmer2_mix = value;
	}
	if (get_param_float(params, "width", &value)) {
		reverb->width = value;
	}
	if (get_param_float(params, "cross_feed", &value)) {
		reverb->cross_feed = value;
	}
	if (get_param_float(params, "low_cut", &value)) {
		reverb->low_cut = value;
	}
	if (get_param_float(params, "high_cut", &value)) {
		reverb->high_cut = value;
	}
	if (get_param_float(params, "wet", &value)) {
		reverb->wet = value;
	}
	if (get_param_float(params, "dry", &value)) {
		reverb->dry = value;
	}

	result = attach_effect_node(source_node, effect_chain, &reverb->base, EFFECT_REVERB);
	if (result != MA_SUCCESS) {
		free_reverb_node(reverb);
		ma_node_uninit(&reverb->base, NULL);
		free(reverb);
		return result;
	}

	*out_effect_node = &reverb->base;
	return MA_SUCCESS;
}

/*
 * query_reverb_params()
 * Build parameter list for reverb effect
 */
static int query_reverb_params(reverb_node_t* reverb, term_t params_list)
{
	term_t param_args = PL_new_term_refs(2);
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);
	term_t param_term = PL_new_term_ref();

	PL_put_atom_chars(param_args+0, "dry");
	if (!PL_put_float(param_args+1, reverb->dry)) return FALSE;
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) return FALSE;
	if (!PL_cons_list(params_list, param_term, params_list)) return FALSE;

	PL_put_atom_chars(param_args+0, "wet");
	if (!PL_put_float(param_args+1, reverb->wet)) return FALSE;
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) return FALSE;
	if (!PL_cons_list(params_list, param_term, params_list)) return FALSE;

	PL_put_atom_chars(param_args+0, "high_cut");
	if (!PL_put_float(param_args+1, reverb->high_cut)) return FALSE;
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) return FALSE;
	if (!PL_cons_list(params_list, param_term, params_list)) return FALSE;

	PL_put_atom_chars(param_args+0, "low_cut");
	if (!PL_put_float(param_args+1, reverb->low_cut)) return FALSE;
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) return FALSE;
	if (!PL_cons_list(params_list, param_term, params_list)) return FALSE;

	PL_put_atom_chars(param_args+0, "cross_feed");
	if (!PL_put_float(param_args+1, reverb->cross_feed)) return FALSE;
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) return FALSE;
	if (!PL_cons_list(params_list, param_term, params_list)) return FALSE;

	PL_put_atom_chars(param_args+0, "width");
	if (!PL_put_float(param_args+1, reverb->width)) return FALSE;
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) return FALSE;
	if (!PL_cons_list(params_list, param_term, params_list)) return FALSE;

	PL_put_atom_chars(param_args+0, "shimmer2_mix");
	if (!PL_put_float(param_args+1, reverb->shimmer2_mix)) return FALSE;
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) return FALSE;
	if (!PL_cons_list(params_list, param_term, params_list)) return FALSE;

	PL_put_atom_chars(param_args+0, "shimmer2_shift");
	if (!PL_put_float(param_args+1, reverb->shimmer2_shift)) return FALSE;
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) return FALSE;
	if (!PL_cons_list(params_list, param_term, params_list)) return FALSE;

	PL_put_atom_chars(param_args+0, "shimmer1_mix");
	if (!PL_put_float(param_args+1, reverb->shimmer1_mix)) return FALSE;
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) return FALSE;
	if (!PL_cons_list(params_list, param_term, params_list)) return FALSE;

	PL_put_atom_chars(param_args+0, "shimmer1_shift");
	if (!PL_put_float(param_args+1, reverb->shimmer1_shift)) return FALSE;
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) return FALSE;
	if (!PL_cons_list(params_list, param_term, params_list)) return FALSE;

	PL_put_atom_chars(param_args+0, "mod_depth");
	if (!PL_put_float(param_args+1, reverb->mod_depth)) return FALSE;
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) return FALSE;
	if (!PL_cons_list(params_list, param_term, params_list)) return FALSE;

	PL_put_atom_chars(param_args+0, "mod_rate");
	if (!PL_put_float(param_args+1, reverb->mod_rate)) return FALSE;
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) return FALSE;
	if (!PL_cons_list(params_list, param_term, params_list)) return FALSE;

	PL_put_atom_chars(param_args+0, "damping");
	if (!PL_put_float(param_args+1, reverb->damping)) return FALSE;
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) return FALSE;
	if (!PL_cons_list(params_list, param_term, params_list)) return FALSE;

	PL_put_atom_chars(param_args+0, "decay");
	if (!PL_put_float(param_args+1, reverb->decay)) return FALSE;
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) return FALSE;
	if (!PL_cons_list(params_list, param_term, params_list)) return FALSE;

	PL_put_atom_chars(param_args+0, "bandwidth");
	if (!PL_put_float(param_args+1, reverb->bandwidth)) return FALSE;
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) return FALSE;
	if (!PL_cons_list(params_list, param_term, params_list)) return FALSE;

	PL_put_atom_chars(param_args+0, "predelay_ms");
	if (!PL_put_float(param_args+1, reverb->predelay_ms)) return FALSE;
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) return FALSE;
	if (!PL_cons_list(params_list, param_term, params_list)) return FALSE;

	return TRUE;
}

/*
 * set_reverb_parameters()
 * Set parameters for reverb effect from Prolog term list
 */
static foreign_t set_reverb_parameters(reverb_node_t* reverb, term_t params_list)
{
	term_t list = PL_copy_term_ref(params_list);
	term_t head = PL_new_term_ref();
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);

	while (PL_get_list(list, head, list)) {
		term_t key_term = PL_new_term_ref();
		term_t value_term = PL_new_term_ref();
		char* param_name;
		double value;

		if (!PL_is_functor(head, eq_functor)) {
			return PL_type_error("key_value_pair", head);
		}
		if (!PL_get_arg(1, head, key_term)) return FALSE;
		if (!PL_get_arg(2, head, value_term)) return FALSE;
		if (!PL_get_atom_chars(key_term, &param_name)) {
			return PL_type_error("atom", key_term);
		}
		if (!PL_get_float(value_term, &value)) {
			return PL_type_error("float", value_term);
		}

		if (strcmp(param_name, "predelay_ms") == 0) {
			reverb->predelay_ms = (float)value;
		} else if (strcmp(param_name, "bandwidth") == 0) {
			reverb->bandwidth = (float)value;
		} else if (strcmp(param_name, "decay") == 0) {
			reverb->decay = (float)value;
		} else if (strcmp(param_name, "damping") == 0) {
			reverb->damping = (float)value;
		} else if (strcmp(param_name, "mod_rate") == 0) {
			reverb->mod_rate = (float)value;
		} else if (strcmp(param_name, "mod_depth") == 0) {
			reverb->mod_depth = (float)value;
		} else if (strcmp(param_name, "shimmer1_shift") == 0) {
			reverb->shimmer1_shift = (float)value;
		} else if (strcmp(param_name, "shimmer1_mix") == 0) {
			reverb->shimmer1_mix = (float)value;
		} else if (strcmp(param_name, "shimmer2_shift") == 0) {
			reverb->shimmer2_shift = (float)value;
		} else if (strcmp(param_name, "shimmer2_mix") == 0) {
			reverb->shimmer2_mix = (float)value;
		} else if (strcmp(param_name, "width") == 0) {
			reverb->width = (float)value;
		} else if (strcmp(param_name, "cross_feed") == 0) {
			reverb->cross_feed = (float)value;
		} else if (strcmp(param_name, "low_cut") == 0) {
			reverb->low_cut = (float)value;
		} else if (strcmp(param_name, "high_cut") == 0) {
			reverb->high_cut = (float)value;
		} else if (strcmp(param_name, "wet") == 0) {
			reverb->wet = (float)value;
		} else if (strcmp(param_name, "dry") == 0) {
			reverb->dry = (float)value;
		} else {
			return PL_domain_error("reverb_parameter", key_term);
		}
	}

	if (!PL_get_nil(list)) {
		return PL_type_error("list", params_list);
	}

	return TRUE;
}

/******************************************************************************
 * PAN EFFECT
 *****************************************************************************/

/*
 * pan_process_pcm_frames()
 * Interpolate toward target pan each frame, applies stereo panning.
 */
static void pan_process_pcm_frames(
		ma_node* node,
		const float** frames_in,
		ma_uint32* frame_count_in,
		float** frames_out,
		ma_uint32* frame_count_out)
{
	pan_node_t* pan_node;
	ma_uint32 channels;
	ma_uint32 frame_count;
	const float* input;
	float* output;
	ma_uint32 i;
	float step, p, left_gain, right_gain;

	pan_node = (pan_node_t*)node;
	channels = ma_node_get_output_channels(node, 0);
	frame_count = *frame_count_out;
	input = frames_in[0];
	output = frames_out[0];

	/* pan only applies to stereo; pass through for other channel counts */
	if (channels != 2) {
		memcpy(output, input, frame_count * channels * sizeof(float));
		*frame_count_in = frame_count;
		*frame_count_out = frame_count;
		return;
	}

	step = (pan_node->target_pan - pan_node->current_pan) / frame_count;

	for (i = 0; i < frame_count; i++) {
		pan_node->current_pan += step;
		p = pan_node->current_pan * 0.5f + 0.5f; /* -1 .. 1 -> 0 .. 1 */
		left_gain = 1.0f - p;
		right_gain = p;

		output[i * 2 + 0] = input[i * 2 + 0] * left_gain + input[i * 2 + 1] * (1.0f - right_gain);
		output[i * 2 + 1] = input[i * 2 + 0] * (1.0f - left_gain) + input[i * 2 + 1] * right_gain;
	}

	pan_node->current_pan = pan_node->target_pan;

	*frame_count_in = frame_count;
	*frame_count_out = frame_count;
}

static ma_node_vtable pan_vtable = {
	pan_process_pcm_frames,
	NULL,
	1,  /* 1 input bus */
	1,  /* 1 output bus */
	0
};

/*
 * init_pan_node()
 * Initialize a pan effect node.
 */
static ma_result init_pan_node(pan_node_t* node, float initial_pan)
{
	ma_result result;

	result = init_effect_node_base(&node->base, &pan_vtable);
	if (result != MA_SUCCESS) {
		return result;
	}

	node->current_pan = initial_pan;
	node->target_pan = initial_pan;

	return MA_SUCCESS;
}

/*
 * attach_pan_effect()
 * Create and attach a pan effect to the given source node.
 */
static ma_result attach_pan_effect(term_t params, ma_node* source_node, effect_node_t** effect_chain, ma_node_base** out_effect_node)
{
	float pan;
	pan_node_t* node;
	ma_result result;

	if (!get_param_float(params, "pan", &pan)) {
		pan = 0.0f;
	}

	node = (pan_node_t*)malloc(sizeof(pan_node_t));
	if (!node) {
		return MA_OUT_OF_MEMORY;
	}

	result = init_pan_node(node, pan);
	if (result != MA_SUCCESS) {
		free(node);
		return result;
	}

	result = attach_effect_node(source_node, effect_chain, &node->base, EFFECT_PAN);
	if (result != MA_SUCCESS) {
		ma_node_uninit(&node->base, NULL);
		free(node);
		return result;
	}

	*out_effect_node = &node->base;
	return MA_SUCCESS;
}

/*
 * query_pan_params()
 * Get the current pan value.
 */
static int query_pan_params(pan_node_t* node, term_t params_list)
{
	term_t param_args = PL_new_term_refs(2);
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);
	term_t param_term = PL_new_term_ref();

	PL_put_atom_chars(param_args+0, "pan");
	if (!PL_put_float(param_args+1, node->target_pan)) {
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
 * set_pan_parameters()
 * Set pan effect parameters from a Prolog list.
 */
static foreign_t set_pan_parameters(pan_node_t* node, term_t params_list)
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

		if (strcmp(param_name, "pan") == 0) {
			double pan;
			if (!PL_get_float(value_term, &pan)) {
				return PL_type_error("float", value_term);
			}
			node->target_pan = (float)pan;
		}
	}

	return TRUE;
}


/******************************************************************************
 * VCA EFFECT
 *****************************************************************************/

/*
 * vca_process_pcm_frames()
 * Applies gain with per-sample interpolation from current to target.
 */
static void vca_process_pcm_frames(
		ma_node* node,
		const float** frames_in,
		ma_uint32* frame_count_in,
		float** frames_out,
		ma_uint32* frame_count_out)
{
	vca_node_t* vca;
	ma_uint32 channels;
	ma_uint32 frame_count;
	ma_uint32 total_samples;
	const float* input;
	float* output;
	ma_uint32 i;
	float step, gain;

	vca = (vca_node_t*)node;
	channels = ma_node_get_output_channels(node, 0);
	frame_count = *frame_count_out;
	total_samples = frame_count * channels;
	input = frames_in[0];
	output = frames_out[0];

	step = (vca->target_gain - vca->current_gain) / frame_count;
	gain = vca->current_gain;

	for (i = 0; i < total_samples; i++) {
		if (i % channels == 0 && i > 0) {
			gain += step;
		}
		output[i] = input[i] * gain;
	}

	vca->current_gain = vca->target_gain;

	*frame_count_in = frame_count;
	*frame_count_out = frame_count;
}

static ma_node_vtable vca_vtable = {
	vca_process_pcm_frames,
	NULL,
	1,  /* 1 input bus */
	1,  /* 1 output bus */
	0
};

/*
 * init_vca_node()
 * Initialize a VCA effect node.
 */
static ma_result init_vca_node(vca_node_t* node, float initial_gain)
{
	ma_result result;

	result = init_effect_node_base(&node->base, &vca_vtable);
	if (result != MA_SUCCESS) {
		return result;
	}

	node->current_gain = initial_gain;
	node->target_gain = initial_gain;

	return MA_SUCCESS;
}

/*
 * attach_vca_effect()
 * Create and attach a VCA effect to the given source node.
 */
static ma_result attach_vca_effect(term_t params, ma_node* source_node, effect_node_t** effect_chain, ma_node_base** out_effect_node)
{
	float gain;
	vca_node_t* vca;
	ma_result result;

	if (!get_param_float(params, "gain", &gain)) {
		gain = 1.0f;
	}

	vca = (vca_node_t*)malloc(sizeof(vca_node_t));
	if (!vca) {
		return MA_OUT_OF_MEMORY;
	}

	result = init_vca_node(vca, gain);
	if (result != MA_SUCCESS) {
		free(vca);
		return result;
	}

	result = attach_effect_node(source_node, effect_chain, &vca->base, EFFECT_VCA);
	if (result != MA_SUCCESS) {
		ma_node_uninit(&vca->base, NULL);
		free(vca);
		return result;
	}

	*out_effect_node = &vca->base;
	return MA_SUCCESS;
}

/*
 * query_vca_params()
 * Build parameter list for VCA effect.
 */
static int query_vca_params(vca_node_t* vca, term_t params_list)
{
	term_t param_args = PL_new_term_refs(2);
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);
	term_t param_term = PL_new_term_ref();

	PL_put_atom_chars(param_args+0, "gain");
	if (!PL_put_float(param_args+1, vca->target_gain)) {
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
 * set_vca_parameters()
 * Set parameters for VCA effect from Prolog term list.
 */
static foreign_t set_vca_parameters(vca_node_t* vca, term_t params_list)
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

		if (strcmp(param_name, "gain") == 0) {
			double gain;
			if (!PL_get_float(value_term, &gain)) {
				return PL_type_error("float", value_term);
			}
			vca->target_gain = (float)gain;
		} else {
			return PL_domain_error("vca_parameter", key_term);
		}
	}

	if (!PL_get_nil(list)) {
		return PL_type_error("list", params_list);
	}

	return TRUE;
}

/******************************************************************************
 * COMPRESSOR (with look-ahead)
 *****************************************************************************/

#define COMPRESSOR_DEFAULT_LOOKAHEAD_MS 5.0f

/*
 * compute_compressor_gain()
 * Calculate the gain reduction for a given input level.
 * Uses soft knee if knee > 0
 */
static float compute_compressor_gain(compressor_node_t* comp, float input_level)
{
	float input_db;
	float threshold_db;
	float output_db;
	float knee_start;
	float knee_end;
	float slope;

	input_db = AMP_TO_DB(input_level);
	threshold_db = AMP_TO_DB(comp->threshold);
	slope = 1.0f - 1.0f / comp->ratio;

	if (comp->knee <= 0.0f) {
		/* hard knee */
		if (input_db <= threshold_db) {
			return 1.0f;
		}
		output_db = threshold_db + (input_db - threshold_db) / comp->ratio;
	} else {
		/* soft knee */
		knee_start = threshold_db - comp->knee / 2.0f;
		knee_end = threshold_db + comp->knee / 2.0f;

		if (input_db <= knee_start) {
			return 1.0f;
		} else if (input_db >= knee_end) {
			output_db = threshold_db + (input_db - threshold_db) / comp->ratio;
		} else {
			/* quadratic interpolation in knee region */
			output_db = input_db - slope * (input_db - knee_start) * (input_db - knee_start) / (2.0f * comp->knee);
		}
	}

	return DB_TO_AMP(output_db - input_db);
}

/*
 * compressor_process_pcm_frames()
 * Look-ahead compressor with ratio control.
 */
static void compressor_process_pcm_frames(
		ma_node* node,
		const float** frames_in,
		ma_uint32* frame_count_in,
		float** frames_out,
		ma_uint32* frame_count_out)
{

	compressor_node_t* comp = (compressor_node_t*)node;
	const float* input = frames_in[0];
	float* output = frames_out[0];
	ma_uint32 frame_count = *frame_count_in;
	ma_uint32 channels = ma_node_get_output_channels(node, 0);
	ma_uint32 delay_size = comp->delay_frames * channels;
	ma_uint32 i, j, c;
	float peak, gain, sample;
	ma_uint32 lookahead_pos;

	for (i = 0; i < frame_count; i++) {
		/* write input to delay buffer */
		for (c = 0; c < channels; c++) {
			comp->delay_buffer[comp->delay_pos + c] = input[i * channels + c];
		}

		/* advance write position, now pointing to oldest sample (next output) */
		comp->delay_pos = (comp->delay_pos + channels) % delay_size;


		/* scan look-ahead window for peak */
		peak = 0.0f;
		for (j = 0; j < comp->delay_frames; j++) {
			lookahead_pos = (comp->delay_pos + j * channels) % delay_size;
			for (c = 0; c < channels; c++) {
				sample = fabsf(comp->delay_buffer[lookahead_pos + c]);
				if (sample > peak) peak = sample;
			}
		}

		/* envelope follower: fast attack, slow release */
		if (peak > comp->envelope) {
			comp->envelope += comp->attack_coeff * (peak - comp->envelope);
		} else {
			comp->envelope += comp->release_coeff * (peak - comp->envelope);
		}

		/* compute and apply gain */
		gain = compute_compressor_gain(comp, comp->envelope);
		gain *= comp->makeup_gain;

		/* output the delayed frame with gain applied */
		for (c = 0; c < channels; c++) {
			output[i * channels + c] = comp->delay_buffer[comp->delay_pos + c] * gain;
		}
	}

	*frame_count_out = frame_count;
}

static ma_node_vtable compressor_vtable = {
	compressor_process_pcm_frames,
	NULL,
	1,  /* 1 input bus */
	1,  /* 1 output bus */
	0
};

/*
 * init_compressor_node()
 * Initialize a compressor effect node with look-ahead buffer.
 */
static ma_result init_compressor_node(
		compressor_node_t* node, 
		float threshold, float ratio, float knee, 
		float attack_ms, float release_ms,
		float makeup_gain,
		float lookahead_ms)
{
	ma_result result;
	ma_uint32 sample_rate;
	ma_uint32 channels;

	result = init_effect_node_base(&node->base, &compressor_vtable);
	if (result != MA_SUCCESS) {
		return result;
	}

	get_engine_format_info(NULL, &channels, &sample_rate);

	node->threshold = threshold;
	node->ratio = ratio;
	node->knee = knee;
	node->attack_coeff = 1.0f - expf(-1.0f / (attack_ms * sample_rate / 1000.0f));
	node->release_coeff = 1.0f - expf(-1.0f / (release_ms * sample_rate / 1000.0f));
	node->makeup_gain = makeup_gain;
	node->envelope = 0.0f;
	node->delay_frames = (ma_uint32)(lookahead_ms * sample_rate / 1000.0f);
	node->delay_pos = 0;

	node->delay_buffer = (float*)calloc(node->delay_frames * channels, sizeof(float));
	if (!node->delay_buffer) {
		ma_node_uninit(&node->base, NULL);
		return MA_OUT_OF_MEMORY;
	}

	return MA_SUCCESS;
}


/*
 * attach_compressor_effect()
 * Create and attach a compressor effect to the given source node.
 */
static ma_result attach_compressor_effect(term_t params, ma_node* source_node, effect_node_t** effect_chain, ma_node_base** out_effect_node)
{
	float threshold, ratio, knee;
	float attack_ms, release_ms;
	float makeup_gain, lookahead_ms;
	compressor_node_t* comp;
	ma_result result;

	if (!get_param_float(params, "threshold", &threshold)) {
		threshold = 0.5f;
	}
	if (!get_param_float(params, "ratio", &ratio)) {
		ratio = 4.0f;
	}
	if (!get_param_float(params, "knee", &knee)) {
		knee = 6.0f;
	}
	if (!get_param_float(params, "attack_ms", &attack_ms)) {
		attack_ms = 5.0f;
	}
	if (!get_param_float(params, "release_ms", &release_ms)) {
		release_ms = 100.0f;
	}
	if (!get_param_float(params, "makeup_gain", &makeup_gain)) {
		makeup_gain = 1.0f;
	}
	if (!get_param_float(params, "lookahead_ms", &lookahead_ms)) {
		lookahead_ms = COMPRESSOR_DEFAULT_LOOKAHEAD_MS;
	}

	comp = (compressor_node_t*)malloc(sizeof(compressor_node_t));
	if (!comp) {
		return MA_OUT_OF_MEMORY;
	}

	result = init_compressor_node(comp, threshold, ratio, knee, attack_ms, release_ms, makeup_gain, lookahead_ms);
	if (result != MA_SUCCESS) {
		free(comp);
		return result;
	}

	result = attach_effect_node(source_node, effect_chain, &comp->base, EFFECT_COMPRESSOR);
	if (result != MA_SUCCESS) {
		ma_node_uninit(&comp->base, NULL);
		free(comp);
		return result;
	}

	*out_effect_node = &comp->base;
	return MA_SUCCESS;
}

/*
 * query_compressor_params()
 * Build parameter list for compressor effect.
 */
static int query_compressor_params(compressor_node_t* comp, term_t params_list)
{
	term_t param_args = PL_new_term_refs(2);
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);
	term_t param_term = PL_new_term_ref();
	ma_uint32 sample_rate = ma_engine_get_sample_rate(g_engine);
	float attack_ms, release_ms;

	attack_ms = -1000.0f / (sample_rate * logf(1.0f - comp->attack_coeff));
	release_ms = -1000.0f / (sample_rate * logf(1.0f - comp->release_coeff));

	PL_put_atom_chars(param_args+0, "threshold");
	if (!PL_put_float(param_args+1, comp->threshold)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "ratio");
	if (!PL_put_float(param_args+1, comp->ratio)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "knee");
	if (!PL_put_float(param_args+1, comp->knee)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "attack_ms");
	if (!PL_put_float(param_args+1, attack_ms)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "release_ms");
	if (!PL_put_float(param_args+1, release_ms)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "makeup_gain");
	if (!PL_put_float(param_args+1, comp->makeup_gain)) {
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
 * set_compressor_parameters()
 * Set parameters for compressor effect from Prolog term list.
 */
static foreign_t set_compressor_parameters(compressor_node_t* comp, term_t params_list)
{
	term_t list = PL_copy_term_ref(params_list);
	term_t head = PL_new_term_ref();
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);
	ma_uint32 sample_rate = ma_engine_get_sample_rate(g_engine);
	term_t key_term, value_term;
	char* param_name;
	double val;

	while (PL_get_list(list, head, list)) {
		key_term = PL_new_term_ref();
		value_term = PL_new_term_ref();

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

		if (strcmp(param_name, "threshold") == 0) {
			if (!PL_get_float(value_term, &val)) {
				return PL_type_error("float", value_term);
			}
			comp->threshold = (float)val;
		} else if (strcmp(param_name, "ratio") == 0) {
			if (!PL_get_float(value_term, &val)) {
				return PL_type_error("float", value_term);
			}
			comp->ratio = (float)val;
		} else if (strcmp(param_name, "knee") == 0) {
			if (!PL_get_float(value_term, &val)) {
				return PL_type_error("float", value_term);
			}
			comp->knee = (float)val;
		} else if (strcmp(param_name, "attack_ms") == 0) {
			if (!PL_get_float(value_term, &val)) {
				return PL_type_error("float", value_term);
			}
			comp->attack_coeff = 1.0f - expf(-1.0f / ((float)val * sample_rate / 1000.0f));
		} else if (strcmp(param_name, "release_ms") == 0) {
			if (!PL_get_float(value_term, &val)) {
				return PL_type_error("float", value_term);
			}
			comp->release_coeff = 1.0f - expf(-1.0f / ((float)val * sample_rate / 1000.0f));
		} else if (strcmp(param_name, "makeup_gain") == 0) {
			if (!PL_get_float(value_term, &val)) {
				return PL_type_error("float", value_term);
			}
			comp->makeup_gain = (float)val;
		} else {
			return PL_domain_error("compressor_parameter", key_term);
		}
	}

	if (!PL_get_nil(list)) {
		return PL_type_error("list", params_list);
	}

	return TRUE;
}

/******************************************************************************
 * MOOG LADDER FILTER
 *****************************************************************************/

#define MOOG_VT 0.312
#define MOOG_INPUT_SCALE 0.5
#define MOOG_OVERSAMPLE 2

/*
 * moog_process_one_sample()
 * Process one sample through the 4-pole ladder filter.
 */
/*
 * moog_process_one_sample()
 * Process one sample through the 4-pole ladder filter.
 */
static void moog_process_one_sample(
		moog_node_t* moog,
		double input_sample,
		double g,
		double vt2,
		double sample_rate_2x,
		ma_uint32 channel)
{
	double in_sample;
	double dv0, dv1, dv2, dv3;

	in_sample = input_sample - ((double)moog->current_resonance * moog->tv[3][channel]);

	dv0 = g * (tanh(in_sample / vt2) - moog->tv[0][channel]);
	moog->v[0][channel] += (dv0 + moog->dv[0][channel]) / (2.0 * sample_rate_2x);
	moog->dv[0][channel] = dv0;
	moog->tv[0][channel] = tanh(moog->v[0][channel] / vt2);

	dv1 = g * (moog->tv[0][channel] - moog->tv[1][channel]);
	moog->v[1][channel] += (dv1 + moog->dv[1][channel]) / (2.0 * sample_rate_2x);
	moog->dv[1][channel] = dv1;
	moog->tv[1][channel] = tanh(moog->v[1][channel] / vt2);

	dv2 = g * (moog->tv[1][channel] - moog->tv[2][channel]);
	moog->v[2][channel] += (dv2 + moog->dv[2][channel]) / (2.0 * sample_rate_2x);
	moog->dv[2][channel] = dv2;
	moog->tv[2][channel] = tanh(moog->v[2][channel] / vt2);

	dv3 = g * (moog->tv[2][channel] - moog->tv[3][channel]);
	moog->v[3][channel] += (dv3 + moog->dv[3][channel]) / (2.0 * sample_rate_2x);
	moog->dv[3][channel] = dv3;
	moog->tv[3][channel] = tanh(moog->v[3][channel] / vt2);
}

/*
 * moog_process_pcm_frames()
 * Process audio through 4-pole Moog ladder filter.
 * Based on D'Angelo and Valimaki, ICASSP 2013,
 */
static void moog_process_pcm_frames(
		ma_node* node,
		const float** frames_in,
		ma_uint32* frame_count_in,
		float** frames_out,
		ma_uint32* frame_count_out)
{
	moog_node_t* moog;
	ma_uint32 channels;
	ma_uint32 frame_count;
	const float* input;
	float* output;
	ma_uint32 frame, channel;
	float cutoff_step, res_step;
	double dv0, dv1, dv2, dv3;
	double vt2, g, x;
	double in_sample;
	double sample_rate, sample_rate_2x;
	double current_sample, interp_sample, drive_input;

	moog = (moog_node_t*)node;
	frame_count = *frame_count_out;
	input = frames_in[0];
	output = frames_out[0];
	channels = ma_node_get_output_channels(node, 0);
	sample_rate = (double)ma_engine_get_sample_rate(g_engine);
	sample_rate_2x = sample_rate * MOOG_OVERSAMPLE;
	vt2 = 2.0 * MOOG_VT;

	cutoff_step = (moog->target_cutoff - moog->current_cutoff) / (float)frame_count;
	res_step = (moog->target_resonance - moog->current_resonance) / (float)frame_count;

	for (frame = 0; frame < frame_count; frame++) {
		moog->current_cutoff += cutoff_step;
		moog->current_resonance += res_step;

		/* bilinear tranfrom pre-warp for frequency accuracy */
		x = (M_PI * (double)moog->current_cutoff) / sample_rate_2x;
		g = 4.0 * M_PI * MOOG_VT * (double)moog->current_cutoff * (1.0 - x) / (1.0 + x);

		for (channel = 0; channel < channels; channel++) {
			current_sample = (double)input[frame * channels + channel];

			/* interpolated sample: midpoint between previous and current */
			interp_sample = (moog->prev_input[channel] + current_sample) * 0.5;

			/* first pass: process interpolated sample */
			drive_input = interp_sample * MOOG_INPUT_SCALE * (double)moog->drive;
			moog_process_one_sample(moog, drive_input, g, vt2, sample_rate_2x, channel);

			/* second pass: process current sample */
			drive_input = current_sample * MOOG_INPUT_SCALE * (double)moog->drive;
			moog_process_one_sample(moog, drive_input, g, vt2, sample_rate_2x, channel);

			/* store for next frame's interpolation */
			moog->prev_input[channel] = (float)current_sample;

			/* resonance compensation: boost output to offset volume loss at high resonance */
			output[frame * channels + channel] = (float)(moog->v[3][channel] / MOOG_INPUT_SCALE)
				* (1.0f + 0.5f * moog->current_resonance);
		}
	}

	moog->current_cutoff = moog->target_cutoff;
	moog->current_resonance = moog->target_resonance;

	*frame_count_in = frame_count;
	*frame_count_out = frame_count;
}

static ma_node_vtable moog_vtable = {
	moog_process_pcm_frames,
	NULL,
	1,
	1,
	0
};

/*
 * moog_node_init()
 * Initialize moog ladder filter node
 */
static ma_result moog_node_init(moog_node_t* node) 
{
	ma_result result;
	int pole, channel;

	result = init_effect_node_base(&node->base, &moog_vtable);
	if (result != MA_SUCCESS) {
		return result;
	}

	for (pole = 0; pole < 4; pole++) {
		for (channel = 0; channel < 2; channel++) {
			node->v[pole][channel] = 0.0;
			node->dv[pole][channel] = 0.0;
			node->tv[pole][channel] = 0.0;
		}
	}

	node->prev_input[0] = 0.0f;
	node->prev_input[1] = 0.0f;

	node->current_cutoff = 1000.0f;
	node->target_cutoff = node->current_cutoff;
	node->current_resonance = 0.0f;
	node->target_resonance = node->current_resonance;
	node->drive = 1.0f;

	return MA_SUCCESS;
}

/*
 * attach_moog_effect()
 * Attach moog ladder filter to effect chain.
 */
static ma_result attach_moog_effect(term_t params, ma_node* source, effect_node_t** chain, ma_node_base** out_node)
{
	moog_node_t* node;
	ma_result result;
	float cutoff = 1000.0f;
	float resonance = 0.0f;
	float drive = 1.0f;

	if (!get_param_float(params, "cutoff", &cutoff)) {
		cutoff = 1000.0f;
	}
	if (!get_param_float(params, "resonance", &resonance)) {
		resonance = 0.0f;
	}
	if (!get_param_float(params, "drive", &drive)) {
		drive = 1.0f;
	}

	node = malloc(sizeof(moog_node_t));
	if (!node) {
		return MA_OUT_OF_MEMORY;
	}

	result = moog_node_init(node);
	if (result != MA_SUCCESS) {
		free(node);
		return result;
	}

	node->current_cutoff = cutoff;
	node->target_cutoff = cutoff;
	node->current_resonance = resonance;
	node->target_resonance = resonance;
	node->drive = drive;

	result = attach_effect_node(source, chain, &node->base, EFFECT_MOOG);
	if (result != MA_SUCCESS) {
		ma_node_uninit(&node->base, NULL);
		free(node);
		return result;
	}

	*out_node = &node->base;
	return MA_SUCCESS;
}

/*
 * query_moog_params()
 * Get current moog filter parameters.
 */
static int query_moog_params(moog_node_t* node, term_t params_list)
{
	term_t param_args = PL_new_term_refs(2);
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);
	term_t param_term = PL_new_term_ref();

	PL_put_atom_chars(param_args+0, "drive");
	if (!PL_put_float(param_args+1, node->drive)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "resonance");
	if (!PL_put_float(param_args+1, node->target_resonance)) {
		return FALSE;
	}
	if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
		return FALSE;
	}
	if (!PL_cons_list(params_list, param_term, params_list)) {
		return FALSE;
	}

	PL_put_atom_chars(param_args+0, "cutoff");
	if (!PL_put_float(param_args+1, node->target_cutoff)) {
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
 * set_moog_parameters()
 * Set moog filter parameters from a Prolog list.
 */
static foreign_t set_moog_parameters(moog_node_t* node, term_t params_list)
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
			if (!PL_get_float(value_term, &cutoff)) {
				return PL_type_error("float", value_term);
			}
			if (cutoff < 20.0) cutoff = 20.0;
			node->target_cutoff = (float)cutoff;
		} else if (strcmp(param_name, "resonance") == 0) {
			double resonance;
			if (!PL_get_float(value_term, &resonance)) {
				return PL_type_error("float", value_term);
			}
			if (resonance < 0.0) resonance = 0.0;
			if (resonance > 4.0) resonance = 4.0;
			node->target_resonance = (float)resonance;
		} else if (strcmp(param_name, "drive") == 0) {
			double drive;
			if (!PL_get_float(value_term, &drive)) {
				return PL_type_error("float", value_term);
			}
			if (drive < 0.1) drive = 0.1;
			node->drive = (float)drive;
		}
	}

	return TRUE;
}


/******************************************************************************
 * PROLOG INTERFACE
 *****************************************************************************/

/*
 * attach_effect_to_node()
 * Generic effect attachment - dispatches to appropriate effect type.
 * source_type: "sound" or "voice"
 */
static foreign_t attach_effect_to_node(ma_sound* sound, effect_node_t** effect_chain, const char* source_type, int slot, term_t effect_type, term_t params, term_t effect_handle)
{
	char* type_str;
	ma_result result;
	ma_node_base* effect_node;
	term_t effect_term, source_term, arg;
	functor_t effect_functor, source_functor;

	if (!PL_get_atom_chars(effect_type, &type_str)) {
		return PL_type_error("atom", effect_type);
	}

	if (strcmp(type_str, "bitcrush") == 0) {
		result = attach_bitcrush_effect(params, (ma_node*)sound, effect_chain, &effect_node);
	} else if (strcmp(type_str, "lpf") == 0) {
		result = attach_lpf_effect(params, (ma_node*)sound, effect_chain, &effect_node);
	} else if (strcmp(type_str, "hpf") == 0) {
		result = attach_hpf_effect(params, (ma_node*)sound, effect_chain, &effect_node);
	} else if (strcmp(type_str, "bpf") == 0) {
		result = attach_bpf_effect(params, (ma_node*)sound, effect_chain, &effect_node);
	} else if (strcmp(type_str, "delay") == 0) {
		result = attach_delay_effect(params, (ma_node*)sound, effect_chain, &effect_node);
	} else if (strcmp(type_str, "ping_pong_delay") == 0) {
		result = attach_ping_pong_delay_effect(params, (ma_node*)sound, effect_chain, &effect_node);
	} else if (strcmp(type_str, "reverb") == 0) {
		result = attach_reverb_effect(params, (ma_node*)sound, effect_chain, &effect_node);
	} else if (strcmp(type_str, "pan") == 0) {
		result = attach_pan_effect(params, (ma_node*)sound, effect_chain, &effect_node);
	} else if (strcmp(type_str, "moog") == 0) {
		result = attach_moog_effect(params, (ma_node*)sound, effect_chain, &effect_node);
	} else if (strcmp(type_str, "vca") == 0) {
		result = attach_vca_effect(params, (ma_node*)sound, effect_chain, &effect_node);
	} else if (strcmp(type_str, "compressor") == 0) {
		result = attach_compressor_effect(params, (ma_node*)sound, effect_chain, &effect_node);
	} else {
		return PL_domain_error("effect_type", effect_type);
	}

	if (result != MA_SUCCESS) {
		return FALSE;
	}

	/* Build effect(source_type(Slot), Pointer) term */
	source_functor = PL_new_functor(PL_new_atom(source_type), 1);
	source_term = PL_new_term_ref();
	if (!PL_unify_functor(source_term, source_functor)) return FALSE;
	arg = PL_new_term_ref();
	if (!PL_get_arg(1, source_term, arg) || !PL_unify_integer(arg, slot)) return FALSE;

	effect_functor = PL_new_functor(PL_new_atom("effect"), 2);
	effect_term = PL_new_term_ref();
	if (!PL_unify_functor(effect_term, effect_functor)) return FALSE;
	if (!PL_get_arg(1, effect_term, arg) || !PL_unify(arg, source_term)) return FALSE;
	if (!PL_get_arg(2, effect_term, arg) || !PL_unify_pointer(arg, effect_node)) return FALSE;
	return PL_unify(effect_handle, effect_term);
}

static foreign_t pl_sound_attach_effect(term_t handle, term_t effect_type, term_t params, term_t effect_handle)
{
	int slot;
	if (!PL_get_integer(handle, &slot) || slot < 0 || slot >= MAX_SOUNDS || !g_sounds[slot].in_use)
		return PL_existence_error("sound", handle);
	return attach_effect_to_node(g_sounds[slot].sound, &g_sounds[slot].effect_chain, "sound", slot, effect_type, params, effect_handle);
}

static foreign_t pl_voice_attach_effect(term_t handle, term_t effect_type, term_t params, term_t effect_handle)
{
	int slot;
	if (!PL_get_integer(handle, &slot) || slot < 0 || slot >= MAX_VOICES || !g_voices[slot].in_use)
		return PL_existence_error("voice", handle);
	return attach_effect_to_node(&g_voices[slot].group, &g_voices[slot].effect_chain, "voice", slot, effect_type, params, effect_handle);
}

static foreign_t pl_summing_node_attach_effect(term_t handle, term_t effect_type, term_t params, term_t effect_handle)
{
	int slot;
	if (!PL_get_integer(handle, &slot) || slot < 0 || slot >= MAX_SUMMING_NODES || !g_summing_nodes[slot].in_use)
		return PL_existence_error("summing_node", handle);
	return attach_effect_to_node((ma_sound*)&g_summing_nodes[slot].base, &g_summing_nodes[slot].effect_chain, "summing_node", slot, effect_type, params, effect_handle);
}

static foreign_t pl_image_synth_attach_effect(term_t handle, term_t effect_type, term_t params, term_t effect_handle)
{
	int slot;
	if (!PL_get_integer(handle, &slot) || slot < 0 || slot >= MAX_IMAGE_SYNTHS || !g_image_synths[slot].in_use)
		return PL_existence_error("image_synth", handle);
	return attach_effect_to_node((ma_sound*)&g_image_synths[slot].base, &g_image_synths[slot].effect_chain, "image_synth", slot, effect_type, params, effect_handle);
}

static foreign_t pl_granular_attach_effect(term_t handle, term_t effect_type, term_t params, term_t effect_handle)
{
	int slot;
	if (!PL_get_integer(handle, &slot) || slot < 0 || slot >= MAX_GRANULAR_DELAYS || !g_granular_delays[slot].in_use)
		return PL_existence_error("granular", handle);
	return attach_effect_to_node((ma_sound*)&g_granular_delays[slot].base, &g_granular_delays[slot].effect_chain, "granular", slot, effect_type, params, effect_handle);
}

/*
 * pl_effects()
 * Query all effects attached to a sound or voice.
 * First arg: sound(N) or voice(N)
 */
static foreign_t pl_effects(term_t source_handle, term_t effects_list)
{
	term_t slot_term = PL_new_term_ref();
	functor_t f;
	int slot;
	effect_node_t* effect_chain;
	effect_node_t* effect;
	const char* source_type;
	int count = 0;

	if (!PL_get_functor(source_handle, &f)) {
		return PL_type_error("sound_or_voice", source_handle);
	}
	if (!PL_get_arg(1, source_handle, slot_term)) {
		return PL_type_error("sound_or_voice", source_handle);
	}
	if (!PL_get_integer(slot_term, &slot)) {
		return PL_type_error("integer", slot_term);
	}

	if (f == PL_new_functor(PL_new_atom("sound"), 1)) {
		if (slot < 0 || slot >= MAX_SOUNDS || !g_sounds[slot].in_use) {
			return PL_existence_error("sound", source_handle);
		}
		effect_chain = g_sounds[slot].effect_chain;
		source_type = "sound";
	} else if (f == PL_new_functor(PL_new_atom("voice"), 1)) {
		if (slot < 0 || slot >= MAX_VOICES || !g_voices[slot].in_use) {
			return PL_existence_error("voice", source_handle);
		}
		effect_chain = g_voices[slot].effect_chain;
		source_type = "voice";
	} else if (f == PL_new_functor(PL_new_atom("summing_node"), 1)) {
		if (slot < 0 || slot >= MAX_SUMMING_NODES || !g_summing_nodes[slot].in_use) {
			return PL_existence_error("summing_node", source_handle);
		}
		effect_chain = g_summing_nodes[slot].effect_chain;
		source_type = "summing_node";
	} else if (f == PL_new_functor(PL_new_atom("granular"), 1)) {
		if (slot < 0 || slot >= MAX_GRANULAR_DELAYS || !g_granular_delays[slot].in_use) {
			return PL_existence_error("granular", source_handle);
		}
		effect_chain = g_granular_delays[slot].effect_chain;
		source_type = "granular";
	} else {
		return PL_type_error("sound_or_voice_or_summing_node_or_granular", source_handle);
	}

	for (effect = effect_chain; effect != NULL; effect = effect->next) {
		count++;
	}

	term_t list = PL_new_term_ref();
	PL_put_nil(list);

	if (count > 0) {
		effect_node_t** effects_array = malloc(count * sizeof(effect_node_t*));
		int i = 0;
		for (effect = effect_chain; effect != NULL; effect = effect->next) {
			effects_array[i++] = effect;
		}

		term_t effect_term = PL_new_term_ref();
		term_t args = PL_new_term_refs(4);
		functor_t effect_functor = PL_new_functor(PL_new_atom("effect"), 4);
		functor_t source_functor = PL_new_functor(PL_new_atom(source_type), 1);

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
				case EFFECT_REVERB:
					type_str = "reverb";
					query_result = query_reverb_params((reverb_node_t*)effect->effect_node, params_list);
					break;
				case EFFECT_PAN:
					type_str = "pan";
					query_result = query_pan_params((pan_node_t*)effect->effect_node, params_list);
					break;
				case EFFECT_MOOG:
					type_str = "moog";
					query_result = query_moog_params((moog_node_t*)effect->effect_node, params_list);
					break;
				case EFFECT_VCA:
					type_str = "vca";
					query_result = query_vca_params((vca_node_t*)effect->effect_node, params_list);
					break;
				case EFFECT_COMPRESSOR:
					type_str = "compressor";
					query_result = query_compressor_params((compressor_node_t*)effect->effect_node, params_list);
					break;
				default:
					type_str = "unknown";
					break;
			}

			if (!query_result) {
				free(effects_array);
				return FALSE;
			}

			/* Build source(Slot) term */
			term_t source_term = PL_new_term_ref();
			term_t slot_arg = PL_new_term_ref();
			if (!PL_put_integer(slot_arg, slot)) {
				free(effects_array);
				return FALSE;
			}
			if (!PL_cons_functor_v(source_term, source_functor, slot_arg)) {
				free(effects_array);
				return FALSE;
			}

			/* Build effect(source(Slot), Type, Ptr, Params) */
			if (!PL_put_term(args+0, source_term)) {
				free(effects_array);
				return FALSE;
			}
			PL_put_atom_chars(args+1, type_str);
			if (!PL_put_pointer(args+2, effect->effect_node)) {
				free(effects_array);
				return FALSE;
			}
			if (!PL_put_term(args+3, params_list)) {
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
 * pl_effect_set_parameters()
 * Set parameters on an effect
 */
static foreign_t pl_effect_set_parameters(term_t effect_handle, term_t params_list)
{
	ma_sound* source;
	effect_node_t** effect_chain;
	void* effect_ptr;

	if (!get_effect_info_from_handle(effect_handle, &source, &effect_chain, &effect_ptr)) {
		return PL_existence_error("effect", effect_handle);
	}

	/* Find the effect in the chain */
	effect_node_t* node = *effect_chain;
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
		case EFFECT_REVERB:
			return set_reverb_parameters((reverb_node_t*)node->effect_node, params_list);
		case EFFECT_PAN:
			return set_pan_parameters((pan_node_t*)node->effect_node, params_list);
		case EFFECT_MOOG:
			return set_moog_parameters((moog_node_t*)node->effect_node, params_list);
		case EFFECT_VCA:
			return set_vca_parameters((vca_node_t*)node->effect_node, params_list);
		case EFFECT_COMPRESSOR:
			return set_compressor_parameters((compressor_node_t*)node->effect_node, params_list);
		default:
			return PL_domain_error("effect_type", effect_handle);
	}
}

/*
 * pl_effect_detach()
 * Remove an effect from the effect chain
 */
static foreign_t pl_effect_detach(term_t effect_handle)
{
	ma_sound* source;
	effect_node_t** effect_chain;
	void* effect_ptr;

	if (!get_effect_info_from_handle(effect_handle, &source, &effect_chain, &effect_ptr)) {
		return PL_existence_error("effect", effect_handle);
	}

	/* Find and remove the effect from the chain */
	effect_node_t* node = *effect_chain;
	effect_node_t* prev = NULL;

	while (node) {
		if (node->effect_node == effect_ptr) {
			ma_node_detach_output_bus(node->effect_node, 0);

			if (node->type == EFFECT_BITCRUSH) {
				bitcrush_node_t* bitcrush = (bitcrush_node_t*)node->effect_node;
				if (bitcrush->hold_samples) {
					free(bitcrush->hold_samples);
				}
			} else if (node->type == EFFECT_REVERB) {
				free_reverb_node((reverb_node_t*)node->effect_node);
			} else if (node->type == EFFECT_COMPRESSOR) {
				compressor_node_t* comp = (compressor_node_t*)node->effect_node;
				if (comp->delay_buffer) {
					free(comp->delay_buffer);
				}
			}
			ma_node_uninit(node->effect_node, NULL);
			free(node->effect_node);

			if (prev) {
				prev->next = node->next;
			} else {
				*effect_chain = node->next;
			}

			free(node);

			/* reconnect the chain */
			effect_node_t* first_effect = *effect_chain;
			if (first_effect) {
				ma_node_attach_output_bus(source, 0, first_effect->effect_node, 0);
				effect_node_t* current = first_effect;
				while (current->next) {
					ma_node_attach_output_bus(current->effect_node, 0, current->next->effect_node, 0);
					current = current->next;
				}
				ma_node_attach_output_bus(current->effect_node, 0, ma_engine_get_endpoint(g_engine), 0);
			} else {
				ma_node_attach_output_bus(source, 0, ma_engine_get_endpoint(g_engine), 0);
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
	PL_register_foreign("sound_attach_effect", 4, pl_sound_attach_effect, 0);
	PL_register_foreign("voice_attach_effect", 4, pl_voice_attach_effect, 0);
	PL_register_foreign("summing_node_attach_effect", 4, pl_summing_node_attach_effect, 0);
	PL_register_foreign("image_synth_attach_effect", 4, pl_image_synth_attach_effect, 0);
	PL_register_foreign("granular_attach_effect", 4, pl_granular_attach_effect, 0);
	PL_register_foreign("effects", 2, pl_effects, 0);
	PL_register_foreign("effect_set_parameters", 2, pl_effect_set_parameters, 0);
	PL_register_foreign("effect_detach", 1, pl_effect_detach, 0);
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
