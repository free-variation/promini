/*
 * sampler.c - Prolog interface to miniaudio (engine API)
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#include <stdlib.h>
#include <string.h>
#include <math.h>

#if defined(__ARM_NEON) || defined(__aarch64__)
#include <arm_neon.h>
#endif

#include <SWI-Prolog.h>
#include <SWI-Stream.h>

#define MINIAUDIO_IMPLEMENTATION
#include "../../include/miniaudio.h"


/*
 * Forward declarations
 */
static foreign_t pl_sampler_init(void);

/*
 * Macro to ensure engine is initialized before operations
 */
#define ENSURE_ENGINE_INITIALIZED() \
    do { \
        if (g_engine == NULL) { \
            if (!pl_sampler_init()) { \
                return FALSE; \
            } \
        } \
    } while(0)

/*
 * Macro to validate handle and get sound pointer
 */
#define GET_SOUND_FROM_HANDLE(handle_term, sound_var) \
    do { \
        int _slot; \
        if (!PL_get_integer(handle_term, &_slot)) { \
            return PL_type_error("integer", handle_term); \
        } \
        sound_var = get_sound(_slot); \
        if (sound_var == NULL) { \
            return PL_existence_error("sound", handle_term); \
        } \
    } while(0)

/*
 * Macro to validate handle and get data buffer pointer
 */
#define GET_DATA_BUFFER_FROM_HANDLE(handle_term, buffer_var) \
    do { \
        int _slot; \
        if (!PL_get_integer(handle_term, &_slot)) { \
            return PL_type_error("integer", handle_term); \
        } \
        buffer_var = get_data_buffer(_slot); \
        if (buffer_var == NULL) { \
            return PL_existence_error("data_buffer", handle_term); \
        } \
    } while(0)

/*
 * Macro to validate handle and get data buffer pointer with slot index
 * Use when you need access to both the buffer and the slot (e.g., for pData access)
 */
#define GET_DATA_BUFFER_WITH_SLOT(handle_term, buffer_var, slot_var) \
    do { \
        if (!PL_get_integer(handle_term, &slot_var)) { \
            return PL_type_error("integer", handle_term); \
        } \
        buffer_var = get_data_buffer(slot_var); \
        if (buffer_var == NULL) { \
            return PL_existence_error("data_buffer", handle_term); \
        } \
    } while(0)

/*
 * Macro to validate handle and get both sound pointer and slot index
 */
#define GET_SOUND_WITH_SLOT(handle_term, sound_var, slot_var) \
    do { \
        if (!PL_get_integer(handle_term, &slot_var)) { \
            return PL_type_error("integer", handle_term); \
        } \
        if (slot_var < 0 || slot_var >= MAX_SOUNDS || !g_sounds[slot_var].in_use) { \
            return PL_existence_error("sound", handle_term); \
        } \
        sound_var = g_sounds[slot_var].sound; \
    } while(0)

/*
 * Global engine - one engine for the library lifetime
 */
static ma_engine* g_engine = NULL;

/*
 * Sound handle management
 */
#define MAX_SOUNDS 1024
#define MAX_DATA_BUFFERS 32

typedef struct {
	ma_audio_buffer* buffer;
	void *pData;
	ma_uint32 refcount;
	ma_bool32 in_use;
} data_slot_t;

static data_slot_t g_data_buffers[MAX_DATA_BUFFERS] = {{NULL, NULL, 0, MA_FALSE}};

/* Effect types */
typedef enum {
	EFFECT_NONE = 0,
	EFFECT_BITCRUSH,
	EFFECT_ENVELOPE,
	EFFECT_REVERB,
	EFFECT_LPF,
	EFFECT_HPF,
	EFFECT_BPF,
	EFFECT_NOTCH,
	EFFECT_PEAK,
	EFFECT_LOSHELF,
	EFFECT_HISHELF
} effect_type_t;

typedef struct effect_node {
	effect_type_t type;
	ma_node_base* effect_node;
	struct effect_node* next;
} effect_node_t;

typedef struct {
    ma_sound* sound;
    ma_audio_buffer* audio_buffer;
    int data_buffer_index; /* -1 if from file, else index into g_data_buffers */
	ma_bool32 in_use;
	effect_node_t* effect_chain;
} sound_slot_t;

static sound_slot_t g_sounds[MAX_SOUNDS] = {{NULL, NULL, -1, MA_FALSE, NULL}};

typedef struct {
	ma_node_base base;
	ma_uint32 target_bits; // 1-16 bits
	ma_uint32 target_sample_rate; // 0 = no downsample
	float* hold_samples; // Sample-and-hold buffer per channel
	ma_uint64 hold_counter;
	ma_uint32 hold_interval; // Frames between updates when downsampling
} bitcrush_node_t;

typedef struct {
	ma_node_base base;
	float attack;
	float decay;
	float Break;
	float release;
	float break_level;
	float duration_ms;
	ma_bool32 loop;
	ma_uint32 stage; /* 0 = attack; 1 = decay; 2 = break; 3 = release */
	float stage_progress;
} adbr_envelope_node_t;


/*
 * allocate_data_slot()
 * Finds a free data slot and marks it as in use.
 * Returns slot index, or -1 if all slots are full.
 */

static int allocate_data_slot(void)
{
	int i;
	for (i = 0; i < MAX_DATA_BUFFERS; i++) {
		if (!g_data_buffers[i].in_use) {
			g_data_buffers[i].in_use = MA_TRUE;
			g_data_buffers[i].buffer = NULL;
			g_data_buffers[i].pData = NULL;
			g_data_buffers[i].refcount = 0;
			return i;
		}
	}
	return -1;
}

/*
 * free_data_slot()
 * Frees a data slot and its resources.
 */
static void free_data_slot(int index)
{
	if (index >= 0 && index < MAX_DATA_BUFFERS) {
		if (g_data_buffers[index].buffer != NULL) {
			ma_audio_buffer_uninit(g_data_buffers[index].buffer);
			free(g_data_buffers[index].buffer);
			g_data_buffers[index].buffer = NULL;
		}
		if (g_data_buffers[index].pData != NULL) {
			ma_free(g_data_buffers[index].pData, NULL);
			g_data_buffers[index].pData = NULL;
		}
		g_data_buffers[index].in_use = MA_FALSE;
		g_data_buffers[index].refcount = 0;
	}
}

/*
 * get_data_buffer()
 * Validates handle and returns buffer pointer.
 * Returns NULL if invalid handle.
 */
static ma_audio_buffer* get_data_buffer(int index)
{
	if (index < 0 || index >= MAX_DATA_BUFFERS) {
		return NULL;
	}
	if (!g_data_buffers[index].in_use) {
		return NULL;
	}
	return g_data_buffers[index].buffer;
}

/*
 * get_buffer_info()
 * Retrieves audio buffer format parameters
 */
static void get_buffer_info(ma_audio_buffer* buffer, ma_uint64* frames, ma_uint32* channels, ma_uint32* sampleRate)
{
	*frames = buffer->ref.sizeInFrames;
	*channels = buffer->ref.channels;
	*sampleRate = buffer->ref.sampleRate;
}

/*
 * get_engine_format_info()
 * Retrieve the engine's audio format parameters.
 */
static void get_engine_format_info(ma_format* format, ma_uint32* channels, ma_uint32* sampleRate)
{
	ma_device* device = ma_engine_get_device(g_engine);
	*format = device->playback.format;
	*channels = device->playback.channels;
	*sampleRate = device->sampleRate;
}

/*
 * create_data_buffer_from_pcm()
 * Creates a data buffer from raw PCM data. Returns slot index or -1 on error.
 * Caller is responsible for freeing pData on error.
 */
static int create_data_buffer_from_pcm(void* pData, ma_format format, ma_uint32 channels,
                                       ma_uint64 frame_count, ma_uint32 sample_rate)
{
	int slot;
	ma_audio_buffer* buffer;
	ma_audio_buffer_config buffer_config;
	ma_result result;

	slot = allocate_data_slot();
	if (slot < 0) {
		return -1;
	}

	buffer = (ma_audio_buffer*)malloc(sizeof(ma_audio_buffer));
	if (buffer == NULL) {
		free_data_slot(slot);
		return -1;
	}

	buffer_config = ma_audio_buffer_config_init(format, channels, frame_count, pData, NULL);
	buffer_config.sampleRate = sample_rate;
	result = ma_audio_buffer_init(&buffer_config, buffer);
	if (result != MA_SUCCESS) {
		free(buffer);
		free_data_slot(slot);
		return -1;
	}

	g_data_buffers[slot].buffer = buffer;
	g_data_buffers[slot].pData = pData;
	g_data_buffers[slot].refcount = 1;

	return slot;
}

/* Bitcrush processing callback */
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

	MA_ASSERT(node != NULL);

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

/* ADBR envelope processing callback */
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

	MA_ASSERT(node != NULL);

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

static ma_node_vtable bitcrush_vtable = 
{
	bitcrush_process_pcm_frames,
	NULL, 	/* onGetRequiredInputFrameCount */
	1,		/* 1 input */
	1,		/* 1 output */
	0		/* default flags */
};

/* sampler_data_load(+FilePath, -Handle)
 * Loads audio data from file into a shareable buffer.
 * Returns a data handle that can be used to create multiple sounds.
 */
static foreign_t pl_sampler_data_load(term_t filepath, term_t handle)
{
	char* path;
	int slot;
	ma_uint64 frame_count;
	void* pData;
	ma_decoder_config decoder_config;
	ma_format format;
	ma_uint32 channels;
	ma_uint32 sample_rate;
	ma_result result;

	ENSURE_ENGINE_INITIALIZED();

	if (!PL_get_chars(filepath, &path, CVT_ATOM | CVT_STRING | CVT_EXCEPTION)) {
		return FALSE;
	}

	get_engine_format_info(&format, &channels, &sample_rate);
	decoder_config = ma_decoder_config_init(format, channels, sample_rate);

	result = ma_decode_file(path, &decoder_config, &frame_count, &pData);
	if (result != MA_SUCCESS) {
		return FALSE;
	}

	slot = create_data_buffer_from_pcm(pData, format, channels, frame_count, sample_rate);
	if (slot < 0) {
		ma_free(pData, NULL);
		return PL_resource_error("data_buffer_slots");
	}

	return PL_unify_integer(handle, slot);
}

/*
 * sampler_data_extract(+SourceHandle, +StartFrame, +Length, -ExtractedHandle)
 * Extracts a slice of frames into a new buffer.
 */
static foreign_t pl_sampler_data_extract(term_t source_handle, term_t start_term, term_t length_term, term_t extracted_handle)
{
	ma_audio_buffer* source_buffer;
  	int source_slot;
  	int start_int, length_int;
  	ma_uint64 start_frame, length_frames;
  	ma_uint64 source_frame_count;
  	ma_uint32 channels;
  	ma_uint32 sample_rate;
  	ma_format format;
  	ma_uint32 bytes_per_frame;
  	void* source_data;
  	void* extracted_data;
  	int slot;

  	GET_DATA_BUFFER_WITH_SLOT(source_handle, source_buffer, source_slot);

  	if (!PL_get_integer(start_term, &start_int)) {
  		return PL_type_error("integer", start_term);
  	}

  	if (!PL_get_integer(length_term, &length_int)) {
  		return PL_type_error("integer", length_term);
  	}

  	if (start_int < 0 || length_int <= 0) {
  		return PL_domain_error("valid_extraction_params", start_int < 0 ? start_term : length_term);
  	}

  	get_buffer_info(source_buffer, &source_frame_count, &channels, &sample_rate);
  	format = source_buffer->ref.format;

  	start_frame = (ma_uint64)start_int;
  	length_frames = (ma_uint64)length_int;

  	if (start_frame + length_frames > source_frame_count) {
  		return PL_domain_error("extraction_bounds", length_term);
  	}

	bytes_per_frame = ma_get_bytes_per_frame(format, channels);

	extracted_data = malloc(length_frames * bytes_per_frame);
	if (extracted_data == NULL) {
		return PL_resource_error("memory");
	}

	source_data = g_data_buffers[source_slot].pData;
	memcpy(extracted_data,
			(char*)source_data + (start_frame * bytes_per_frame),
			length_frames * bytes_per_frame);

	slot = create_data_buffer_from_pcm(extracted_data, format, channels, length_frames, sample_rate);
	if (slot < 0) {
		free(extracted_data);
		return PL_resource_error("data_buffer_slots");
	}

	return PL_unify_integer(extracted_handle, slot);
}

/*
 * sampler_data_unload(+Handle)
 * Decrements reference count on data buffer.
 * Frees the buffer when refcount reaches zero.
 */
static foreign_t pl_sampler_data_unload(term_t handle)
{
	int slot;

	if (!PL_get_integer(handle, &slot)) {
		return PL_type_error("integer", handle);
	}

	if (slot < 0 || slot >= MAX_DATA_BUFFERS || !g_data_buffers[slot].in_use) {
		return PL_existence_error("data_buffer", handle);
	}

	if (g_data_buffers[slot].refcount == 0) {
		return PL_domain_error("refcount_already_zero", handle);
	}

	g_data_buffers[slot].refcount--;

	if (g_data_buffers[slot].refcount == 0) {
		free_data_slot(slot);
	}

	return TRUE;
}

/*
 * allocate_sound_slot()
 * Finds a free sound slot and marks it as in use.
 * Returns slot index, or -1 if all slots are full.
 */
static int allocate_sound_slot(void)
{
    int i;
    for (i = 0; i < MAX_SOUNDS; i++) {
        if (!g_sounds[i].in_use) {
            g_sounds[i].in_use = MA_TRUE;
            g_sounds[i].sound = NULL;
            g_sounds[i].audio_buffer = NULL;
			g_sounds[i].data_buffer_index = -1;
            return i;
        }
    }
    return -1;
}

/*
 * free_sound_slot()
 * Frees a sound slot.
 */
static void free_sound_slot(int index)
{
    if (index >= 0 && index < MAX_SOUNDS) {
        if (g_sounds[index].sound != NULL) {
            ma_sound_uninit(g_sounds[index].sound);
            free(g_sounds[index].sound);
            g_sounds[index].sound = NULL;
        }

		/* Free audio buffer if present */
		if (g_sounds[index].audio_buffer != NULL) {
			ma_audio_buffer_uninit(g_sounds[index].audio_buffer);
			free(g_sounds[index].audio_buffer);
			g_sounds[index].audio_buffer = NULL;
		}

		/* Free effect chain */
		effect_node_t* effect = g_sounds[index].effect_chain;
		while (effect != NULL) {
			effect_node_t* next = effect->next;

			/* Free effect-specific resources */
			if (effect->type == EFFECT_BITCRUSH) {
				bitcrush_node_t* bitcrush = (bitcrush_node_t*)effect->effect_node;
				if (bitcrush->hold_samples != NULL) {
					free(bitcrush->hold_samples);
				}
			}

			ma_node_uninit(effect->effect_node, NULL);
			free(effect->effect_node);
			free(effect);
			effect = next;
		}
		g_sounds[index].effect_chain = NULL;

		/* Decrement data buffer refcount if sound was created from a buffer */
		if (g_sounds[index].data_buffer_index >= 0) {
			g_data_buffers[g_sounds[index].data_buffer_index].refcount--;
			if (g_data_buffers[g_sounds[index].data_buffer_index].refcount == 0) {
				free_data_slot(g_sounds[index].data_buffer_index);
			}
		}

        g_sounds[index].in_use = MA_FALSE;
        g_sounds[index].data_buffer_index = -1;
    }
}

/*
 * get_sound()
 * Validates handle and returns sound pointer.
 * Returns NULL if invalid handle.
 */
static ma_sound* get_sound(int index)
{
    if (index < 0 || index >= MAX_SOUNDS) {
        return NULL;
    }
    if (!g_sounds[index].in_use) {
        return NULL;
    }
    return g_sounds[index].sound;
}


/*
 * sampler_version(-Version)
 * Unifies Version with the miniaudio version string.
 */
static foreign_t pl_sampler_version(term_t version)
{
    return PL_unify_atom_chars(version, MA_VERSION_STRING);
}

/*
 * sampler_init
 * Initializes the global miniaudio engine.
 * Returns true if successful or already initialized.
 */
static foreign_t pl_sampler_init(void)
{
    ma_result result;

    /* Already initialized */
    if (g_engine != NULL) {
        return TRUE;
    }

    /* Allocate engine */
    g_engine = (ma_engine*)malloc(sizeof(ma_engine));
    if (g_engine == NULL) {
        return PL_resource_error("memory");
    }

    /* Initialize engine with default config */
    result = ma_engine_init(NULL, g_engine);
    if (result != MA_SUCCESS) {
        free(g_engine);
        g_engine = NULL;
        return FALSE;
    }

    return TRUE;
}

/*
 * sampler_sound_unload(+Handle)
 * Unloads a sound and frees its resources.
 */
static foreign_t pl_sampler_sound_unload(term_t handle)
{
    int slot;

    if (!PL_get_integer(handle, &slot)) {
        return PL_type_error("integer", handle);
    }

    if (slot < 0 || slot >= MAX_SOUNDS || !g_sounds[slot].in_use) {
        return PL_existence_error("sound", handle);
    }

    free_sound_slot(slot);
    return TRUE;
}

/*
 * sampler_devices(-Devices)
 * Unifies Devices with a list of available audio devices.
 * Each device is represented as device(Name, Type, IsDefault) where:
 *   - Name is the device name string
 *   - Type is 'playback' or 'capture'
 *   - IsDefault is 'true' or 'false'
 */
static foreign_t pl_sampler_devices(term_t devices)
{
    ma_context* pContext;
    ma_result result;
    ma_device_info* pPlaybackInfos;
    ma_uint32 playbackCount;
    ma_device_info* pCaptureInfos;
    ma_uint32 captureCount;
    term_t list = PL_new_term_ref();
    functor_t device_functor;
    ma_uint32 i;

    /* Auto-initialize engine if needed */
    if (g_engine == NULL) {
        if (!pl_sampler_init()) {
            return PL_unify_nil(devices);
        }
    }

    /* Get context from engine */
    pContext = ma_engine_get_device(g_engine)->pContext;

    /* Get device information */
    result = ma_context_get_devices(pContext, &pPlaybackInfos, &playbackCount,
                                     &pCaptureInfos, &captureCount);
    if (result != MA_SUCCESS) {
        return PL_unify_nil(devices);
    }

    /* Create device/3 functor */
    device_functor = PL_new_functor(PL_new_atom("device"), 3);

    /* Build the list (start with nil) */
    PL_put_nil(list);

    /* Add capture devices (reverse order for cons) */
    for (i = captureCount; i > 0; i--) {
        term_t device = PL_new_term_ref();
        term_t args = PL_new_term_refs(3);

        PL_put_atom_chars(args+0, pCaptureInfos[i-1].name);
        PL_put_atom_chars(args+1, "capture");
        PL_put_atom_chars(args+2, pCaptureInfos[i-1].isDefault ? "true" : "false");

        if (!PL_cons_functor_v(device, device_functor, args)) {
            return FALSE;
        }
        if (!PL_cons_list(list, device, list)) {
            return FALSE;
        }
    }

    /* Add playback devices */
    for (i = playbackCount; i > 0; i--) {
        term_t device = PL_new_term_ref();
        term_t args = PL_new_term_refs(3);

        PL_put_atom_chars(args+0, pPlaybackInfos[i-1].name);
        PL_put_atom_chars(args+1, "playback");
        PL_put_atom_chars(args+2, pPlaybackInfos[i-1].isDefault ? "true" : "false");

        if (!PL_cons_functor_v(device, device_functor, args)) {
            return FALSE;
        }
        if (!PL_cons_list(list, device, list)) {
            return FALSE;
        }
    }

    /* Unify with output */
    return PL_unify(devices, list);
}

/*
 * sampler_sound_start(+Handle)
 * Starts playing a sound
 */
static foreign_t pl_sampler_sound_start(term_t handle)
{
	ma_sound* sound;
	ma_result result;
	int slot;
	effect_node_t* effect;

	GET_SOUND_WITH_SLOT(handle, sound, slot);

	/* reset all envelope nodes in effect chain */
	effect = g_sounds[slot].effect_chain;
	while (effect != NULL) {
		if (effect->type == EFFECT_ENVELOPE) {
			adbr_envelope_node_t* envelope = (adbr_envelope_node_t*)effect->effect_node;
			envelope->stage = 0;
			envelope->stage_progress = 0.0f;
		}
		effect = effect->next;
	}

	result = ma_sound_start(sound);
	return (result == MA_SUCCESS) ? TRUE : FALSE;
}

/*
 * sampler_sound_stop(+Handle)
 * Stops playing a sound
 */
static foreign_t pl_sampler_sound_stop(term_t handle)
{
	ma_sound* sound;
	ma_result result;

	GET_SOUND_FROM_HANDLE(handle, sound);

	result = ma_sound_stop(sound);
	return (result == MA_SUCCESS) ? TRUE : FALSE;
}

/*
 * sampler_sound_is_playing(+Handle)
 * Succeeds if sound is playing, fails otherwise.
 */
static foreign_t pl_sampler_sound_is_playing(term_t handle)
{
	ma_sound* sound;

	GET_SOUND_FROM_HANDLE(handle, sound);

	return ma_sound_is_playing(sound) ? TRUE : FALSE;
}

/*
 * sampler_sound_set_looping(+Handle, +Loop)
 * Sets whether a sound should loop
 */
static foreign_t pl_sampler_sound_set_looping(term_t handle, term_t loop)
{
	ma_sound* sound;
	char* loop_str;
	ma_bool32 should_loop;

	GET_SOUND_FROM_HANDLE(handle, sound);

	if (!PL_get_chars(loop, &loop_str, CVT_ATOM | CVT_STRING | CVT_EXCEPTION)) {
		return FALSE;
	}

	should_loop = (strcmp(loop_str, "true") == 0) ? MA_TRUE : MA_FALSE;
	ma_sound_set_looping(sound, should_loop);

	return TRUE;
}

/*
 * sampler_sound_is_looping(+Handle)
 * Succeeds if sound is set to loop, fails otherwise.
 */
static foreign_t pl_sampler_sound_is_looping(term_t handle)
{
	ma_sound* sound;

	GET_SOUND_FROM_HANDLE(handle, sound);

	return ma_sound_is_looping(sound) ? TRUE : FALSE;
}

/*
 * sampler_sound_seek(+Handle, +Frame)
 * Seeks to a specific frame position in the sound.
 */
static foreign_t pl_sampler_sound_seek(term_t handle, term_t frame)
{
	ma_sound* sound;
	ma_uint64 frame_index;
	ma_result result;

	GET_SOUND_FROM_HANDLE(handle, sound);

	if (!PL_get_uint64(frame, &frame_index)) {
		return PL_type_error("integer", frame);
	}

	result = ma_sound_seek_to_pcm_frame(sound, frame_index);
	return (result == MA_SUCCESS) ? TRUE : FALSE;
}

/*
 * sampler_sound_get_position(+Handle, -Frame)
 * Gets the current playback position in frames.
 */
static foreign_t pl_sampler_sound_get_position(term_t handle, term_t frame) {
	ma_sound* sound;
	ma_uint64 cursor;
	ma_result result;

	GET_SOUND_FROM_HANDLE(handle, sound);

	result = ma_sound_get_cursor_in_pcm_frames(sound, &cursor);
	if (result != MA_SUCCESS) {
		return FALSE;
	}

	return PL_unify_uint64(frame, cursor);
}


/* 
 * sampler_sound_create(+DataHandle, -SoundHandle)
 * Creates a sound instance from a loaded data buffer.
 * Multiple sounds can be created from the same buffer for polyphony.
 */
static foreign_t pl_sampler_sound_create(term_t data_handle, term_t sound_handle) 
{
	int data_slot;
	int sound_slot;
	ma_audio_buffer* source_buffer;
	ma_audio_buffer* sound_buffer;
	ma_sound* sound;
	ma_result result;
	ma_audio_buffer_config buffer_config;
	ma_uint64 frames;
	ma_uint32 channels;
	ma_uint32 sample_rate;

	if (!PL_get_integer(data_handle, &data_slot)) {
		return PL_type_error("integer", data_handle);
	}

	source_buffer = get_data_buffer(data_slot);
	if (source_buffer == NULL) {
		return PL_existence_error("data_buffer", data_handle);
	}

	sound_slot = allocate_sound_slot();
	if (sound_slot < 0) {
		return PL_resource_error("sound_slots");
	}

	/* Create a new buffer that shares the PCM data but has independent cursor */
	sound_buffer = (ma_audio_buffer*)malloc(sizeof(ma_audio_buffer));
	if (sound_buffer == NULL) {
		free_sound_slot(sound_slot);
		return PL_resource_error("memory");
	}

	get_buffer_info(source_buffer, &frames, &channels, &sample_rate);
	buffer_config = ma_audio_buffer_config_init(source_buffer->ref.format, channels, frames, g_data_buffers[data_slot].pData, NULL);
	buffer_config.sampleRate = sample_rate;

	result = ma_audio_buffer_init(&buffer_config, sound_buffer);
	if (result != MA_SUCCESS) {
		free(sound_buffer);
		free_sound_slot(sound_slot);
		return FALSE;
	}

	sound = (ma_sound*)malloc(sizeof(ma_sound));
	if (sound == NULL) {
		ma_audio_buffer_uninit(sound_buffer);
		free(sound_buffer);
		free_sound_slot(sound_slot);
		return PL_resource_error("memory");
	}

	result = ma_sound_init_from_data_source(g_engine, sound_buffer, 0, NULL, sound);
	if (result != MA_SUCCESS) {
		ma_audio_buffer_uninit(sound_buffer);
		free(sound_buffer);
		free(sound);
		free_sound_slot(sound_slot);
		return FALSE;
	}

	g_sounds[sound_slot].sound = sound;
	g_sounds[sound_slot].audio_buffer = sound_buffer;
	g_sounds[sound_slot].data_buffer_index = data_slot;
	g_data_buffers[data_slot].refcount++;

	return PL_unify_integer(sound_handle, sound_slot);
}

/*
 * sampler_sound_set_range(+Handle, +StartFrame, +EndFrame)
 * Sets the playback range for a sound (which frames to play).
 */
static foreign_t pl_sampler_sound_set_range(term_t handle, term_t start_term, term_t end_term)
{
	ma_sound* sound;
  	ma_uint64 start_frame;
  	ma_uint64 end_frame;
  	ma_data_source* data_source;
  	ma_result result;
  	int start_int, end_int;

	GET_SOUND_FROM_HANDLE(handle, sound);

	if (!PL_get_integer(start_term, &start_int)) {
  		return PL_type_error("integer", start_term);
  	}

  	if (!PL_get_integer(end_term, &end_int)) {
  		return PL_type_error("integer", end_term);
  	}

  	if (start_int < 0 || end_int < 0) {
  		return PL_domain_error("non_negative_integer", start_int < 0 ? start_term : end_term);
  	}

  	if (start_int >= end_int) {
  		return PL_domain_error("valid_range", start_term);
  	}

	start_frame = (ma_uint64)start_int;
	end_frame = (ma_uint64)end_int;

	data_source = ma_sound_get_data_source(sound);
	result = ma_data_source_set_range_in_pcm_frames(data_source, start_frame, end_frame);

	if (result != MA_SUCCESS) {
		return FALSE;
	}

	return TRUE;
}

static foreign_t pl_sampler_data_info(term_t data_handle, term_t info)
{
	ma_audio_buffer* buffer;
	ma_uint64 frames;
	ma_uint32 channels;
	ma_uint32 sampleRate;
	double duration;
	term_t args = PL_new_term_refs(4);
	functor_t info_functor;

	GET_DATA_BUFFER_FROM_HANDLE(data_handle, buffer);

	get_buffer_info(buffer, &frames, &channels, &sampleRate);
	duration = (double)frames / (double)sampleRate;

	if (!PL_put_uint64(args + 0, frames)) return FALSE;
	if (!PL_put_integer(args + 1, channels)) return FALSE;
	if (!PL_put_integer(args + 2, sampleRate)) return FALSE;
	if (!PL_put_float(args + 3, duration)) return FALSE;

	term_t result = PL_new_term_ref();
	info_functor = PL_new_functor(PL_new_atom("data_info"), 4);
	if (!PL_cons_functor_v(result, info_functor, args)) {
		return FALSE;
	}
	return PL_unify(info, result);
}

/*
 * sampler_sound_length(+Handle, -Frames)
 * Gets the total length of a sound in PCM frames.
 */
static foreign_t pl_sampler_sound_length(term_t handle, term_t frames)
{
	ma_sound* sound;
	ma_uint64 length;
	ma_result result;

	GET_SOUND_FROM_HANDLE(handle, sound);

	result = ma_sound_get_length_in_pcm_frames(sound, &length);
	if (result != MA_SUCCESS) {
		return FALSE;
	}

	return PL_unify_uint64(frames, length);
}

/*
 * sampler_sound_set_pitch(+Handle, +Pitch)
 * Sets the pitch in semitones (12 = one octave up, -12 = one octave down).
 */
static foreign_t pl_sampler_sound_set_pitch(term_t handle, term_t pitch)
{
	ma_sound* sound;
	double pitch_value;
	float ratio;

	GET_SOUND_FROM_HANDLE(handle, sound);

	if (!PL_get_float(pitch, &pitch_value)) {
		return PL_type_error("float", pitch);
	}

	ratio = powf(2.0f, (float)pitch_value / 12.0f);
	ma_sound_set_pitch(sound, ratio);
	return TRUE;
}

/*
 * sampler_sound_get_pitch(+Handle, -Pitch)
 * Gets pitch in semitones.
 */
static foreign_t pl_sampler_sound_get_pitch(term_t handle, term_t pitch)
{
	ma_sound* sound;
	float ratio;
	double semitones;

	GET_SOUND_FROM_HANDLE(handle, sound);

	ratio = ma_sound_get_pitch(sound);
	semitones = 12.0 * log2(ratio);

	return PL_unify_float(pitch, semitones);
}

/*
 * sampler_sound_set_pan_mode(+Handle, +Mode)
 * Sets the pan mode: "balance" or "pan"
 */
static foreign_t pl_sampler_sound_set_pan_mode(term_t handle, term_t mode)
{
	ma_sound* sound;
	char* mode_str;
	ma_pan_mode pan_mode;

	GET_SOUND_FROM_HANDLE(handle, sound);

	if (!PL_get_chars(mode, &mode_str, CVT_ATOM | CVT_STRING | CVT_EXCEPTION)) {
		return FALSE;
	}

	if (strcmp(mode_str, "balance") == 0) {
		pan_mode = ma_pan_mode_balance;
	} else if (strcmp(mode_str, "pan") == 0) {
		pan_mode = ma_pan_mode_pan;
	} else {
		return PL_domain_error("pan_mode", mode);
	}

	ma_sound_set_pan_mode(sound, pan_mode);
	return TRUE;
}

/*
 * sampler_sound_get_pan_mode(+Handle, -Mode)
 * Gets the current pan mode as 'balance' or 'pan'.
 */
static foreign_t pl_sampler_sound_get_pan_mode(term_t handle, term_t mode)
{
	ma_sound* sound;
	ma_pan_mode pan_mode;
	const char* mode_str;

	GET_SOUND_FROM_HANDLE(handle, sound);

	pan_mode = ma_sound_get_pan_mode(sound);

	if (pan_mode == ma_pan_mode_balance) {
		mode_str = "balance";
	} else {
		mode_str = "pan";
	}

	return PL_unify_atom_chars(mode, mode_str);
}

/*
 * sampler_sound_net_pan(+Handle, +Pan)
 * Sets stereo pan (-1.0 = hard left, 0.0 = center, 1.0 = right).
 */
static foreign_t pl_sampler_sound_set_pan(term_t handle, term_t pan)
{
	ma_sound* sound;
	double pan_value;

	GET_SOUND_FROM_HANDLE(handle, sound);

	if (!PL_get_float(pan, &pan_value)) {
		return PL_type_error("float", pan);
	}

	ma_sound_set_pan(sound, (float)pan_value);
	return TRUE;
}

/*
 * sampler_sound_get_pan(+Handle, -Pan)
 * Gets current stereo pan value
 */
static foreign_t pl_sampler_sound_get_pan(term_t handle, term_t pan)
{
	ma_sound* sound;
	double pan_value;

	GET_SOUND_FROM_HANDLE(handle, sound);

	pan_value = ma_sound_get_pan(sound);

	return PL_unify_float(pan, (double)pan_value);
}

/*
 * sampler_sound_set_volume(+Handle, +Volume)
 * Sets volume (1.0 = normal, 0.0 = silence, >1.0 = amplification).
 */
static foreign_t pl_sampler_sound_set_volume(term_t handle, term_t volume)
{
	ma_sound* sound;
	double volume_value;

	GET_SOUND_FROM_HANDLE(handle, sound);

	if (!PL_get_float(volume, &volume_value)) {
		return PL_type_error("float", volume);
	}

	ma_sound_set_volume(sound, (float)volume_value);
	return TRUE;
}

/*
 * sampler_sound_get_volume(+Handle, -Volume)
 * Gets current volume.
 */
static foreign_t pl_sampler_sound_get_volume(term_t handle, term_t volume)
{
	ma_sound* sound;
	float volume_value;

	GET_SOUND_FROM_HANDLE(handle, sound);

	volume_value = ma_sound_get_volume(sound);

	return PL_unify_float(volume, (double)volume_value);
}

/*
 * sampler_data_reverse(+SourceHandle, -ReversedHandle)
 * Creates a reversed copy of a data buffer
 */
static foreign_t pl_sampler_data_reverse(term_t source_handle, term_t reversed_handle)
{
	ma_audio_buffer* source_buffer;
	int slot;
	ma_uint64 frame_count;
	ma_uint32 channels;
	ma_uint32 sample_rate;
	ma_format format;
	ma_uint32 bytes_per_frame;
	void* source_data;
	void* reversed_data;
	ma_uint64 i;
	int source_slot;

	GET_DATA_BUFFER_WITH_SLOT(source_handle, source_buffer, source_slot);

	get_buffer_info(source_buffer, &frame_count, &channels, &sample_rate);
	format = source_buffer->ref.format;
	bytes_per_frame = ma_get_bytes_per_frame(format, channels);

	reversed_data = malloc(frame_count * bytes_per_frame);
	if (reversed_data == NULL) {
		return PL_resource_error("memory");
	}

	/* Reverse frame order while preserving channel interleaving within each frame.
	 * Frame i in source becomes frame (frame_count - 1 - i) in destination.
	 * e.g., stereo: [L0,R0, L1,R1, L2,R2] becomes [L2,R2, L1,R1, L0,R0] */
	source_data = g_data_buffers[source_slot].pData;
	for (i = 0; i < frame_count; i++) {
		memcpy(
			(char*)reversed_data + ((frame_count - 1 - i) * bytes_per_frame),
			(char*)source_data + (i * bytes_per_frame),
			bytes_per_frame
		);
	}

	slot = create_data_buffer_from_pcm(reversed_data, format, channels, frame_count, sample_rate);
	if (slot < 0) {
		free(reversed_data);
		return PL_resource_error("data_buffer_slots");
	}

	return PL_unify_integer(reversed_handle, slot);
}

/* Effect management helpers */

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

/* helper: add effect node to chain and connect to node graph */
static ma_result attach_effect_node_to_sound(sound_slot_t* sound_slot, ma_node_base* effect_node, effect_type_t type)
{
	effect_node_t* new_effect;
  	effect_node_t* tail;
  	ma_node* sound_node;
  	ma_node* endpoint;
  	ma_result result;

	new_effect = (effect_node_t*)malloc(sizeof(effect_node_t));
	if (new_effect == NULL) {
		return MA_OUT_OF_MEMORY;
	}

	new_effect->type = type;
	new_effect->effect_node = effect_node;
	new_effect->next = NULL;

	sound_node = (ma_node*)sound_slot->sound;
	endpoint = ma_node_graph_get_endpoint(ma_engine_get_node_graph(g_engine));

	/* if no effects yet, attach sound -> effect -> endpoint */
	if (sound_slot->effect_chain == NULL) {
		result = ma_node_attach_output_bus(sound_node, 0, (ma_node*)effect_node, 0);
		if (result != MA_SUCCESS) {
			free(new_effect);
			return result;
		}

		result = ma_node_attach_output_bus((ma_node*)effect_node, 0, endpoint, 0);
		if (result != MA_SUCCESS) {
			ma_node_detach_output_bus(sound_node, 0);
			ma_node_attach_output_bus(sound_node, 0, endpoint, 0);
			free(new_effect);
			return result;
		}

		sound_slot->effect_chain = new_effect;
	} else {
		/* find tail and reconnect: sound -> ... existing ... -> new_effect -> endpoint */
		tail = sound_slot->effect_chain;
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

/* Helper: initialize effect node base (common boilerplate) */
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

/* helper: initialize ADBR envelope node */
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

/* complete envelope attachment - validate, init, attach */
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
/* Helper: initialize bitcrush effect node */
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

/* Complete bitcrush attachment - validate, init, attach */
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

/* sampler_sound_attach_effect(+SoundHandle, +EffectType, +Params, -EffectHandle)
 * Attach an effect to a sound's processing chain.
 * EffectType is an atom (bitcrush, envelope, etc.)
 * Params is a list of key=value pairs.
 * Returns effect(SoundHandle, EffectPointer)
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
 * pl_sampler_sound_effects(+SoundHandle, -Effects)
 * Query all effects attached to a sound.
 * Returns list of effect type atoms.
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

			if (effect->type == EFFECT_BITCRUSH) {
				type_str = "bitcrush";
				bitcrush_node_t* bitcrush = (bitcrush_node_t*)effect->effect_node;

				term_t param_args = PL_new_term_refs(2);
				functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);
				term_t param_term = PL_new_term_ref();

				PL_put_atom_chars(param_args+0, "sample_rate");
				if (!PL_put_integer(param_args+1, bitcrush->target_sample_rate)) {
					free(effects_array);
					return FALSE;
				}
				if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
					free(effects_array);
					return FALSE;
				}
				if (!PL_cons_list(params_list, param_term, params_list)) {
					free(effects_array);
					return FALSE;
				}

				PL_put_atom_chars(param_args+0, "bits");
				if (!PL_put_integer(param_args+1, bitcrush->target_bits)) {
					free(effects_array);
					return FALSE;
				}
				if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
					free(effects_array);
					return FALSE;
				}
				if (!PL_cons_list(params_list, param_term, params_list)) {
					free(effects_array);
					return FALSE;
				}

			} else if (effect->type == EFFECT_ENVELOPE) {
				type_str = "envelope";
				adbr_envelope_node_t* envelope = (adbr_envelope_node_t*)effect->effect_node;

				term_t param_args = PL_new_term_refs(2);
				functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);
				term_t param_term = PL_new_term_ref();

				PL_put_atom_chars(param_args+0, "loop");
				if (!PL_put_integer(param_args+1, envelope->loop)) {
					free(effects_array);
					return FALSE;
				}
				if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
					free(effects_array);
					return FALSE;
				}
				if (!PL_cons_list(params_list, param_term, params_list)) {
					free(effects_array);
					return FALSE;
				}

				PL_put_atom_chars(param_args+0, "duration_ms");
				if (!PL_put_float(param_args+1, envelope->duration_ms)) {
					free(effects_array);
					return FALSE;
				}
				if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
					free(effects_array);
					return FALSE;
				}
				if (!PL_cons_list(params_list, param_term, params_list)) {
					free(effects_array);
					return FALSE;
				}

				PL_put_atom_chars(param_args+0, "break_level");
				if (!PL_put_float(param_args+1, envelope->break_level)) {
					free(effects_array);
					return FALSE;
				}
				if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
					free(effects_array);
					return FALSE;
				}
				if (!PL_cons_list(params_list, param_term, params_list)) {
					free(effects_array);
					return FALSE;
				}

				PL_put_atom_chars(param_args+0, "break");
				if (!PL_put_float(param_args+1, envelope->Break)) {
					free(effects_array);
					return FALSE;
				}
				if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
					free(effects_array);
					return FALSE;
				}
				if (!PL_cons_list(params_list, param_term, params_list)) {
					free(effects_array);
					return FALSE;
				}

				PL_put_atom_chars(param_args+0, "decay");
				if (!PL_put_float(param_args+1, envelope->decay)) {
					free(effects_array);
					return FALSE;
				}
				if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
					free(effects_array);
					return FALSE;
				}
				if (!PL_cons_list(params_list, param_term, params_list)) {
					free(effects_array);
					return FALSE;
				}

				PL_put_atom_chars(param_args+0, "attack");
				if (!PL_put_float(param_args+1, envelope->attack)) {
					free(effects_array);
					return FALSE;
				}
				if (!PL_cons_functor_v(param_term, eq_functor, param_args)) {
					free(effects_array);
					return FALSE;
				}
				if (!PL_cons_list(params_list, param_term, params_list)) {
					free(effects_array);
					return FALSE;
				}

			} else {
				type_str = "unknown";
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

/*
 * sampler_effect_set_parameters(+EffectHandle, +Parameters)
 * Sets parameters on an effect.
 * EffectHandle is effect(SoundHandle, EffectPointer)
 * Parameters is a list of key=value pairs
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

	/* Iterate over parameter list */
	term_t list = PL_copy_term_ref(params_list);
	term_t head = PL_new_term_ref();
	functor_t eq_functor = PL_new_functor(PL_new_atom("="), 2);

	while (PL_get_list(list, head, list)) {
		term_t key_term = PL_new_term_ref();
		term_t value_term = PL_new_term_ref();
		char* param_name;

		/* Extract key=value */
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

		/* Set parameter based on effect type */
		if (node->type == EFFECT_BITCRUSH) {
			bitcrush_node_t* bitcrush = (bitcrush_node_t*)node->effect_node;

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
		} else if (node->type == EFFECT_ENVELOPE) {
			adbr_envelope_node_t* envelope = (adbr_envelope_node_t*)node->effect_node;

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
		} else {
			return PL_domain_error("effect_type", effect_handle);
		}
	}

	if (!PL_get_nil(list)) {
		return PL_type_error("list", params_list);
	}

	return TRUE;
}

/*
 * sampler_effect_detach(+EffectHandle)
 * Removes an effect from the sound's effect chain.
 * EffectHandle is effect(SoundHandle, EffectPointer)
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
 * install()
 * Register foreign predicates with SWI-Prolog.
 */
install_t install(void)
{
    PL_register_foreign("sampler_version", 1, pl_sampler_version, 0);
    PL_register_foreign("sampler_init", 0, pl_sampler_init, 0);
    PL_register_foreign("sampler_devices", 1, pl_sampler_devices, 0);
    PL_register_foreign("sampler_sound_unload", 1, pl_sampler_sound_unload, 0);
	PL_register_foreign("sampler_sound_start", 1, pl_sampler_sound_start, 0);
	PL_register_foreign("sampler_sound_stop", 1, pl_sampler_sound_stop, 0);
	PL_register_foreign("sampler_sound_is_playing", 1, pl_sampler_sound_is_playing, 0);
	PL_register_foreign("sampler_sound_set_looping", 2, pl_sampler_sound_set_looping, 0);
	PL_register_foreign("sampler_sound_is_looping", 1, pl_sampler_sound_is_looping, 0);
	PL_register_foreign("sampler_data_load", 2, pl_sampler_data_load, 0);
	PL_register_foreign("sampler_data_unload", 1, pl_sampler_data_unload, 0);
	PL_register_foreign("sampler_sound_create", 2, pl_sampler_sound_create, 0);
	PL_register_foreign("sampler_sound_seek", 2, pl_sampler_sound_seek, 0);
	PL_register_foreign("sampler_sound_get_position", 2, pl_sampler_sound_get_position, 0);
	PL_register_foreign("sampler_data_info", 2, pl_sampler_data_info, 0);
	PL_register_foreign("sampler_sound_length", 2, pl_sampler_sound_length, 0);
	PL_register_foreign("sampler_sound_set_pitch", 2, pl_sampler_sound_set_pitch, 0);
	PL_register_foreign("sampler_sound_get_pitch", 2, pl_sampler_sound_get_pitch, 0);
	PL_register_foreign("sampler_sound_set_pan", 2, pl_sampler_sound_set_pan, 0);
	PL_register_foreign("sampler_sound_get_pan", 2, pl_sampler_sound_get_pan, 0);
	PL_register_foreign("sampler_sound_set_pan_mode", 2, pl_sampler_sound_set_pan_mode, 0);
	PL_register_foreign("sampler_sound_get_pan_mode", 2, pl_sampler_sound_get_pan_mode, 0);
	PL_register_foreign("sampler_sound_set_volume", 2, pl_sampler_sound_set_volume, 0);
	PL_register_foreign("sampler_sound_get_volume", 2, pl_sampler_sound_get_volume, 0);
	PL_register_foreign("sampler_data_reverse", 2, pl_sampler_data_reverse, 0);
	PL_register_foreign("sampler_sound_set_range", 3, pl_sampler_sound_set_range, 0);
	PL_register_foreign("sampler_data_extract", 4, pl_sampler_data_extract, 0);
	PL_register_foreign("sampler_sound_attach_effect", 4, pl_sampler_sound_attach_effect, 0);
	PL_register_foreign("sampler_sound_effects", 2, pl_sampler_sound_effects, 0);
	PL_register_foreign("sampler_effect_set_parameters", 2, pl_sampler_effect_set_parameters, 0);
	PL_register_foreign("sampler_effect_detach", 1, pl_sampler_effect_detach, 0);
}

/*
 * uninstall_sampler()
 * Called when the foreign library is unloaded.
 * Cleans up the engine.
 */
install_t uninstall_sampler(void)
{
    int i;

	/* Clean up all data buffers */
	for (i = 0; i < MAX_DATA_BUFFERS; i++) {
		if (g_data_buffers[i].in_use) {
			free_data_slot(i);
		}
	}

    /* Clean up all sound slots */
    for (i = 0; i < MAX_SOUNDS; i++) {
        if (g_sounds[i].in_use) {
            free_sound_slot(i);
        }
    }

    /* Clean up engine */
    if (g_engine != NULL) {
        ma_engine_uninit(g_engine);
        free(g_engine);
        g_engine = NULL;
    }
}
