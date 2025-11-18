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
 * Global engine - one engine for the library lifetime
 */
static ma_engine* g_engine = NULL;

/*
 * Sound handle management
 */
#define MAX_SOUNDS 1024
#define MAX_DATA_BUFFERS 32

typedef struct {
    ma_sound* sound;
    int data_buffer_index; /* -1 if from file, else index into g_data_buffers */
	ma_bool32 in_use;
} sound_slot_t;


static sound_slot_t g_sounds[MAX_SOUNDS] = {{NULL, -1, MA_FALSE}};

typedef struct {
	ma_audio_buffer* buffer;
	void *pData;
	ma_uint32 refcount;
	ma_bool32 in_use;
} data_slot_t;

static data_slot_t g_data_buffers[MAX_DATA_BUFFERS] = {{NULL, NULL, 0, MA_FALSE}};

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
 * sampler_sound_load(+FilePath, -Handle)
 * Loads a sound from a file and returns a handle.
 */
static foreign_t pl_sampler_sound_load(term_t filepath, term_t handle)
{
    char* path;
    ma_sound* sound;
    ma_result result;
    int slot;

    ENSURE_ENGINE_INITIALIZED();

    /* Get file path string */
    if (!PL_get_chars(filepath, &path, CVT_ATOM|CVT_STRING|CVT_EXCEPTION)) {
        return FALSE;
    }

    /* Allocate sound slot */
    slot = allocate_sound_slot();
    if (slot < 0) {
        return PL_resource_error("sound_slots");
    }

    /* Allocate sound */
    sound = (ma_sound*)malloc(sizeof(ma_sound));
    if (sound == NULL) {
        free_sound_slot(slot);
        return PL_resource_error("memory");
    }

    /* Load sound from file */
    result = ma_sound_init_from_file(g_engine, path, 0, NULL, NULL, sound);
    if (result != MA_SUCCESS) {
        free(sound);
        free_sound_slot(slot);
        return FALSE;
    }

    /* Store sound and return handle */
    g_sounds[slot].sound = sound;
    return PL_unify_integer(handle, slot);
}

/*
 * sampler_sound_unload(+Handle)
 * Unloads a sound and frees its resources.
 */
static foreign_t pl_sampler_sound_unload(term_t handle)
{
    int slot;

    /* Get handle */
    if (!PL_get_integer(handle, &slot)) {
        return PL_type_error("integer", handle);
    }

    /* Validate handle */
    if (slot < 0 || slot >= MAX_SOUNDS || !g_sounds[slot].in_use) {
        return PL_existence_error("sound", handle);
    }

    /* Free sound slot */
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

	GET_SOUND_FROM_HANDLE(handle, sound);

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
	g_sounds[sound_slot].data_buffer_index = data_slot;
	g_data_buffers[data_slot].refcount++;

	return PL_unify_integer(sound_handle, sound_slot);
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

/*
 * sempler_data_resample(+SourceHandle, +NewSampleRate, -ResampleHandle)
 * Resamples audio data to a different sample rate.
 * Creates a new data buffer with the resampled audio.
 */
static foreign_t pl_sampler_data_resample(term_t source_handle, term_t new_rate_term, term_t resampled_handle)
{
	ma_audio_buffer* source_buffer;
  	int slot;
  	ma_uint64 old_frame_count;
  	ma_uint64 new_frame_count;
  	ma_uint32 channels;
  	ma_uint32 old_sample_rate;
  	ma_uint32 new_sample_rate;
  	ma_format format;
  	void* source_data;
  	void* resampled_data;
  	int source_slot;
  	ma_resampler_config resampler_config;
  	ma_resampler resampler;
  	ma_result result;
  	ma_uint64 frames_in;
  	ma_uint64 frames_out;

	GET_DATA_BUFFER_WITH_SLOT(source_handle, source_buffer, source_slot);

	if (!PL_get_integer(new_rate_term, (int*)&new_sample_rate)) {
		return PL_type_error("integer", new_rate_term);
	}

	if (new_sample_rate <= 0) {
		return PL_domain_error("positive_integer", new_rate_term);
	}

	get_buffer_info(source_buffer, &old_frame_count, &channels, &old_sample_rate);
	format = source_buffer->ref.format;

	new_frame_count = (ma_uint64)((double)old_frame_count * new_sample_rate / old_sample_rate);

	resampler_config = ma_resampler_config_init(format, channels, old_sample_rate, new_sample_rate, ma_resample_algorithm_linear);
	result = ma_resampler_init(&resampler_config, NULL, &resampler);
	if (result != MA_SUCCESS) {
		return FALSE;
	}

	resampled_data = malloc(new_frame_count * ma_get_bytes_per_frame(format, channels));
	if (resampled_data == NULL) {
  		ma_resampler_uninit(&resampler, NULL);
  		return PL_resource_error("memory");
  	}

	source_data = g_data_buffers[source_slot].pData;
	frames_in = old_frame_count;
	frames_out = new_frame_count;
	result = ma_resampler_process_pcm_frames(&resampler, source_data, &frames_in, resampled_data, &frames_out);

	ma_resampler_uninit(&resampler, NULL);

	if (result != MA_SUCCESS) {
  		free(resampled_data);
  		return FALSE;
  	}

	slot = create_data_buffer_from_pcm(resampled_data, format, channels, frames_out, new_sample_rate);
	if (slot < 0) {
  		free(resampled_data);
  		return PL_resource_error("data_buffer_slots");
  	}

	return PL_unify_integer(resampled_handle, slot);
}

/*
 * sampler_data_bit_reduce(+SourceHandle, +TargetBits, -ReducedHandle)
 * Reduces bit depth by quantizing samples to fewer discrete levels.
 * Simulates lower bit depth while maintaining the same storage format.
 */
static foreign_t pl_sampler_data_bit_reduce(term_t source_handle, term_t bits_term, term_t reduced_handle)
{
	ma_audio_buffer* source_buffer;
	int source_slot;
	int target_bits;
	ma_uint64 frame_count;
	ma_uint32 channels;
	ma_uint32 sample_rate;
	ma_format format;
	ma_uint64 sample_count;
	void* source_data;
	void* reduced_data;
	ma_uint32 bytes_per_sample;
	int slot;
	ma_uint64 i;
	float levels;
	float sample;
	float* src_f32;
	float* dst_f32;
	ma_int16* src_s16;
	ma_int16* dst_s16;
	ma_int16 levels_s16;
	ma_int16 sample_s16;

	GET_DATA_BUFFER_WITH_SLOT(source_handle, source_buffer, source_slot);

	if (!PL_get_integer(bits_term, &target_bits)) {
		return PL_type_error("integer", bits_term);
	}

	if (target_bits < 1 || target_bits > 16) {
		return PL_domain_error("bit_depth_range", bits_term);
	}

	get_buffer_info(source_buffer, &frame_count, &channels, &sample_rate);
	format = source_buffer->ref.format;
	sample_count = frame_count * channels;
	bytes_per_sample = ma_get_bytes_per_sample(format);

	reduced_data = malloc(sample_count * bytes_per_sample);
	if (reduced_data == NULL) {
		return PL_resource_error("memory");
	}

	source_data = g_data_buffers[source_slot].pData;

	if (format == ma_format_f32) {
		/* For float format (-1.0 to 1.0 range):
		 * levels = 2^(target_bits-1) quantization steps (e.g., 4-bit = 8 levels)
		 * Quantization: scale to ±levels, round to integer, scale back to ±1.0
		 * Example (4-bit): 0.75 * 8 = 6.0 -> round(6.0) = 6 -> 6/8 = 0.75
		 *                  0.73 * 8 = 5.84 -> round(5.84) = 6 -> 6/8 = 0.75 */
		levels = (float)(1 << (target_bits - 1));
		src_f32 = (float*)source_data;
		dst_f32 = (float*)reduced_data;

#if defined(__ARM_NEON) || defined(__aarch64__)
		/* NEON-optimized path: process 4 samples at a time */
		float32x4_t levels_vec = vdupq_n_f32(levels);
		float32x4_t inv_levels_vec = vdupq_n_f32(1.0f / levels);
		ma_uint64 vec_count = sample_count / 4;
		ma_uint64 vec_samples = vec_count * 4;

		for (i = 0; i < vec_samples; i += 4) {
			float32x4_t samples = vld1q_f32(&src_f32[i]);
			samples = vmulq_f32(samples, levels_vec);
			samples = vrndnq_f32(samples);
			samples = vmulq_f32(samples, inv_levels_vec);
			vst1q_f32(&dst_f32[i], samples);
		}

		/* Handle remaining samples with scalar code */
		for (i = vec_samples; i < sample_count; i++) {
			sample = src_f32[i];
			dst_f32[i] = roundf(sample * levels) / levels;
		}
#else
		/* Scalar fallback */
		for (i = 0; i < sample_count; i++) {
			sample = src_f32[i];
			dst_f32[i] = roundf(sample * levels) / levels;
		}
#endif
	} else if (format == ma_format_s16) {
		/* For s16 format (-32768 to 32767 range):
		 * levels_s16 = 2^(target_bits-1) quantization steps
		 * step_size = 32768 / levels_s16 (e.g., 4-bit: 32768/8 = 4096)
		 * Quantization: divide by step_size (truncates), multiply back
		 * Example (4-bit): 10000 / 4096 = 2 -> 2 * 4096 = 8192 */
		levels_s16 = (ma_int16)(1 << (target_bits - 1));
		src_s16 = (ma_int16*)source_data;
		dst_s16 = (ma_int16*)reduced_data;

		/* Scalar path for s16 (NEON integer division is complex) */
		for (i = 0; i < sample_count; i++) {
			sample_s16 = src_s16[i];
			dst_s16[i] = (ma_int16)((sample_s16 / (32768 / levels_s16)) * (32768 / levels_s16));
		}
	} else {
		free(reduced_data);
		return PL_domain_error("supported_format", source_handle);
	}

	slot = create_data_buffer_from_pcm(reduced_data, format, channels, frame_count, sample_rate);
	if (slot < 0) {
		free(reduced_data);
		return PL_resource_error("data_buffer_slots");
	}

	return PL_unify_integer(reduced_handle, slot);
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
    PL_register_foreign("sampler_sound_load", 2, pl_sampler_sound_load, 0);
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
	PL_register_foreign("sampler_data_resample", 3, pl_sampler_data_resample, 0);
	PL_register_foreign("sampler_data_bit_reduce", 3, pl_sampler_data_bit_reduce, 0);
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
