/*
 * sampler.c - Prolog interface to miniaudio (engine API)
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#include <stdlib.h>
#include <string.h>

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

/* sampler_data_load(+FilePath, -Handle)
 * Loads audio data from file into a shareable buffer.
 * Returns a data handle that can be used to create multiple sounds.
 */
static foreign_t pl_sampler_data_load(term_t filepath, term_t handle)
{
	char* path;
	int slot;
	ma_uint64 frameCount;
	void* pData;

	ma_decoder_config decoderConfig;
	ma_format format;
	ma_uint32 channels;
	ma_uint32 sampleRate;
	ma_result result;
	ma_audio_buffer_config bufferConfig;
	ma_audio_buffer* buffer;

	ENSURE_ENGINE_INITIALIZED();

	/* Get file path string */
	if (!PL_get_chars(filepath, &path, CVT_ATOM | CVT_STRING | CVT_EXCEPTION)) {
		return FALSE;
	}

	/* Allocate data slot */
	slot = allocate_data_slot();
	if (slot < 0) {
		return PL_resource_error("data_buffer_slots");
	}

	/* Get engine's format for compatibility */
	get_engine_format_info(&format, &channels, &sampleRate);

	/* Configure decoder to match engine format */
	decoderConfig = ma_decoder_config_init(format, channels, sampleRate);

	/* Decode audio file */
	result = ma_decode_file(path, &decoderConfig, &frameCount, &pData);
	if (result != MA_SUCCESS) {
		free_data_slot(slot);
		return FALSE;
	}

	/* Allocate audio buffer */
	buffer = (ma_audio_buffer*)malloc(sizeof(ma_audio_buffer));
	if (buffer == NULL) {
		ma_free(pData, NULL);
		free_data_slot(slot);
		return PL_resource_error("memory");
	}

	/* Initialize audio buffer */
	bufferConfig = ma_audio_buffer_config_init(format, channels, frameCount, pData, NULL);
	result = ma_audio_buffer_init(&bufferConfig, buffer);
	if (result != MA_SUCCESS) {
		ma_free(pData, NULL);
		free(buffer);
		free_data_slot(slot);
		return FALSE;
	}

	/* Store buffer and data, set initial refcount to 1 */
	g_data_buffers[slot].buffer = buffer;
	g_data_buffers[slot].pData = pData;
	g_data_buffers[slot].refcount = 1;

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
	int slot;
	ma_sound* sound;
	ma_result result;

	if (!PL_get_integer(handle, &slot)) {
		return PL_type_error("integer", handle);
	}

	sound = get_sound(slot);
	if (sound == NULL) {
		return PL_existence_error("sound", handle);
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
	int slot;
	ma_sound* sound;
	ma_result result;

	if (!PL_get_integer(handle, &slot)) {
		return PL_type_error("integer", handle);
	}

	sound = get_sound(slot);
	if (sound == NULL) {
		return PL_existence_error("sound", handle);
	}

	result = ma_sound_stop(sound);
	return (result == MA_SUCCESS) ? TRUE : FALSE;
}

/*
 * sampler_sound_is_playing(+Handle)
 * Succeeds if sound is playing, fails otherwise.
 */
static foreign_t pl_sampler_sound_is_playing(term_t handle)
{
	int slot;
	ma_sound* sound;

	if (!PL_get_integer(handle, &slot)) {
		return PL_type_error("integer", handle);
	}

	sound = get_sound(slot);
	if (sound == NULL) {
		return PL_existence_error("sound", handle);
	}

	return ma_sound_is_playing(sound) ? TRUE : FALSE;
}

/*
 * sampler_sound_set_looping(+Handle, +Loop)
 * Sets whether a sound should loop
 */
static foreign_t pl_sampler_sound_set_looping(term_t handle, term_t loop)
{
	int slot;
	ma_sound* sound;
	char* loop_str;
	ma_bool32 should_loop;

	if (!PL_get_integer(handle, &slot)) {
		return PL_type_error("integer", handle);
	}

	sound = get_sound(slot);
	if (sound == NULL) {
		return PL_existence_error("sound", handle);
	}
	
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
	int slot;
	ma_sound* sound;

	if (!PL_get_integer(handle, &slot)) {
		return PL_type_error("integer", handle);
	}

	sound = get_sound(slot);
	if (sound == NULL) {
		return PL_existence_error("sound", handle);
	}

	return ma_sound_is_looping(sound) ? TRUE : FALSE;
}

/*
 * sampler_sound_seek(+Handle, +Frame)
 * Seeks to a specific frame position in the sound.
 */
static foreign_t pl_sampler_sound_seek(term_t handle, term_t frame)
{
	int slot;
	ma_sound* sound;
	ma_uint64 frame_index;
	ma_result result;

	if (!PL_get_integer(handle, &slot)) {
		return PL_type_error("integer", handle);
	}

	sound = get_sound(slot);
	if (sound == NULL) {
		return PL_existence_error("sound", handle);
	}

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
	int slot;
	ma_sound* sound;
	ma_uint64 cursor;
	ma_result result;

	if (!PL_get_integer(handle, &slot)) {
		return PL_type_error("integer", handle);
	}

	sound = get_sound(slot);
	if (sound == NULL) {
		return PL_existence_error("sound", handle);
	}

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
	ma_audio_buffer* buffer;
	ma_sound* sound;
	ma_result result;

	if (!PL_get_integer(data_handle, &data_slot)) {
		return PL_type_error("integer", data_handle);
	}

	buffer = get_data_buffer(data_slot);
	if (buffer == NULL) {
		return PL_existence_error("data_buffer", data_handle);
	}

	sound_slot = allocate_sound_slot();
	if (sound_slot < 0) {
		return PL_resource_error("sound_slots");
	}

	sound = (ma_sound*)malloc(sizeof(ma_sound));
	if (sound == NULL) {
		free_sound_slot(sound_slot);
		return PL_resource_error("memory");
	}

	result = ma_sound_init_from_data_source(g_engine, buffer, 0, NULL, sound);
	if (result != MA_SUCCESS) {
		free(sound);
		free_sound_slot(sound_slot);
		return FALSE;
	}

	g_sounds[sound_slot].sound = sound;
	g_sounds[sound_slot].data_buffer_index = data_slot;
	g_data_buffers[data_slot].refcount++;

	return PL_unify_integer(sound_handle, sound_slot);
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
