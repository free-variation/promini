/*
 * promini.c - Prolog interface to miniaudio (engine API)
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */


#if defined(__ARM_NEON) || defined(__aarch64__)
#include <arm_neon.h>
#endif


#define MINIAUDIO_IMPLEMENTATION
#include "../../include/miniaudio.h"
#include "promini.h"


/******************************************************************************
 * MACROS AND TYPE DEFINITIONS
 *****************************************************************************/

/*
 * Forward declarations
 */

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

#define GET_CAPTURE_DEVICE_FROM_HANDLE(handle_term, capture_var) \
  	do { \
  		int _slot; \
  		if (!PL_get_integer(handle_term, &_slot)) { \
  			return PL_type_error("integer", handle_term); \
  		} \
  		capture_var = get_capture_device(_slot); \
  		if (capture_var == NULL) { \
  			return PL_existence_error("capture_device", handle_term); \
  		} \
  	} while(0)


/******************************************************************************
 * GLOBAL VARIABLES
 *****************************************************************************/

/*
 * Global engine - one engine for the library lifetime
 */
ma_engine* g_engine = NULL;

/*
 * Thread safety - mutexes for protecting global state
 */
pthread_mutex_t g_sounds_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t g_data_buffers_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t g_capture_devices_mutex = PTHREAD_MUTEX_INITIALIZER;

/*
 * Sound handle management
 */


static data_slot_t g_data_buffers[MAX_DATA_BUFFERS] = {{NULL, NULL, 0, MA_FALSE}};

/* Shared sound slots array (declared in promini.h) */
sound_slot_t g_sounds[MAX_SOUNDS] = {{NULL, NULL, -1, MA_FALSE, NULL}};

/* capture device management */
#define MAX_CAPTURE_DEVICES 8

typedef struct {
	ma_device device;
	void* buffer_data;
	ma_uint64 capacity_frames;
	ma_uint64 write_position; /* wraps at capacity */
	ma_bool32 in_use;
} capture_slot_t;

static capture_slot_t g_capture_devices[MAX_CAPTURE_DEVICES] = {{0}};


/******************************************************************************
 * HELPER FUNCTIONS
 *****************************************************************************/

/*
 * allocate_data_slot()
 * Finds a free data slot and marks it as in use.
 * Returns slot index, or -1 if all slots are full.
 * Thread-safe: protected by g_data_buffers_mutex
 */
static int allocate_data_slot(void)
{
	int i;
	int slot = -1;

	pthread_mutex_lock(&g_data_buffers_mutex);
	for (i = 0; i < MAX_DATA_BUFFERS; i++) {
		if (!g_data_buffers[i].in_use) {
			g_data_buffers[i].in_use = MA_TRUE;
			g_data_buffers[i].buffer = NULL;
			g_data_buffers[i].pData = NULL;
			g_data_buffers[i].refcount = 0;
			slot = i;
			break;
		}
	}
	pthread_mutex_unlock(&g_data_buffers_mutex);

	return slot;
}

/*
 * free_data_slot()
 * Frees a data slot and its resources.
 * Thread-safe: protected by g_data_buffers_mutex
 */
static void free_data_slot(int index)
{
	if (index >= 0 && index < MAX_DATA_BUFFERS) {
		pthread_mutex_lock(&g_data_buffers_mutex);

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

		pthread_mutex_unlock(&g_data_buffers_mutex);
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
 * get_data_slot()
 * Validates handle and return a data slot
 */
data_slot_t* get_data_slot(int index)
{
	if (index < 0 || index >= MAX_DATA_BUFFERS || !g_data_buffers[index].in_use) {
		return NULL;
	}
	return &g_data_buffers[index];
}

/*
 * allocate_capture_slot()
 * Finds a free capture slot and marks it as in use.
 * Returns slot index, or -1 if all slots are full.
 * Thread-safe: protected by g_capture_devices_mutex
 */
static int allocate_capture_slot(void)
{
	int i;
	int slot = -1;

	pthread_mutex_lock(&g_capture_devices_mutex);
	for (i = 0; i < MAX_CAPTURE_DEVICES; i++){
		if (!g_capture_devices[i].in_use) {
			g_capture_devices[i].in_use = MA_TRUE;
			g_capture_devices[i].buffer_data = NULL;
			g_capture_devices[i].capacity_frames = 0;
			slot = i;
			break;
		}
	}
	pthread_mutex_unlock(&g_capture_devices_mutex);

	return slot;
}

/*
 * free_capture_slot()
 * Frees a capture slot and its resources.
 * Thread-safe: protected by g_capture_devices_mutex
 */
static void free_capture_slot(int index)
{
	if (index >= 0 && index < MAX_CAPTURE_DEVICES) {
		pthread_mutex_lock(&g_capture_devices_mutex);

		if (g_capture_devices[index].in_use) {
			ma_device_uninit(&g_capture_devices[index].device);
			if (g_capture_devices[index].buffer_data != NULL) {
				ma_free(g_capture_devices[index].buffer_data, NULL);
				g_capture_devices[index].buffer_data = NULL;
			}
			g_capture_devices[index].in_use = MA_FALSE;
		}

		pthread_mutex_unlock(&g_capture_devices_mutex);
	}
}

/*
 * get_capture_device()
 * Validates handle and returns capture slot pointer.
 * Returns NULL if invalid handle.
 */
static capture_slot_t* get_capture_device(int index)
{
	if (index < 0 || index >= MAX_CAPTURE_DEVICES) {
		return NULL;
	}
	if (!g_capture_devices[index].in_use) {
		return NULL;
	}
	return &g_capture_devices[index];
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
void get_engine_format_info(ma_format* format, ma_uint32* channels, ma_uint32* sampleRate)
{
	ma_device* device = ma_engine_get_device(g_engine);
	if (format) *format = device->playback.format;
	if (channels) *channels = device->playback.channels;
	if (sampleRate) *sampleRate = device->sampleRate;
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


/******************************************************************************
 * DATA BUFFER MANAGEMENT
 *****************************************************************************/

/*
 * pl_audio_load()
 * audio_load(+FilePath, -Handle)
 * Loads audio data from file into a shareable buffer.
 * Returns a data handle that can be used to create multiple sounds.
 */
static foreign_t pl_audio_load(term_t filepath, term_t handle)
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
 * audio_extract(+SourceHandle, +StartFrame, +Length, -ExtractedHandle)
 * Extracts a slice of frames into a new buffer.
 */
static foreign_t pl_audio_extract(term_t source_handle, term_t start_term, term_t length_term, term_t extracted_handle)
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
 * audio_unload(+Handle)
 * Decrements reference count on data buffer.
 * Frees the buffer when refcount reaches zero.
 */
static foreign_t pl_audio_unload(term_t handle)
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
 * Thread-safe: protected by g_sounds_mutex
 */
static int allocate_sound_slot(void)
{
    int i;
	int slot = -1;

	pthread_mutex_lock(&g_sounds_mutex);
    for (i = 0; i < MAX_SOUNDS; i++) {
        if (!g_sounds[i].in_use) {
            g_sounds[i].in_use = MA_TRUE;
            g_sounds[i].sound = NULL;
            g_sounds[i].audio_buffer = NULL;
			g_sounds[i].data_buffer_index = -1;
            slot = i;
			break;
        }
    }
	pthread_mutex_unlock(&g_sounds_mutex);

    return slot;
}

/*
 * free_effect_chain()
 * Frees all nodes in an effect chain.
 */
void free_effect_chain(effect_node_t* effect) {
	while (effect != NULL) {
		effect_node_t* next = effect->next;

		ma_node_detach_output_bus(effect->effect_node, 0);

		if (effect->type == EFFECT_BITCRUSH) {
			bitcrush_node_t* bitcrush = (bitcrush_node_t*)effect->effect_node;
			if (bitcrush->hold_samples != NULL) {
				free(bitcrush->hold_samples);
			}
		} else if (effect->type == EFFECT_REVERB) {
			free_reverb_node((reverb_node_t*)effect->effect_node);
		}

		ma_node_uninit(effect->effect_node, NULL);
		free(effect->effect_node);
		free(effect);
		effect = next;
	}
}


/*
 * free_sound_slot()
 * Frees a sound slot.
 * Thread-safe: protected by g_sounds_mutex
 */
static void free_sound_slot(int index)
{
    if (index >= 0 && index < MAX_SOUNDS) {
		pthread_mutex_lock(&g_sounds_mutex);

		free_effect_chain(g_sounds[index].effect_chain);	
		g_sounds[index].effect_chain = NULL;

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

		/* Decrement data buffer refcount if sound was created from a buffer */
		if (g_sounds[index].data_buffer_index >= 0) {
			g_data_buffers[g_sounds[index].data_buffer_index].refcount--;
			if (g_data_buffers[g_sounds[index].data_buffer_index].refcount == 0) {
				free_data_slot(g_sounds[index].data_buffer_index);
			}
		}

        g_sounds[index].in_use = MA_FALSE;
        g_sounds[index].data_buffer_index = -1;

		pthread_mutex_unlock(&g_sounds_mutex);
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


/******************************************************************************
 * CORE INFRASTRUCTURE
 *****************************************************************************/

/*
 * pl_promini_version()
 * promini_version(-Version)
 * Unifies Version with the miniaudio version string.
 */
static foreign_t pl_promini_version(term_t version)
{
    return PL_unify_atom_chars(version, MA_VERSION_STRING);
}

/*
 * engine_audio_callback()
 * Main audio callback for the engine. 
 * Processes modulation before rendering.
 * Called by miniaudio once per audio block.
 */
static void engine_audio_callback(
		ma_device* device, 
		void* frames_out, 
		const void* frames_in, 
		ma_uint32 frame_count)
{
	ma_engine* engine = (ma_engine*)device->pUserData;
	ma_uint32 sample_rate = ma_engine_get_sample_rate(engine);

	process_modulation(frame_count, sample_rate);

	ma_engine_read_pcm_frames(engine, frames_out, frame_count, NULL);
}

/*
 * pl_promini_init()
 * promini_init
 * Initializes the global miniaudio engine.
 * Returns true if successful or already initialized.
 */
foreign_t pl_promini_init(void)
{
    ma_engine_config engine_config;
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
	engine_config = ma_engine_config_init();
	engine_config.dataCallback = engine_audio_callback;

    result = ma_engine_init(&engine_config, g_engine);
    if (result != MA_SUCCESS) {
        free(g_engine);
        g_engine = NULL;
        return FALSE;
    }

    return TRUE;
}

/*
 * sound_unload(+Handle)
 * Unloads a sound and frees its resources.
 */
static foreign_t pl_sound_unload(term_t handle)
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
 * promini_devices(-Devices)
 * Unifies Devices with a list of available audio devices.
 * Each device is represented as device(Name, Type, IsDefault) where:
 *   - Name is the device name string
 *   - Type is 'playback' or 'capture'
 *   - IsDefault is 'true' or 'false'
 */
static foreign_t pl_promini_devices(term_t devices)
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
        if (!pl_promini_init()) {
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


/******************************************************************************
 * SOUND MANAGEMENT
 *****************************************************************************/

/*
 * pl_sound_start()
 * sound_start(+Handle)
 * Starts playing a sound
 */
static foreign_t pl_sound_start(term_t handle)
{
	ma_sound* sound;
	ma_result result;

	GET_SOUND_FROM_HANDLE(handle, sound);

	result = ma_sound_start(sound);
	return (result == MA_SUCCESS) ? TRUE : FALSE;
}

/*
 * sound_stop(+Handle)
 * Stops playing a sound
 */
static foreign_t pl_sound_stop(term_t handle)
{
	ma_sound* sound;
	ma_result result;

	GET_SOUND_FROM_HANDLE(handle, sound);

	result = ma_sound_stop(sound);
	return (result == MA_SUCCESS) ? TRUE : FALSE;
}

/*
 * sound_is_playing(+Handle)
 * Succeeds if sound is playing, fails otherwise.
 */
static foreign_t pl_sound_is_playing(term_t handle)
{
	ma_sound* sound;

	GET_SOUND_FROM_HANDLE(handle, sound);

	return ma_sound_is_playing(sound) ? TRUE : FALSE;
}

/*
 * sound_set_looping(+Handle, +Loop)
 * Sets whether a sound should loop
 */
static foreign_t pl_sound_set_looping(term_t handle, term_t loop)
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
 * sound_is_looping(+Handle)
 * Succeeds if sound is set to loop, fails otherwise.
 */
static foreign_t pl_sound_is_looping(term_t handle)
{
	ma_sound* sound;

	GET_SOUND_FROM_HANDLE(handle, sound);

	return ma_sound_is_looping(sound) ? TRUE : FALSE;
}

/*
 * sound_seek(+Handle, +Frame)
 * Seeks to a specific frame position in the sound.
 */
static foreign_t pl_sound_seek(term_t handle, term_t frame)
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
 * sound_get_position(+Handle, -Frame)
 * Gets the current playback position in frames.
 */
static foreign_t pl_sound_get_position(term_t handle, term_t frame) {
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
 * sound_create(+DataHandle, -SoundHandle)
 * Creates a sound instance from a loaded data buffer.
 * Multiple sounds can be created from the same buffer for polyphony.
 */
static foreign_t pl_sound_create(term_t data_handle, term_t sound_handle) 
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

	result = ma_sound_init_from_data_source(g_engine, sound_buffer, MA_SOUND_FLAG_NO_SPATIALIZATION, NULL, sound);
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
 * sound_set_range(+Handle, +StartFrame, +EndFrame)
 * Sets the playback range for a sound (which frames to play).
 */
static foreign_t pl_sound_set_range(term_t handle, term_t start_term, term_t end_term)
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

static foreign_t pl_audio_info(term_t data_handle, term_t info)
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
 * sound_length(+Handle, -Frames)
 * Gets the total length of a sound in PCM frames.
 */
static foreign_t pl_sound_length(term_t handle, term_t frames)
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
 * sound_set_pitch(+Handle, +Pitch)
 * Sets the pitch in semitones (12 = one octave up, -12 = one octave down).
 */
static foreign_t pl_sound_set_pitch(term_t handle, term_t pitch)
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
 * sound_get_pitch(+Handle, -Pitch)
 * Gets pitch in semitones.
 */
static foreign_t pl_sound_get_pitch(term_t handle, term_t pitch)
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
 * sound_set_pan_mode(+Handle, +Mode)
 * Sets the pan mode: "balance" or "pan"
 */
static foreign_t pl_sound_set_pan_mode(term_t handle, term_t mode)
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
 * sound_get_pan_mode(+Handle, -Mode)
 * Gets the current pan mode as 'balance' or 'pan'.
 */
static foreign_t pl_sound_get_pan_mode(term_t handle, term_t mode)
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
 * sound_set_pan(+Handle, +Pan)
 * Sets stereo pan (-1.0 = hard left, 0.0 = center, 1.0 = right).
 */
static foreign_t pl_sound_set_pan(term_t handle, term_t pan)
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
 * sound_get_pan(+Handle, -Pan)
 * Gets current stereo pan value
 */
static foreign_t pl_sound_get_pan(term_t handle, term_t pan)
{
	ma_sound* sound;
	double pan_value;

	GET_SOUND_FROM_HANDLE(handle, sound);

	pan_value = ma_sound_get_pan(sound);

	return PL_unify_float(pan, (double)pan_value);
}

/*
 * sound_set_volume(+Handle, +Volume)
 * Sets volume (1.0 = normal, 0.0 = silence, >1.0 = amplification).
 */
static foreign_t pl_sound_set_volume(term_t handle, term_t volume)
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
 * sound_get_volume(+Handle, -Volume)
 * Gets current volume.
 */
static foreign_t pl_sound_get_volume(term_t handle, term_t volume)
{
	ma_sound* sound;
	float volume_value;

	GET_SOUND_FROM_HANDLE(handle, sound);

	volume_value = ma_sound_get_volume(sound);

	return PL_unify_float(volume, (double)volume_value);
}

/*
 * audio_reverse(+SourceHandle, -ReversedHandle)
 * Creates a reversed copy of a data buffer
 */
static foreign_t pl_audio_reverse(term_t source_handle, term_t reversed_handle)
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


/******************************************************************************
 * CAPTURE DEVICE MANAGEMENT
 *****************************************************************************/

/*
 * capture_data_callback()
 * Callback function for capture device data
 */
static void capture_data_callback(ma_device* device, void* output, const void*input, ma_uint32 frame_count) {
	capture_slot_t* capture;
	ma_uint64 write_pos;
  	ma_uint64 frames_to_end;
  	ma_uint32 bytes_per_frame;
  	ma_uint32 channels;

	(void)output; /* unused for capture */

	capture = (capture_slot_t*)device->pUserData;
	if (capture == NULL || input == NULL) {
		return;
	}

	channels = device->capture.channels;
	bytes_per_frame = ma_get_bytes_per_frame(device->capture.format, channels);
	write_pos = capture->write_position % capture->capacity_frames;
	frames_to_end = capture->capacity_frames - write_pos;

	if (frame_count <= frames_to_end) {
		/* no wrap */
		memcpy((char *)capture->buffer_data + (write_pos * bytes_per_frame), input, frame_count * bytes_per_frame);
	} else {
		/* wrap around */
		memcpy((char*)capture->buffer_data + (write_pos * bytes_per_frame), input, frames_to_end * bytes_per_frame);
		memcpy(capture->buffer_data, (char*) input + (frames_to_end * bytes_per_frame), (frame_count - frames_to_end) * bytes_per_frame);
	}

	capture->write_position += frame_count;
}

/*
 * capture_start(+DeviceName, +PeriodSeconds, -CaptureHandle, -BufferFrames)
 * starts capture from specified device into ring buffer.
 */
static foreign_t pl_capture_start(term_t device_name, term_t period_term, term_t capture_handle, term_t buffer_frames_out)
{
	char* name;
	double period_seconds;
  	int buffer_frames_int;
  	int slot;
  	ma_context* context;
  	ma_device_info* capture_infos;
  	ma_uint32 capture_count;
  	ma_result result;
  	ma_uint32 i;
  	ma_device_id* device_id;
  	ma_device_config device_config;
  	ma_format format;
  	ma_uint32 channels;
  	ma_uint32 sample_rate;
  	ma_uint64 buffer_frames;
  	ma_uint32 buffer_size_bytes;

	ENSURE_ENGINE_INITIALIZED();

	if (!PL_get_chars(device_name, &name, CVT_ATOM | CVT_STRING | CVT_EXCEPTION)) {
  		return FALSE;
  	}

  	if (!PL_get_float(period_term, &period_seconds)) {
  		return PL_type_error("float", period_term);
  	}

  	if (period_seconds <= 0.0) {
  		return PL_domain_error("positive_number", period_term);
  	}

	get_engine_format_info(&format, &channels, &sample_rate);
  	buffer_frames = (ma_uint64)(period_seconds * sample_rate);

	if (buffer_frames == 0) {
		buffer_frames = 1;
	}

	/* find device by name */
	context = ma_engine_get_device(g_engine)->pContext;
	result = ma_context_get_devices(context, NULL, NULL, &capture_infos, &capture_count);
	if (result != MA_SUCCESS) {
		return FALSE;
	}

	device_id = NULL;
	for (i = 0; i < capture_count; i++) {
		if (strcmp(capture_infos[i].name, name) == 0) {
			device_id = &capture_infos[i].id;
			break;
		}
	}

	if (device_id == NULL) {
		return PL_existence_error("capture_device", device_name);
	}

	slot = allocate_capture_slot();
	if (slot < 0) {
		return PL_resource_error("capture_slots");
	}

	buffer_size_bytes = buffer_frames * ma_get_bytes_per_frame(format, channels);

	g_capture_devices[slot].buffer_data = ma_malloc(buffer_size_bytes, NULL);
	if (g_capture_devices[slot].buffer_data == NULL) {
		free_capture_slot(slot);
		return PL_resource_error("memory");
	}

	g_capture_devices[slot].capacity_frames = buffer_frames;
	g_capture_devices[slot].write_position = 0;

	/* initialize capture device */
	device_config = ma_device_config_init(ma_device_type_capture);
	device_config = ma_device_config_init(ma_device_type_capture);
  	device_config.capture.pDeviceID = device_id;
  	device_config.capture.format = format;
  	device_config.capture.channels = channels;
  	device_config.sampleRate = sample_rate;
  	device_config.dataCallback = capture_data_callback;
  	device_config.pUserData = &g_capture_devices[slot];

	result = ma_device_init(context, &device_config, &g_capture_devices[slot].device);
	if (result != MA_SUCCESS) {
		free_capture_slot(slot);
		return FALSE;
	}

	/* start capture */
	result = ma_device_start(&g_capture_devices[slot].device);
	if (result != MA_SUCCESS) {
		free_capture_slot(slot);
		return FALSE;
	}

	if (!PL_unify_integer(buffer_frames_out, buffer_frames)) {
		return FALSE;
	}

	return PL_unify_integer(capture_handle, slot);
}

/*
 * capture_stop(+CaptureHandle)
 * Stops capture and frees resources.
 */
static foreign_t pl_capture_stop(term_t capture_handle)
{
	capture_slot_t* capture;

	GET_CAPTURE_DEVICE_FROM_HANDLE(capture_handle, capture);

	free_capture_slot(capture - g_capture_devices);
	return TRUE;
}

/*
 * capture_get_info(+CaptureHandle, -Info)
 * Returns capture_info(WritePosition, Capacity, SampleRate)
 */
static foreign_t pl_capture_get_info(term_t capture_handle, term_t info)
{
	capture_slot_t* capture;
  	ma_uint64 write_position;
  	term_t args;
  	functor_t info_functor;
  	term_t result;

	GET_CAPTURE_DEVICE_FROM_HANDLE(capture_handle, capture);

	write_position = capture->write_position;

	args = PL_new_term_refs(3);
	if (!PL_put_uint64(args + 0, write_position)) return FALSE;
	if (!PL_put_uint64(args + 1, capture->capacity_frames)) return FALSE;
	if (!PL_put_integer(args + 2, capture->device.sampleRate)) return FALSE;

	result = PL_new_term_ref();
	info_functor = PL_new_functor(PL_new_atom("capture_info"), 3);
	if (!PL_cons_functor_v(result, info_functor, args)) {
		return FALSE;
	}

	return PL_unify(info, result);
}

/*
 * capture_extract(+CaptureHandle, +RelativeOffset, +Length, -DataHandle)
 * Extracts frames from capture buffer to a new data buffer.
 * RelativeOffset is negative frames from current write position.
 */
static foreign_t pl_capture_extract(term_t capture_handle, term_t offset_term, term_t length_term, term_t data_handle)
{
	capture_slot_t* capture;
  	int offset_int;
  	int length_int;
  	ma_int64 offset;
  	ma_uint64 length;
  	ma_uint64 write_pos;
  	ma_uint64 read_pos;
  	ma_uint64 read_pos_wrapped;
  	ma_uint64 frames_to_end;
  	ma_format format;
  	ma_uint32 channels;
  	ma_uint32 sample_rate;
  	ma_uint32 bytes_per_frame;
  	void* extracted_data;
  	int slot;

	GET_CAPTURE_DEVICE_FROM_HANDLE(capture_handle, capture);

	if (!PL_get_integer(offset_term, &offset_int)) {
  		return PL_type_error("integer", offset_term);
  	}

  	if (!PL_get_integer(length_term, &length_int)) {
  		return PL_type_error("integer", length_term);
  	}

  	if (offset_int >= 0) {
  		return PL_domain_error("negative_offset", offset_term);
  	}

  	if (length_int <= 0) {
  		return PL_domain_error("positive_length", length_term);
  	}

	offset = (ma_int64)offset_int;
	length = (ma_uint64)length_int;

	if (length > capture->capacity_frames) {
  		return PL_domain_error("length_exceeds_capacity", length_term);
  	}

	/* read position */
	write_pos = capture->write_position;
	read_pos = write_pos + offset; /* offset is -ve, so this substracts */
	read_pos_wrapped = read_pos % capture->capacity_frames;

	/* buffer format */
	format = capture->device.capture.format;
  	channels = capture->device.capture.channels;
  	sample_rate = capture->device.sampleRate;
  	bytes_per_frame = ma_get_bytes_per_frame(format, channels);

	extracted_data = malloc(length * bytes_per_frame);
	if (extracted_data == NULL) {
		return PL_resource_error("memory");
	}

	/* copy data, handling wraparound */
	frames_to_end = capture->capacity_frames - read_pos_wrapped;
	if (length <= frames_to_end) {
		/* no wrap */
		memcpy(extracted_data,
				(char *)capture->buffer_data + (read_pos_wrapped * bytes_per_frame),
				length * bytes_per_frame);
	} else {
		/* wrap around */
		memcpy(extracted_data,
				(char *)capture->buffer_data + (read_pos_wrapped * bytes_per_frame),
				frames_to_end * bytes_per_frame);
		memcpy((char *)extracted_data + (frames_to_end * bytes_per_frame),
				capture->buffer_data,
				(length - frames_to_end) * bytes_per_frame);
	}

	slot = create_data_buffer_from_pcm(extracted_data, format, channels, length, sample_rate);
	if (slot < 0) {
		free(extracted_data);
		return PL_resource_error("data_buffer_slots");
	}

	return PL_unify_integer(data_handle, slot);
}




/* helper: add effect node to chain and connect to node graph */


/******************************************************************************
 * PROLOG INTERFACE
 *****************************************************************************/

/*
 * promini_register_predicates()
 * Register promini foreign predicates with SWI-Prolog.
 */
install_t promini_register_predicates(void)
{
    PL_register_foreign("promini_version", 1, pl_promini_version, 0);
    PL_register_foreign("promini_init", 0, pl_promini_init, 0);
    PL_register_foreign("promini_devices", 1, pl_promini_devices, 0);
    PL_register_foreign("sound_unload", 1, pl_sound_unload, 0);
	PL_register_foreign("sound_start", 1, pl_sound_start, 0);
	PL_register_foreign("sound_stop", 1, pl_sound_stop, 0);
	PL_register_foreign("sound_is_playing", 1, pl_sound_is_playing, 0);
	PL_register_foreign("sound_set_looping", 2, pl_sound_set_looping, 0);
	PL_register_foreign("sound_is_looping", 1, pl_sound_is_looping, 0);
	PL_register_foreign("audio_load", 2, pl_audio_load, 0);
	PL_register_foreign("audio_unload", 1, pl_audio_unload, 0);
	PL_register_foreign("sound_create", 2, pl_sound_create, 0);
	PL_register_foreign("sound_seek", 2, pl_sound_seek, 0);
	PL_register_foreign("sound_get_position", 2, pl_sound_get_position, 0);
	PL_register_foreign("audio_info", 2, pl_audio_info, 0);
	PL_register_foreign("sound_length", 2, pl_sound_length, 0);
	PL_register_foreign("sound_set_pitch", 2, pl_sound_set_pitch, 0);
	PL_register_foreign("sound_get_pitch", 2, pl_sound_get_pitch, 0);
	PL_register_foreign("sound_set_pan", 2, pl_sound_set_pan, 0);
	PL_register_foreign("sound_get_pan", 2, pl_sound_get_pan, 0);
	PL_register_foreign("sound_set_pan_mode", 2, pl_sound_set_pan_mode, 0);
	PL_register_foreign("sound_get_pan_mode", 2, pl_sound_get_pan_mode, 0);
	PL_register_foreign("sound_set_volume", 2, pl_sound_set_volume, 0);
	PL_register_foreign("sound_get_volume", 2, pl_sound_get_volume, 0);
	PL_register_foreign("audio_reverse", 2, pl_audio_reverse, 0);
	PL_register_foreign("sound_set_range", 3, pl_sound_set_range, 0);
	PL_register_foreign("audio_extract", 4, pl_audio_extract, 0);
	PL_register_foreign("capture_start", 4, pl_capture_start, 0);
	PL_register_foreign("capture_stop", 1, pl_capture_stop, 0);
	PL_register_foreign("capture_get_info", 2, pl_capture_get_info, 0);
	PL_register_foreign("capture_extract", 4, pl_capture_extract, 0);
}

/*
 * uninstall_promini()
 * Called when the foreign library is unloaded.
 * Cleans up the engine.
 */
install_t uninstall_promini(void)
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

	/* Clean up all capture devices */
	for (i = 0; i < MAX_CAPTURE_DEVICES; i++) {
		if (g_capture_devices[i].in_use) {
			free_capture_slot(i);
		}
	}

    /* Clean up engine */
    if (g_engine != NULL) {
        ma_engine_uninit(g_engine);
        free(g_engine);
        g_engine = NULL;
    }
}
