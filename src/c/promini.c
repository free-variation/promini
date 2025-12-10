/*
		output[i4dd * cannels] *= g->gain_normalization;
		if (channels > 1) {
			output[i * channels + 1] *= g->gain_normalization;
		}		
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
 * Accepts audio(Handle) term
 */
#define GET_DATA_BUFFER_FROM_HANDLE(handle_term, buffer_var) \
    do { \
        int _slot; \
        atom_t _name; \
        size_t _arity; \
        term_t _arg; \
        if (!PL_get_name_arity(handle_term, &_name, &_arity) || \
            _arity != 1 || \
            strcmp(PL_atom_chars(_name), "audio") != 0) { \
            return PL_type_error("audio(Handle)", handle_term); \
        } \
        _arg = PL_new_term_ref(); \
        if (!PL_get_arg(1, handle_term, _arg) || !PL_get_integer(_arg, &_slot)) { \
            return PL_type_error("audio(Handle)", handle_term); \
        } \
        buffer_var = get_data_buffer(_slot); \
        if (buffer_var == NULL) { \
            return PL_existence_error("data_buffer", handle_term); \
        } \
    } while(0)

/*
 * Macro to validate handle and get data buffer pointer with slot index
 * Use when you need access to both the buffer and the slot (e.g., for pData access)
 * Accepts audio(Handle) term
 */
#define GET_DATA_BUFFER_WITH_SLOT(handle_term, buffer_var, slot_var) \
    do { \
        atom_t _name; \
        size_t _arity; \
        term_t _arg; \
        if (!PL_get_name_arity(handle_term, &_name, &_arity) || \
            _arity != 1 || \
            strcmp(PL_atom_chars(_name), "audio") != 0) { \
            return PL_type_error("audio(Handle)", handle_term); \
        } \
        _arg = PL_new_term_ref(); \
        if (!PL_get_arg(1, handle_term, _arg) || !PL_get_integer(_arg, &slot_var)) { \
            return PL_type_error("audio(Handle)", handle_term); \
        } \
        buffer_var = get_data_buffer(slot_var); \
        if (buffer_var == NULL) { \
            return PL_existence_error("data_buffer", handle_term); \
        } \
    } while(0)

/*
 * unify_audio_handle()
 * Unifies term with audio(Handle).
 */
static int unify_audio_handle(term_t term, int slot)
{
	term_t arg = PL_new_term_ref();
	functor_t f = PL_new_functor(PL_new_atom("audio"), 1);

	if (!PL_put_integer(arg, slot)) return FALSE;
	return PL_unify_term(term, PL_FUNCTOR, f, PL_TERM, arg);
}

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

/*
 * Sound handle management
 */


static data_slot_t g_data_buffers[MAX_DATA_BUFFERS] = {{NULL, NULL, 0, MA_FALSE}};

/* Shared sound slots array (declared in promini.h) */
sound_slot_t g_sounds[MAX_SOUNDS] = {{NULL, NULL, -1, MA_FALSE, NULL}};


/******************************************************************************
 * HELPER FUNCTIONS
 *****************************************************************************/

/*
 * Parameter parsing helpers for key=value lists.
 * Used by effects.c and image.c for set_parameters predicates.
 */
#define DEFINE_GET_PARAM(suffix, type, get_code) \
	ma_bool32 get_param_##suffix(term_t params, const char* key, type* value) \
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
 * get_source_from_term()
 * Parse sound(N), voice(N), image_synth(N), or capture(N) term.
 * Returns source node and effect chain.
 */
ma_bool32 get_source_from_term(term_t source_term, ma_node **source_node, effect_node_t **chain)
{
	term_t slot_term = PL_new_term_ref();
	functor_t f;
	int slot;
	capture_slot_t *capture;

	if (!PL_get_functor(source_term, &f)) return MA_FALSE;
	if (!PL_get_arg(1, source_term, slot_term)) return MA_FALSE;
	if (!PL_get_integer(slot_term, &slot)) return MA_FALSE;

	if (f == PL_new_functor(PL_new_atom("sound"), 1)) {
		if (slot < 0 || slot >= MAX_SOUNDS || !g_sounds[slot].in_use) return MA_FALSE;
		*source_node = (ma_node *)g_sounds[slot].sound;
		*chain = g_sounds[slot].effect_chain;
		return MA_TRUE;
	} else if (f == PL_new_functor(PL_new_atom("voice"), 1)) {
		if (slot < 0 || slot >= MAX_VOICES || !g_voices[slot].in_use) return MA_FALSE;
		*source_node = (ma_node *)&g_voices[slot].group;
		*chain = g_voices[slot].effect_chain;
		return MA_TRUE;
	} else if (f == PL_new_functor(PL_new_atom("image_synth"), 1)) {
		if (slot < 0 || slot >= MAX_IMAGE_SYNTHS || !g_image_synths[slot].in_use) return MA_FALSE;
		*source_node = (ma_node *)&g_image_synths[slot].base;
		*chain = g_image_synths[slot].effect_chain;
		return MA_TRUE;
	} else if (f == PL_new_functor(PL_new_atom("capture"), 1)) {
		capture = get_capture_device(slot);
		if (capture == NULL) return MA_FALSE;
		*source_node = (ma_node *)&capture->node;
		*chain = NULL;
		return MA_TRUE;
	} else if (f == PL_new_functor(PL_new_atom("granular"), 1)) {
		if (slot < 0 || slot >= MAX_GRANULAR_DELAYS || !g_granular_delays[slot].in_use) return MA_FALSE;
		*source_node = (ma_node *)&g_granular_delays[slot].base;
		*chain = g_granular_delays[slot].effect_chain;
		return MA_TRUE;
	}

	return MA_FALSE;
}

/*
 * create_data_buffer_from_pcm()
 * Creates a data buffer from raw PCM data. Returns slot index or -1 on error.
 * Caller is responsible for freeing pData on error.
 */
int create_data_buffer_from_pcm(void *pData, ma_format format, ma_uint32 channels,
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
 * RING BUFFER
 *****************************************************************************/

/*
 * ring_buffer_init()
 * Alocate ring buffer.  Returns MA_SUCCESS or MA_OUT_OF_MEMORY.
 */
ma_result ring_buffer_init(ring_buffer_t *rb, ma_uint64 capacity_frames, ma_uint32 channels, ma_format format)
{
	ma_uint32 bytes_per_frame;
	ma_uint64 buffer_size_bytes;

	bytes_per_frame = ma_get_bytes_per_frame(format, channels);
	buffer_size_bytes = capacity_frames * bytes_per_frame;

	rb->samples = (float *)ma_malloc(buffer_size_bytes, NULL);
	if (rb->samples == NULL) return MA_OUT_OF_MEMORY;

	memset(rb->samples, 0, buffer_size_bytes);
	rb->capacity_frames = capacity_frames;
	rb->channels = channels;
	rb->format = format;
	rb->write_pos = 0;

	return MA_SUCCESS;
}

/*
 * ring_buffer_free()
 * Free ring buffer memory.
 */
void ring_buffer_free(ring_buffer_t *rb)
{
	if (rb->samples != NULL) {
		ma_free(rb->samples, NULL);
		rb->samples = NULL;
	}
}

/*
 * ring_buffer_write()
 * Write frames to ring buffer, advancing write position with wrap.
 */
void ring_buffer_write(ring_buffer_t *rb, const void *input, ma_uint32 frame_count) 
{
	ma_uint64 write_pos;
	ma_uint64 frames_to_end;
	ma_uint32 bytes_per_frame;

	bytes_per_frame = ma_get_bytes_per_frame(rb->format, rb->channels);
	write_pos = rb->write_pos % rb->capacity_frames;
	frames_to_end = rb->capacity_frames - write_pos;

	if (frame_count <= frames_to_end) {
		memcpy((char *)rb->samples + (write_pos * bytes_per_frame), input, frame_count * bytes_per_frame);
	} else {
		memcpy((char *)rb->samples + (write_pos * bytes_per_frame), input, frames_to_end * bytes_per_frame);
		memcpy(rb->samples, 
				(char *)input + (frames_to_end * bytes_per_frame), 
				(frame_count - frames_to_end) *bytes_per_frame);
	}

	rb->write_pos += frame_count;
}

/*
 * ring_buffer_read()
 * Read frames from ring buffer at given delay (frames before write_pos).
 */
void ring_buffer_read(ring_buffer_t *rb, void *output, ma_uint64 delay_frames, ma_uint32 frame_count)
{
	ma_uint64 read_pos;
	ma_uint64 frames_to_end;
	ma_uint32 bytes_per_frame;

	if (delay_frames >= rb->capacity_frames) {
		delay_frames = rb->capacity_frames - 1;
	}

	bytes_per_frame = ma_get_bytes_per_frame(rb->format, rb->channels);
	read_pos = (rb->write_pos + rb->capacity_frames - delay_frames) % rb->capacity_frames;
	frames_to_end = rb->capacity_frames - read_pos;

	if (frame_count <= frames_to_end) {
		memcpy(output, (char *)rb->samples + (read_pos * bytes_per_frame), frame_count * bytes_per_frame);
	} else {
		memcpy(output, (char *)rb->samples + (read_pos * bytes_per_frame), frames_to_end * bytes_per_frame);
		memcpy((char *)output + (frames_to_end * bytes_per_frame), rb->samples, (frame_count - frames_to_end) * bytes_per_frame);
	}
}

/*
 * ring_buffer_read_interpolated()
 * Read a single sample from the ring buffer at a fractional position.
 * Uses Catmull-Rom cubic interpolation for smooth pitch shifting.
 */
float ring_buffer_read_interpolated(ring_buffer_t* rb, float position, ma_uint32 channel)
{
	ma_uint64 i0, i1, i2, i3;
	float frac;
	float y0, y1, y2, y3;
	float c0, c1, c2, c3;

	/* Fractional position between samples */
	frac = position - floorf(position);

	/* Four adjacent sample positions with wraparound */
	i1 = (ma_uint64)position % rb->capacity_frames;
	i0 = (i1 == 0) ? rb->capacity_frames - 1 : i1 - 1;
	i2 = (i1 + 1) % rb->capacity_frames;
	i3 = (i2 + 1) % rb->capacity_frames;

	/* Get four adjacent samples */
	y0 = rb->samples[i0 * rb->channels + channel];
	y1 = rb->samples[i1 * rb->channels + channel];
	y2 = rb->samples[i2 * rb->channels + channel];
	y3 = rb->samples[i3 * rb->channels + channel];

	/* Catmull-Rom spline coefficients */
	c0 = y1;
	c1 = 0.5f * (y2 - y0);
	c2 = y0 - 2.5f * y1 + 2.0f * y2 - 0.5f * y3;
	c3 = 0.5f * (y3 - y0) + 1.5f * (y1 - y2);

	/* Evaluate cubic polynomial */
	return ((c3 * frac + c2) * frac + c1) * frac + c0;
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

	return unify_audio_handle(handle, slot);
}

/*
 * extract_from_ring_buffer()
 * Extract audio data from a ring buffer into a new data buffer.
 * Offset is relative to current write position (negative = past).
 */
static foreign_t extract_from_ring_buffer(
    ring_buffer_t *rb,
    ma_uint32 sample_rate,
    ma_uint64 max_frames,
    term_t offset_term,
    term_t length_term,
    term_t extracted_handle)
{
	int offset_int, length_int;
	ma_uint64 offset_frames, length_frames;
	void *extracted_data;
	int slot;

	if (!PL_get_integer(offset_term, &offset_int) ||
	    !PL_get_integer(length_term, &length_int)) {
		return PL_type_error("integer", offset_term);
	}

	offset_frames = (ma_uint64)(-offset_int);
	length_frames = (ma_uint64)length_int;

	if (offset_frames > max_frames) offset_frames = max_frames;
	if (length_frames > max_frames) length_frames = max_frames;
	if (offset_frames + length_frames > max_frames) {
		length_frames = max_frames - offset_frames;
	}

	extracted_data = malloc(length_frames * rb->channels * sizeof(float));
	if (extracted_data == NULL) {
		return PL_resource_error("memory");
	}

	ring_buffer_read(rb, extracted_data, offset_frames, (ma_uint32)length_frames);

	slot = create_data_buffer_from_pcm(extracted_data, ma_format_f32, rb->channels,
	                                   length_frames, sample_rate);
	if (slot < 0) {
		free(extracted_data);
		return PL_resource_error("data_buffer_slots");
	}

	return unify_audio_handle(extracted_handle, slot);
}

/*
 * pl_audio_extract()
 * audio_extract(+Source, +Offset, +Length, -ExtractedHandle)
 * Extracts a slice of frames into a new buffer.
 * Source can be:
 * 	- audio(Handle) - data buffer (absolute offset)
 * 	- capture(Handle) - capture ring buffer (negative offset from write head)
 * 	- granular(Handle) - granular ring buffer (negative offset from write head)
 */
static foreign_t pl_audio_extract(
		term_t source_handle,
		term_t start_term,
		term_t length_term,
		term_t extracted_handle)
{
	atom_t name;
	size_t arity;
	term_t arg;
	int slot;
	const char *name_str;

	if (!PL_get_name_arity(source_handle, &name, &arity) || arity != 1) {
		return PL_type_error("audio|capture|granular", source_handle);
	}

	arg = PL_new_term_ref();
	if (!PL_get_arg(1, source_handle, arg) || !PL_get_integer(arg, &slot)) {
		return PL_type_error("handle", source_handle);
	}

	name_str = PL_atom_chars(name);

	if (strcmp(name_str, "audio") == 0) {
		ma_audio_buffer *source_buffer;
		int start_int, length_int;
		ma_uint64 start_frame, length_frames;
		ma_uint64 source_frame_count;
		ma_uint32 channels;
		ma_uint32 sample_rate;
		ma_format format;
		ma_uint32 bytes_per_frame;
		void *source_data;
		void *extracted_data;
		int extracted_slot;

		source_buffer = get_data_buffer(slot);
		if (source_buffer == NULL) {
			return PL_existence_error("data_buffer", source_handle);
		}

		if (!PL_get_integer(start_term, &start_int)) {
			return PL_type_error("integer", start_term);
		}

		if (!PL_get_integer(length_term, &length_int)) {
			return PL_type_error("integer", length_term);
		}

		if (start_int < 0 || length_int <= 0) {
			return PL_domain_error("valid_extraction_params",
			                       start_int < 0 ? start_term : length_term);
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

		source_data = g_data_buffers[slot].pData;
		memcpy(extracted_data,
		       (char*)source_data + (start_frame * bytes_per_frame),
		       length_frames * bytes_per_frame);

		extracted_slot = create_data_buffer_from_pcm(extracted_data, format, channels,
		                                             length_frames, sample_rate);
		if (extracted_slot < 0) {
			free(extracted_data);
			return PL_resource_error("data_buffer_slots");
		}

		return unify_audio_handle(extracted_handle, extracted_slot);
	}
	else if (strcmp(name_str, "capture") == 0) {
		capture_slot_t *capture;
		ma_uint32 sr;

		capture = get_capture_device(slot);
		if (capture == NULL) {
			return PL_existence_error("capture", source_handle);
		}
		get_engine_format_info(NULL, NULL, &sr);
		return extract_from_ring_buffer(&capture->buffer, sr,
		                                capture->buffer.capacity_frames,
		                                start_term, length_term, extracted_handle);
	}
	else if (strcmp(name_str, "granular") == 0) {
		granular_delay_t *g;
		ma_uint32 sr;

		if (slot < 0 || slot >= MAX_GRANULAR_DELAYS || !g_granular_delays[slot].in_use) {
			return PL_existence_error("granular", source_handle);
		}
		g = &g_granular_delays[slot];
		get_engine_format_info(NULL, NULL, &sr);
		return extract_from_ring_buffer(&g->buffer, sr,
		                                g->frames_recorded,
		                                start_term, length_term, extracted_handle);
	}
	else {
		return PL_type_error("audio|capture|granular", source_handle);
	}
}

/*
 * audio_unload(+Handle)
 * Decrements reference count on data buffer.
 * Frees the buffer when refcount reaches zero.
 */
static foreign_t pl_audio_unload(term_t handle)
{
	int slot;
	atom_t name;
	size_t arity;
	term_t arg;

	if (!PL_get_name_arity(handle, &name, &arity) ||
	    arity != 1 ||
	    strcmp(PL_atom_chars(name), "audio") != 0) {
		return PL_type_error("audio(Handle)", handle);
	}
	arg = PL_new_term_ref();
	if (!PL_get_arg(1, handle, arg) || !PL_get_integer(arg, &slot)) {
		return PL_type_error("audio(Handle)", handle);
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
		} else if (effect->type == EFFECT_COMPRESSOR) {
			compressor_node_t* comp = (compressor_node_t*)effect->effect_node;
			if (comp->delay_buffer != NULL) {
				free(comp->delay_buffer);
			}
		} else if (effect->type == EFFECT_PING_PONG_DELAY) {
			ping_pong_delay_node_t* pp = (ping_pong_delay_node_t*)effect->effect_node;
			if (pp->buffer_l != NULL) {
				free(pp->buffer_l);
			}
			if (pp->buffer_r != NULL) {
				free(pp->buffer_r);
			}
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
	engine_config.periodSizeInMilliseconds = 10;
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

	GET_DATA_BUFFER_WITH_SLOT(data_handle, source_buffer, data_slot);

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

	ratio = SEMITONES_TO_RATIO((float)pitch_value);
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

	return unify_audio_handle(reversed_handle, slot);
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
	PL_register_foreign("audio_reverse", 2, pl_audio_reverse, 0);
	PL_register_foreign("sound_set_range", 3, pl_sound_set_range, 0);
	PL_register_foreign("audio_extract", 4, pl_audio_extract, 0);
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

    /* Clean up engine */
    if (g_engine != NULL) {
        ma_engine_uninit(g_engine);
        free(g_engine);
        g_engine = NULL;
    }
}
