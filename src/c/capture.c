/*
 * capture.c - Audio capture with node graph integration
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#include "promini.h"

/******************************************************************************
 * GLOBAL VARIABLES
 *****************************************************************************/

capture_slot_t g_capture_devices[MAX_CAPTURE_DEVICES] = {{0}};
pthread_mutex_t g_capture_devices_mutex = PTHREAD_MUTEX_INITIALIZER;

/******************************************************************************
 * HELPER MACRO
 *****************************************************************************/

#define GET_CAPTURE_DEVICE_FROM_HANDLE(handle_term, capture_var) \
	do { \
		int _slot; \
		if (!get_typed_handle(handle_term, "capture", &_slot)) { \
			return PL_type_error("capture", handle_term); \
		} \
		capture_var = get_capture_device(_slot); \
		if (capture_var == NULL) { \
			return PL_existence_error("capture_device", handle_term); \
		} \
	} while(0)

/******************************************************************************
 * CAPTURE NODE PROCESSING
 *****************************************************************************/

/*
 * capture_node_process()
 * Read from capture ring buffer and output to node graph.
 */
static void capture_node_process(
		ma_node *node,
		const float **frames_in,
		ma_uint32 *frame_count_in,
		float **frames_out,
		ma_uint32 *frame_count_out)
{
	capture_slot_t *capture = (capture_slot_t *)node;
	ma_uint32 frame_count = *frame_count_out;
	ma_uint32 channels = capture->buffer.channels;
	float *output = frames_out[0];
	ma_uint32 i;
	ma_uint32 c;
	ma_uint64 read_pos;

	(void)frames_in;
	(void)frame_count_in;

	/* read from ring buffer - most recent frames */
	for (i = 0; i < frame_count; i++) {
		/* read position: frame_count - i frames behind write position */
		read_pos = (capture->buffer.write_pos + capture->buffer.capacity_frames - frame_count + i)
		           % capture->buffer.capacity_frames;

		for (c = 0; c < channels; c++) {
			output[i * channels + c] = capture->buffer.samples[read_pos * channels + c];
		}
	}
}

static ma_node_vtable capture_node_vtable = {
	capture_node_process,
	NULL,
	0,		/* 0 input buses (source node) */
	1		/* 1 output bus */
};

/******************************************************************************
 * SLOT MANAGEMENT
 *****************************************************************************/

/*
 * allocate_capture_slot()
 * Finds a free capture slot and marks it as in use.
 * Returns slot index, or -1 if all slots are full.
 */
static int allocate_capture_slot(void)
{
	int i;
	int slot = -1;

	pthread_mutex_lock(&g_capture_devices_mutex);
	for (i = 0; i < MAX_CAPTURE_DEVICES; i++) {
		if (!g_capture_devices[i].in_use) {
			g_capture_devices[i].in_use = MA_TRUE;
			g_capture_devices[i].buffer.samples = NULL;
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
 */
static void free_capture_slot(int index)
{
	if (index >= 0 && index < MAX_CAPTURE_DEVICES) {
		pthread_mutex_lock(&g_capture_devices_mutex);

		if (g_capture_devices[index].in_use) {
			ma_device_uninit(&g_capture_devices[index].device);
			ma_node_uninit(&g_capture_devices[index].node, NULL);
			ring_buffer_free(&g_capture_devices[index].buffer);
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
capture_slot_t *get_capture_device(int index)
{
	if (index < 0 || index >= MAX_CAPTURE_DEVICES) {
		return NULL;
	}
	if (!g_capture_devices[index].in_use) {
		return NULL;
	}
	return &g_capture_devices[index];
}

/******************************************************************************
 * CAPTURE DEVICE CALLBACK
 *****************************************************************************/

/*
 * capture_data_callback()
 * Callback function for capture device data.
 */
static void capture_data_callback(ma_device *device, void *output, const void *input, ma_uint32 frame_count)
{
	capture_slot_t *capture;

	(void)output;

	capture = (capture_slot_t *)device->pUserData;
	if (capture == NULL || input == NULL) {
		return;
	}

	ring_buffer_write(&capture->buffer, input, frame_count);
}

/******************************************************************************
 * PROLOG INTERFACE
 *****************************************************************************/

/*
 * pl_capture_start()
 * capture_start(+DeviceName, +PeriodSeconds, -Capture, -BufferFrames)
 * Starts capture from specified device into ring buffer. Returns capture(N).
 */
static foreign_t pl_capture_start(term_t device_name, term_t period_term, term_t capture_handle, term_t buffer_frames_out)
{
	char *name;
	double period_seconds;
	int slot;
	ma_context *context;
	ma_device_info *capture_infos;
	ma_uint32 capture_count;
	ma_result result;
	ma_uint32 i;
	ma_device_id *device_id;
	ma_device_config device_config;
	ma_node_config node_config;
	ma_format format;
	ma_uint32 channels;
	ma_uint32 sample_rate;
	ma_uint64 buffer_frames;
	capture_slot_t *capture;

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

	capture = &g_capture_devices[slot];

	result = ring_buffer_init(&capture->buffer, buffer_frames, channels, format);
	if (result != MA_SUCCESS) {
		free_capture_slot(slot);
		return PL_resource_error("memory");
	}

	/* initialize capture device */
	device_config = ma_device_config_init(ma_device_type_capture);
	device_config.capture.pDeviceID = device_id;
	device_config.capture.format = format;
	device_config.capture.channels = channels;
	device_config.sampleRate = sample_rate;
	device_config.dataCallback = capture_data_callback;
	device_config.pUserData = capture;

	result = ma_device_init(context, &device_config, &capture->device);
	if (result != MA_SUCCESS) {
		free_capture_slot(slot);
		return FALSE;
	}

	/* initialize capture node */
	node_config = ma_node_config_init();
	node_config.vtable = &capture_node_vtable;
	node_config.pInputChannels = NULL;
	node_config.pOutputChannels = &channels;

	result = ma_node_init(ma_engine_get_node_graph(g_engine), &node_config, NULL, &capture->node);
	if (result != MA_SUCCESS) {
		ma_device_uninit(&capture->device);
		free_capture_slot(slot);
		return FALSE;
	}

	/* connect node to endpoint */
	result = ma_node_attach_output_bus(&capture->node, 0,
			ma_node_graph_get_endpoint(ma_engine_get_node_graph(g_engine)), 0);
	if (result != MA_SUCCESS) {
		ma_node_uninit(&capture->node, NULL);
		ma_device_uninit(&capture->device);
		free_capture_slot(slot);
		return FALSE;
	}

	/* start capture */
	result = ma_device_start(&capture->device);
	if (result != MA_SUCCESS) {
		free_capture_slot(slot);
		return FALSE;
	}

	if (!PL_unify_integer(buffer_frames_out, buffer_frames)) {
		return FALSE;
	}

	return unify_typed_handle(capture_handle, "capture", slot);
}

/*
 * pl_capture_stop()
 * capture_stop(+Capture)
 * Stops capture and frees resources.
 */
static foreign_t pl_capture_stop(term_t capture_handle)
{
	capture_slot_t *capture;

	GET_CAPTURE_DEVICE_FROM_HANDLE(capture_handle, capture);

	free_capture_slot(capture - g_capture_devices);
	return TRUE;
}

/*
 * pl_capture_get_info()
 * capture_get_info(+Capture, -Info)
 * Returns capture_info(WritePosition, Capacity, SampleRate).
 */
static foreign_t pl_capture_get_info(term_t capture_handle, term_t info)
{
	capture_slot_t *capture;
	ma_uint64 write_position;
	term_t args;
	functor_t info_functor;
	term_t result_term;

	GET_CAPTURE_DEVICE_FROM_HANDLE(capture_handle, capture);

	write_position = capture->buffer.write_pos;

	args = PL_new_term_refs(3);
	if (!PL_put_uint64(args + 0, write_position)) return FALSE;
	if (!PL_put_uint64(args + 1, capture->buffer.capacity_frames)) return FALSE;
	if (!PL_put_integer(args + 2, capture->device.sampleRate)) return FALSE;

	result_term = PL_new_term_ref();
	info_functor = PL_new_functor(PL_new_atom("capture_info"), 3);
	if (!PL_cons_functor_v(result_term, info_functor, args)) {
		return FALSE;
	}

	return PL_unify(info, result_term);
}

/*
 * pl_capture_extract()
 * capture_extract(+Capture, +RelativeOffset, +Length, -Audio)
 * Extracts frames from capture buffer to a new data buffer. Returns audio(N).
 * RelativeOffset is negative frames from current write position.
 */
static foreign_t pl_capture_extract(term_t capture_handle, term_t offset_term, term_t length_term, term_t data_handle)
{
	capture_slot_t *capture;
	int offset_int;
	int length_int;
	ma_int64 offset;
	ma_uint64 length;
	ma_format format;
	ma_uint32 channels;
	ma_uint32 sample_rate;
	ma_uint32 bytes_per_frame;
	void *extracted_data;
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

	if (length > capture->buffer.capacity_frames) {
		return PL_domain_error("length_exceeds_capacity", length_term);
	}

	/* buffer format */
	format = capture->buffer.format;
	channels = capture->buffer.channels;
	sample_rate = capture->device.sampleRate;
	bytes_per_frame = ma_get_bytes_per_frame(format, channels);

	extracted_data = malloc(length * bytes_per_frame);
	if (extracted_data == NULL) {
		return PL_resource_error("memory");
	}

	/* copy data */
	ring_buffer_read(&capture->buffer, extracted_data, (ma_uint64)(-offset), length);

	slot = create_audio_buffer_from_pcm(extracted_data, format, channels, length, sample_rate);
	if (slot < 0) {
		free(extracted_data);
		return PL_resource_error("audio_buffer_slots");
	}

	return unify_typed_handle(data_handle, "audio", slot);
}

/******************************************************************************
 * MODULE REGISTRATION
 *****************************************************************************/

install_t capture_register_predicates(void)
{
	PL_register_foreign("capture_start", 4, pl_capture_start, 0);
	PL_register_foreign("capture_stop", 1, pl_capture_stop, 0);
	PL_register_foreign("capture_get_info", 2, pl_capture_get_info, 0);
	PL_register_foreign("capture_extract", 4, pl_capture_extract, 0);
}

install_t uninstall_capture(void)
{
	int i;

	for (i = 0; i < MAX_CAPTURE_DEVICES; i++) {
		if (g_capture_devices[i].in_use) {
			free_capture_slot(i);
		}
	}
}
