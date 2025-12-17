/*
 * visualizer.c - Audio visualization with SDL
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#include "promini.h"

/******************************************************************************
 * GLOBAL VARIABLES
 *****************************************************************************/

visualizer_node_t g_visualizers[MAX_VISUALIZERS] = {{0}};
pthread_mutex_t g_visualizers_mutex = PTHREAD_MUTEX_INITIALIZER;

/******************************************************************************
 * THEME DEFINITIONS
 *****************************************************************************/

typedef struct {
	Uint8 bg_r, bg_g, bg_b;
	Uint8 fg_r, fg_g, fg_b;
} viz_theme_t;

static const viz_theme_t viz_themes[] = {
	{ 32, 32, 32,    255, 255, 255 },   /* default: white on dark gray */
	{ 0, 0, 0,       0, 255, 0 },       /* osc: green on black */
	{ 180, 200, 220, 0, 0, 0 }          /* blueprint: black on light blue */
};

#define VIZ_THEME_COUNT (sizeof(viz_themes) / sizeof(viz_themes[0]))

/******************************************************************************
 * SLOT AND WINDOW MANAGEMENT
 *****************************************************************************/

/*
 * allocate_visualizer_slot()
 * Finds a free visualizer slot and marks it as in use.
 * Returns slot index, or -1 if all slots are full.
 */
static int allocate_visualizer_slot(void)
{
	int i;
	for (i = 0; i < MAX_VISUALIZERS; i++) {
		if (!g_visualizers[i].in_use) {
			g_visualizers[i].in_use = MA_TRUE;
			return i;
		}
	}
	return -1;
}

/*
 * free_visualizer_slot()
 * Frees a visualizer slot.
 */
static void free_visualizer_slot(int index)
{
	if (index >= 0 && index < MAX_VISUALIZERS) {
		g_visualizers[index].in_use = MA_FALSE;
	}
}

/*
 * cleanup_visualizer()
 * Cleans up a visualizer's resources. Caller must hold mutex.
 */
static void cleanup_visualizer(visualizer_node_t *viz)
{
	if (viz->renderer != NULL) {
		SDL_DestroyRenderer(viz->renderer);
		viz->renderer = NULL;
	}
	if (viz->window != NULL) {
		SDL_DestroyWindow(viz->window);
		viz->window = NULL;
	}
	ma_node_detach_output_bus(&viz->base, 0);
	ma_node_uninit(&viz->base, NULL);
	ring_buffer_free(&viz->buffer);
}

/* visualizer_handle_event()
 * Handle SDL event if it belongs to a visualizer window.
 */
void visualizer_handle_event(SDL_Event *event) 
{
	int i;
	SDL_WindowID event_wid;
	visualizer_node_t *viz;

	/* extract window ID based on event type */
	switch (event->type) {
		case SDL_EVENT_KEY_DOWN:
		case SDL_EVENT_KEY_UP:
			event_wid = event->key.windowID;
			break;
		case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
			event_wid = event->window.windowID;
			break;
		default:
			return;
	}

	pthread_mutex_lock(&g_visualizers_mutex);

	for (i = 0; i < MAX_VISUALIZERS; i++) {
		viz = &g_visualizers[i];

		if (!viz->in_use || viz->window == NULL) continue;
		if (SDL_GetWindowID(viz->window) != event_wid) continue;

		if (event->type == SDL_EVENT_WINDOW_CLOSE_REQUESTED) {
			cleanup_visualizer(viz);
			free_visualizer_slot(i);
		} else if (event->type == SDL_EVENT_KEY_DOWN) {
			switch (event->key.scancode) {
				case SDL_SCANCODE_ESCAPE:
					cleanup_visualizer(viz);
					free_visualizer_slot(i);
					break;
				case SDL_SCANCODE_TAB:
					viz->mode = (viz->mode == VIZ_MODE_WAVEFORM)
						? VIZ_MODE_SPECTROGRAM : VIZ_MODE_WAVEFORM;
					break;
				case SDL_SCANCODE_T:
					viz->triggered = !viz->triggered;
					break;
				case SDL_SCANCODE_SPACE:
					viz->paused = !viz->paused;
					break;
				case SDL_SCANCODE_L:
					viz->log_freq = !viz->log_freq;
					break;
				case SDL_SCANCODE_D:
					viz->db_scale = !viz->db_scale;
					break;
				case SDL_SCANCODE_LEFT:
					viz->zoom *= 1.25f;
					if (viz->zoom > 4.0f) viz->zoom = 4.0f;
					break;
				case SDL_SCANCODE_RIGHT:
					viz->zoom *= 0.8f;
					if (viz->zoom < 0.25f) viz->zoom = 0.25f;
					break;
				case SDL_SCANCODE_UP:
					viz->amp_scale *= 1.25f;
					if (viz->amp_scale > 16.0f) viz->amp_scale = 16.0f;
					break;
				case SDL_SCANCODE_DOWN:
					viz->amp_scale *= 0.8f;
					if (viz->amp_scale < 0.25f) viz->amp_scale = 0.25f;
					break;
				case SDL_SCANCODE_C:
					viz->theme = (viz->theme + 1) % VIZ_THEME_COUNT;
					break;
				default:
					break;
			}
		}

		pthread_mutex_unlock(&g_visualizers_mutex);
		return;
	}

	pthread_mutex_unlock(&g_visualizers_mutex);
}


/******************************************************************************
 * NODE PROCESSING
 *****************************************************************************/

/*
 * visualizer_process_pcm_frames()
 * Passthrough node that copies audio to ring buffer for visualization.
 */
static void visualizer_process_pcm_frames(
		ma_node *node,
		const float **frames_in,
		ma_uint32 *frame_count_in,
		float **frames_out,
		ma_uint32 *frame_count_out)
{
	visualizer_node_t *viz = (visualizer_node_t *)node;
	ma_uint32 frame_count = *frame_count_out;
	ma_uint32 channels = viz->buffer.channels;

	(void)frame_count_in;

	/* copy to visualization buffer */
	ring_buffer_write(&viz->buffer, frames_in[0], frame_count);

	/* pass through unchanged */
	memcpy(frames_out[0], frames_in[0], frame_count * channels * sizeof(float));
}

static ma_node_vtable visualizer_vtable = {
	visualizer_process_pcm_frames,
	NULL,
	1,	/* 1 input bus */
	1	/* 1 output bus */
};

/*
 * find_trigger()
 * Search for positive-going zero crossing.  
 * Returns frame offset from read_starte
 */

static ma_uint64 find_trigger(
		float *samples,
		ma_uint64 read_start,
		ma_uint64 capacity,
		ma_uint32 channels)
{
	ma_uint64 i;
	float prev, curr;
	ma_uint64 idx;

	for (i = 0; i < VIZ_TRIGGER_SEARCH_FRAMES; i++) {
		idx = ((read_start + i) % capacity) * channels;
		prev = samples[idx];
		idx = ((read_start + i + 1) % capacity) * channels;
		curr = samples[idx];
		if (prev <= 0.0f && curr > 0.0f) {
			return i;
		}
	}
	return 0;
}


/*
 * render_waveform()
 * Draw waveform for one visualizer.  Stereo L top half, R bottom half.
 */
static void render_waveform(visualizer_node_t *viz)
{
	int win_w, win_h;
	ma_uint32 frames_to_draw;
	ma_uint64 read_start;
	ma_uint64 trigger_offset;
	ma_uint64 idx;
	ma_uint64 frame_idx;
	float *samples;
	int i;
	float x,y, prev_x, prev_y;
	float sample;
	int half_h;
	const viz_theme_t *theme_colors;

	SDL_GetWindowSize(viz->window, &win_w, &win_h);
	frames_to_draw = (ma_uint32)(win_w * viz->zoom);
	if (frames_to_draw > viz->buffer.capacity_frames) {
		frames_to_draw = viz->buffer.capacity_frames;
	}

	samples = viz->buffer.samples;
	read_start = (viz->buffer.write_pos + viz->buffer.capacity_frames - frames_to_draw) 
		% viz->buffer.capacity_frames;
	
	if (viz->triggered) {
		/* Start further back to account for trigger search range */
		read_start = (read_start + viz->buffer.capacity_frames - VIZ_TRIGGER_SEARCH_FRAMES)
			% viz->buffer.capacity_frames;
		trigger_offset = find_trigger(
				samples,
				read_start,
				viz->buffer.capacity_frames,
				viz->buffer.channels);
		read_start = (read_start + trigger_offset) % viz->buffer.capacity_frames;
	}

	half_h = win_h / 2;

	/* set waveform color based on theme */
	theme_colors = &viz_themes[viz->theme];
	SDL_SetRenderDrawColor(viz->renderer, theme_colors->fg_r, theme_colors->fg_g, theme_colors->fg_b, 255);

	/* left channel (top half) */
	prev_x = 0.0f;
	sample = CLAMP(samples[read_start * viz->buffer.channels] * viz->amp_scale, -1.0f, 1.0f);
	prev_y = half_h /2 - sample * half_h / 2;

	for (i = 1; i < win_w; i++) {
		frame_idx = (ma_uint64)i * frames_to_draw / win_w;
		idx = ((read_start + frame_idx) % viz->buffer.capacity_frames) * viz->buffer.channels;
		sample = CLAMP(samples[idx] * viz->amp_scale, -1.0f, 1.0f);
		x = (float)i;
		y = half_h / 2 - sample * half_h / 2;
		SDL_RenderLine(viz->renderer, prev_x, prev_y, x, y);
		prev_x = x;
		prev_y = y;
	}

	/* right channel (bottom half) */
	prev_x = 0;
	sample = CLAMP(samples[read_start * viz->buffer.channels + 1] * viz->amp_scale, -1.0f, 1.0f);
	prev_y = half_h + half_h / 2 - sample * half_h / 2;

	for (i = 1; i < win_w; i++) {
		frame_idx = (ma_uint64)i * frames_to_draw / win_w;
		idx = ((read_start + frame_idx) % viz->buffer.capacity_frames) * viz->buffer.channels + 1;
		sample = CLAMP(samples[idx] * viz->amp_scale, -1.0f, 1.0f);
		x = (float)i;
		y = half_h + half_h / 2 - sample * half_h / 2;
		SDL_RenderLine(viz->renderer, prev_x, prev_y, x, y);
		prev_x = x;
		prev_y = y;
	}
}


/*
 * visualizer_render_all()
 * Render all active visualizer windows.
 */
void visualizer_render_all(void)
{
	int i;
	visualizer_node_t *viz;
	const viz_theme_t *theme_colors;

	pthread_mutex_lock(&g_visualizers_mutex);

	for (i = 0; i < MAX_VISUALIZERS; i++) {
		viz = &g_visualizers[i];
		if (!viz->in_use || viz->window == NULL) continue;
		if (viz->paused) continue;

		/* clear with theme background */
		theme_colors = &viz_themes[viz->theme];
		SDL_SetRenderDrawColor(viz->renderer, theme_colors->bg_r, theme_colors->bg_g, theme_colors->bg_b, 255);
		SDL_RenderClear(viz->renderer);

		if (viz->mode == VIZ_MODE_WAVEFORM) {
			render_waveform(viz);
		}

		SDL_RenderPresent(viz->renderer);
	}

	pthread_mutex_unlock(&g_visualizers_mutex);
}


/******************************************************************************
 * PROLOG INTERFACE
 *****************************************************************************/

/*
 * pl_visualizer_attach()
 * visualizer_attach(+Source, +BeforeEffects, -Visualizer)
 * Attaches a visualizer to an audio source and opens the window.
 * BeforeEffects: true = before effects, false = after effects
 */
static foreign_t pl_visualizer_attach(
		term_t source_term,
		term_t before_effects_term,
		term_t handle_term)
{
	int slot;
	int before_effects;
	visualizer_node_t *viz;
	ma_node *source_node;
	effect_node_t *chain;
	ma_node_config config;
	ma_uint32 channels;
	ma_uint32 sample_rate;
	ma_format format;
	ma_result result;
	ma_node *attach_point;
	ma_node *current_output;

	ENSURE_ENGINE_INITIALIZED();

	if (!get_source_from_term(source_term, &source_node, &chain)) {
		return PL_existence_error("source", source_term);
	}

	if (!PL_get_bool(before_effects_term, &before_effects)) {
		return PL_type_error("boolean", before_effects_term);
	}

	pthread_mutex_lock(&g_visualizers_mutex);

	slot = allocate_visualizer_slot();
	if (slot < 0) {
		pthread_mutex_unlock(&g_visualizers_mutex);
		return PL_resource_error("visualizer_slots");
	}

	viz = &g_visualizers[slot];
	get_engine_format_info(&format, &channels, &sample_rate);

	/* initialize node */
	config = ma_node_config_init();
	config.vtable = &visualizer_vtable;
	config.pInputChannels = &channels;
	config.pOutputChannels = &channels;

	result = ma_node_init(ma_engine_get_node_graph(g_engine), &config, NULL, &viz->base);
	if (result != MA_SUCCESS) {
		free_visualizer_slot(slot);
		pthread_mutex_unlock(&g_visualizers_mutex);
		return PL_resource_error("node_init");
	}

	/* initialize ring buffer */
	result = ring_buffer_init(&viz->buffer, VIZ_BUFFER_FRAMES, channels, format);
	if (result != MA_SUCCESS) {
		ma_node_uninit(&viz->base, NULL);
		free_visualizer_slot(slot);
		pthread_mutex_unlock(&g_visualizers_mutex);
		return PL_resource_error("memory");
	}

	/* determine attach point based on before_effects */
	if (before_effects || chain == NULL) {
		attach_point = source_node;
	} else {
		attach_point = (ma_node *)get_effect_chain_tail(chain);
	}

	/* insert visualizer: attach_point -> viz -> endpoint */
	current_output = ma_node_graph_get_endpoint(ma_engine_get_node_graph(g_engine));

	result = ma_node_attach_output_bus(attach_point, 0, &viz->base, 0);
	if (result != MA_SUCCESS) {
		ring_buffer_free(&viz->buffer);
		ma_node_uninit(&viz->base, NULL);
		free_visualizer_slot(slot);
		pthread_mutex_unlock(&g_visualizers_mutex);
		return PL_resource_error("node_attach");
	}

	result = ma_node_attach_output_bus(&viz->base, 0, current_output, 0);
	if (result != MA_SUCCESS) {
		ma_node_attach_output_bus(attach_point, 0, current_output, 0);
		ring_buffer_free(&viz->buffer);
		ma_node_uninit(&viz->base, NULL);
		free_visualizer_slot(slot);
		pthread_mutex_unlock(&g_visualizers_mutex);
		return PL_resource_error("node_attach");
	}

	/* create SDL window */
	viz->window = SDL_CreateWindow("promini visualizer", 640, 480,
			SDL_WINDOW_RESIZABLE);
	if (viz->window == NULL) {
		ma_node_attach_output_bus(attach_point, 0, current_output, 0);
		ring_buffer_free(&viz->buffer);
		ma_node_uninit(&viz->base, NULL);
		free_visualizer_slot(slot);
		pthread_mutex_unlock(&g_visualizers_mutex);
		return FALSE;
	}

	viz->renderer = SDL_CreateRenderer(viz->window, NULL);
	if (viz->renderer == NULL) {
		SDL_DestroyWindow(viz->window);
		ma_node_attach_output_bus(attach_point, 0, current_output, 0);
		ring_buffer_free(&viz->buffer);
		ma_node_uninit(&viz->base, NULL);
		free_visualizer_slot(slot);
		pthread_mutex_unlock(&g_visualizers_mutex);
		return FALSE;
	}

	/* initialize display state */
	viz->mode = VIZ_MODE_WAVEFORM;
	viz->triggered = MA_FALSE;
	viz->zoom = 1.0f;
	viz->amp_scale = 1.0f;
	viz->smoothing = 0.8f;
	viz->log_freq = MA_FALSE;
	viz->db_scale = MA_FALSE;
	viz->paused = MA_FALSE;
	viz->theme = 0;

	pthread_mutex_unlock(&g_visualizers_mutex);

	return unify_typed_handle(handle_term, "visualizer", slot);
}

/*
 * pl_visualizer_detach()
 * visualizer_detach(+Visualizer)
 * Detaches visualizer from source and closes window.
 */
static foreign_t pl_visualizer_detach(term_t handle_term)
{
	int slot;
	visualizer_node_t *viz;

	if (!get_typed_handle(handle_term, "visualizer", &slot)) {
		return PL_type_error("visualizer", handle_term);
	}

	pthread_mutex_lock(&g_visualizers_mutex);

	if (slot < 0 || slot >= MAX_VISUALIZERS || !g_visualizers[slot].in_use) {
		pthread_mutex_unlock(&g_visualizers_mutex);
		return PL_existence_error("visualizer", handle_term);
	}

	viz = &g_visualizers[slot];
	cleanup_visualizer(viz);
	free_visualizer_slot(slot);

	pthread_mutex_unlock(&g_visualizers_mutex);

	return TRUE;
}

/******************************************************************************
 * MODULE REGISTRATION
 *****************************************************************************/

/*
 * visualizer_register_predicates()
 * Register visualizer-related Prolog predicates.
 */
install_t visualizer_register_predicates(void)
{
	PL_register_foreign("visualizer_attach", 3, pl_visualizer_attach, 0);
	PL_register_foreign("visualizer_detach", 1, pl_visualizer_detach, 0);
}

/*
 * uninstall_visualizer()
 * Cleanup visualizer resources.
 */
install_t uninstall_visualizer(void)
{
	int i;

	pthread_mutex_lock(&g_visualizers_mutex);

	for (i = 0; i < MAX_VISUALIZERS; i++) {
		if (g_visualizers[i].in_use) {
			cleanup_visualizer(&g_visualizers[i]);
			g_visualizers[i].in_use = MA_FALSE;
		}
	}

	pthread_mutex_unlock(&g_visualizers_mutex);
}
