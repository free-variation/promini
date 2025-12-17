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
	Uint8 mid_r, mid_g, mid_b;
	Uint8 fg_r, fg_g, fg_b;
} viz_theme_t;

static const viz_theme_t viz_themes[] = {
	{ 32, 32, 32,     128, 64, 64,     255, 255, 255 },  /* default: gray → dull red → white */
	{ 0, 0, 0,        0, 128, 0,       0, 255, 0 },      /* osc: black → dark green → green */
	{ 180, 200, 220,  90, 100, 110,    0, 0, 0 }         /* blueprint: light blue → mid → black */
};

#define VIZ_THEME_COUNT (sizeof(viz_themes) / sizeof(viz_themes[0]))

/*
 * theme_gradient()
 * Compute RGB color from theme based on intensity (0.0 to 1.0).
 * 0.0 = bg, 0.5 = mid, 1.0 = fg
 */
static void theme_gradient(const viz_theme_t *th, float intensity, Uint8 *r, Uint8 *g, Uint8 *b)
{
	float t;

	if (intensity <= 0.5f) {
		t = intensity * 2.0f;
		*r = (Uint8)(th->bg_r + t * (th->mid_r - th->bg_r));
		*g = (Uint8)(th->bg_g + t * (th->mid_g - th->bg_g));
		*b = (Uint8)(th->bg_b + t * (th->mid_b - th->bg_b));
	} else {
		t = (intensity - 0.5f) * 2.0f;
		*r = (Uint8)(th->mid_r + t * (th->fg_r - th->mid_r));
		*g = (Uint8)(th->mid_g + t * (th->fg_g - th->mid_g));
		*b = (Uint8)(th->mid_b + t * (th->fg_b - th->mid_b));
	}
}

/* forward declarations */
static void fft_free(fft_state_t *fft);

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
	fft_free(&viz->fft);
	if (viz->smoothed_magnitudes[0]) ma_free(viz->smoothed_magnitudes[0], NULL);
	if (viz->smoothed_magnitudes[1]) ma_free(viz->smoothed_magnitudes[1], NULL);
	if (viz->waterfall[0]) ma_free(viz->waterfall[0], NULL);
	if (viz->waterfall[1]) ma_free(viz->waterfall[1], NULL);
	viz->smoothed_magnitudes[0] = NULL;
	viz->smoothed_magnitudes[1] = NULL;
	viz->waterfall[0] = NULL;
	viz->waterfall[1] = NULL;
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
					viz->mode = (viz->mode + 1) % 3;
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
					if (viz->zoom > 16.0f) viz->zoom = 16.0f;
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
 * FFT IMPLEMENTATION
 *****************************************************************************/

/* 
 * fft_init()
 * Initialize FFT state with given size.
 * Creates the Hann window.
 */
static ma_result fft_init(fft_state_t *fft, ma_uint32 size)
{
	ma_uint32 i;

	fft->fft_size = size;
	fft->real = (float *)ma_malloc(size * sizeof(float), NULL);
	fft->imag = (float *)ma_malloc(size * sizeof(float), NULL);
	fft->magnitudes = (float *)ma_malloc((size / 2) * sizeof(float), NULL);
	fft->window = (float *)ma_malloc(size * sizeof(float), NULL);

	if (!fft->real || !fft->imag || !fft->magnitudes || !fft->window) {
  		ma_free(fft->real, NULL);
  		ma_free(fft->imag, NULL);
  		ma_free(fft->magnitudes, NULL);
  		ma_free(fft->window, NULL);
  		return MA_OUT_OF_MEMORY;
  	}

	/* Hann window */
	for (i = 0; i < size; i++) {
		fft->window[i] = 0.5f * (1.0f - cosf(2.0f * M_PI * i / (size - 1)));
	}

	return MA_SUCCESS;
}

/*
 * fft_free()
 * Free FFT state buffers.
 */
static void fft_free(fft_state_t *fft)
{
	if (fft->real) ma_free(fft->real, NULL);
	if (fft->imag) ma_free(fft->imag, NULL);
	if (fft->magnitudes) ma_free(fft->magnitudes, NULL);
	if (fft->window) ma_free(fft->window, NULL);
	fft->real = NULL;
	fft->imag = NULL;
	fft->magnitudes = NULL;
	fft->window = NULL;
}

/*
 * fft_compute()
 * Compute FFT on input samples, store magnitudes.
 * Input is windowed before transform.
 */
static void fft_compute(fft_state_t *fft, const float *input)
{
	ma_uint32 n = fft->fft_size;
	ma_uint32 i, j, k, m, step;
	float tr, ti, wr, wi, theta;

	/* apply window and copy to real, zero imag */
	for (i = 0; i < n; i++) {
		fft->real[i] = input[i] * fft->window[i];
		fft->imag[i] = 0.0f;
	}

	/*
	 * Bit-reversal permutation: reorder samples so that after the
	 * iterative FFT, results end up in natural order. Index j counts
	 * in bit-reversed fashion (e.g., for n=8: 0,4,2,6,1,5,3,7).
	 */
	j = 0;
	for (i = 0; i < n - 1; i++) {
		if (i < j) {
			tr = fft->real[i];
			fft->real[i] = fft->real[j];
			fft->real[j] = tr;
			ti = fft->imag[i];
			fft->imag[i] = fft->imag[j];
			fft->imag[j] = ti;
		}
		k = n >> 1;
		while (k <= j) {
			j -= k;
			k >>= 1;
		}
		j += k;
	}

	/*
	 * Cooley-Tukey radix-2 decimation-in-time FFT.
	 * Process in stages: step doubles each stage (2, 4, 8, ..., n).
	 * Each stage combines pairs of smaller DFTs using "butterfly" ops:
	 *   X[i]   = X[i] + W * X[j]
	 *   X[j]   = X[i] - W * X[j]
	 * where W = e^(-j*2*pi*m/step) is the "twiddle factor".
	 */
	for (step = 2; step <= n; step <<= 1) {
		theta = -2.0f * M_PI / step;
		for (m = 0; m < step / 2; m++) {
			wr = cosf(theta * m);
			wi = sinf(theta * m);
			for (i = m; i < n; i += step) {
				j = i + step / 2;
				tr = wr * fft->real[j] - wi * fft->imag[j];
				ti = wr * fft->imag[j] + wi * fft->real[j];
				fft->real[j] = fft->real[i] - tr;
				fft->imag[j] = fft->imag[i] - ti;
				fft->real[i] += tr;
				fft->imag[i] += ti;
			}
		}
	}

	/*
	 * Compute magnitudes for positive frequencies only (bins 0 to n/2-1).
	 * Divide by n/2 to normalize: a unit sine wave produces magnitude ~1.0.
	 */
	for (i = 0; i < n / 2; i++) {
		fft->magnitudes[i] = sqrtf(fft->real[i] * fft->real[i] + fft->imag[i] * fft->imag[i]) / (n / 2);
	}
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
 * render_spectrum()
 * Draw spectrum analyzer for one visualizer.
 * Stereo: L top half, R bottom half.
 * X-axis = frequency, Y-axis = amplitude.
 */
static void render_spectrum(visualizer_node_t *viz)
{
	int win_w, win_h;
	int half_h;
	ma_uint32 fft_size = viz->fft.fft_size;
	ma_uint32 num_bins = fft_size / 2;
	ma_uint32 max_bin;
	ma_uint32 highest_active;
	float target_max;
	float *samples;
	float fft_input[1024];
	ma_uint64 read_start;
	ma_uint32 i, bin;
	int x, x1, x2;
	float mag, db, height;
	const viz_theme_t *theme_colors;
	int ch;

	SDL_GetWindowSize(viz->window, &win_w, &win_h);
	half_h = win_h / 2;
	samples = viz->buffer.samples;
	theme_colors = &viz_themes[viz->theme];

	/* get samples for FFT from ring buffer */
	read_start = (viz->buffer.write_pos + viz->buffer.capacity_frames - fft_size)
		% viz->buffer.capacity_frames;

	/* process each channel */
	for (ch = 0; ch < 2; ch++) {
		int y_base = ch * half_h + half_h;

		/* extract mono samples for this channel */
		for (i = 0; i < fft_size; i++) {
			ma_uint64 idx = ((read_start + i) % viz->buffer.capacity_frames) * viz->buffer.channels + ch;
			fft_input[i] = samples[idx];
		}

		/* compute FFT */
		fft_compute(&viz->fft, fft_input);

		/* smooth magnitudes */
		for (bin = 0; bin < num_bins; bin++) {
			mag = viz->fft.magnitudes[bin];
			viz->smoothed_magnitudes[ch][bin] =
				viz->smoothing * viz->smoothed_magnitudes[ch][bin] +
				(1.0f - viz->smoothing) * mag;
		}

		/*
		 * Auto-range: adjust display to fit content.
		 * Find highest bin with energy > threshold, add 25% headroom,
		 * then smooth (95% old + 5% new) to avoid jittery zooming.
		 */
		if (ch == 0) {
			highest_active = 3;
			for (bin = 3; bin < num_bins; bin++) {
				if (viz->smoothed_magnitudes[0][bin] > 0.01f) {
					highest_active = bin;
				}
			}
			target_max = (float)highest_active * 1.25f;
			if (target_max < 16.0f) target_max = 16.0f;
			viz->auto_max_bin = viz->auto_max_bin * 0.95f + target_max * 0.05f;
			max_bin = (ma_uint32)viz->auto_max_bin;
			if (max_bin > num_bins) max_bin = num_bins;
		}

		/* draw spectrum bars - iterate over bins, skip DC */
		for (bin = 1; bin < max_bin; bin++) {
			Uint8 r, g, b;
			float norm_height;
			float freq_ratio;

			mag = viz->smoothed_magnitudes[ch][bin];

			/* convert to height */
			if (viz->db_scale) {
				db = 20.0f * log10f(mag + 1e-6f);
				height = (db + 60.0f) / 60.0f;
			} else {
				height = mag * 2.0f;
			}
			norm_height = CLAMP(height, 0.0f, 1.0f);
			height = norm_height * half_h;

			/*
			 * Map bin to x position. Log mode spreads low frequencies
			 * across more pixels (matches human pitch perception).
			 * log(bin)/log(max) maps bin 1->0, bin max->1.
			 */
			if (viz->log_freq) {
				freq_ratio = logf((float)bin) / logf((float)max_bin);
				x1 = (int)(freq_ratio * win_w);
				freq_ratio = logf((float)(bin + 1)) / logf((float)max_bin);
				x2 = (int)(freq_ratio * win_w);
			} else {
				x1 = bin * win_w / max_bin;
				x2 = (bin + 1) * win_w / max_bin;
			}
			if (x2 <= x1) x2 = x1 + 1;

			/* color based on intensity */
			theme_gradient(theme_colors, norm_height, &r, &g, &b);
			SDL_SetRenderDrawColor(viz->renderer, r, g, b, 255);

			/* draw bar */
			for (x = x1; x < x2 && x < win_w; x++) {
				SDL_RenderLine(viz->renderer, (float)x, (float)y_base,
					(float)x, (float)(y_base - height));
			}
		}
	}
}


/*
 * render_spectrogram()
 * Draw waterfall spectrogram for one visualizer.
 * Stereo: L top half, R bottom half.
 */
static void render_spectrogram(visualizer_node_t *viz)
{
	int win_w, win_h;
	int half_h;
	ma_uint32 fft_size = viz->fft.fft_size;
	ma_uint32 num_bins = fft_size / 2;
	ma_uint32 max_bin;
	ma_uint32 highest_active;
	float target_max;
	float *samples;
	float fft_input[1024];
	ma_uint64 read_start;
	ma_uint32 i, row, bin;
	int x, x1, x2, y;
	float mag, db, intensity;
	const viz_theme_t *theme_colors;
	int ch;

	SDL_GetWindowSize(viz->window, &win_w, &win_h);
	half_h = win_h / 2;
	samples = viz->buffer.samples;

	/* get samples for FFT from ring buffer */
	read_start = (viz->buffer.write_pos + viz->buffer.capacity_frames - fft_size)
		% viz->buffer.capacity_frames;

	/* process each channel */
	for (ch = 0; ch < 2; ch++) {
		/* extract mono samples for this channel */
		for (i = 0; i < fft_size; i++) {
			ma_uint64 idx = ((read_start + i) % viz->buffer.capacity_frames) * viz->buffer.channels + ch;
			fft_input[i] = samples[idx];
		}

		/* compute FFT */
		fft_compute(&viz->fft, fft_input);

		/* smooth and store in waterfall */
		for (bin = 0; bin < num_bins; bin++) {
			mag = viz->fft.magnitudes[bin];
			viz->smoothed_magnitudes[ch][bin] =
				viz->smoothing * viz->smoothed_magnitudes[ch][bin] +
				(1.0f - viz->smoothing) * mag;
		}

		/*
		 * Auto-range: adjust display to fit content.
		 * Find highest bin with energy > threshold, add 25% headroom,
		 * then smooth (95% old + 5% new) to avoid jittery zooming.
		 */
		if (ch == 0) {
			highest_active = 1;
			for (bin = 1; bin < num_bins; bin++) {
				if (viz->smoothed_magnitudes[0][bin] > 0.01f) {
					highest_active = bin;
				}
			}
			target_max = (float)highest_active * 1.25f;
			if (target_max < 16.0f) target_max = 16.0f;
			viz->auto_max_bin = viz->auto_max_bin * 0.95f + target_max * 0.05f;
			max_bin = (ma_uint32)viz->auto_max_bin;
			if (max_bin > num_bins) max_bin = num_bins;
		}

		/* copy current spectrum to circular waterfall buffer */
		memcpy(&viz->waterfall[ch][viz->waterfall_row * num_bins],
		       viz->smoothed_magnitudes[ch],
		       num_bins * sizeof(float));
	}

	/* advance waterfall row */
	viz->waterfall_row = (viz->waterfall_row + 1) % VIZ_WATERFALL_ROWS;

	/* render waterfall */
	theme_colors = &viz_themes[viz->theme];

	for (ch = 0; ch < 2; ch++) {
		int y_offset = ch * half_h;

		for (row = 0; row < VIZ_WATERFALL_ROWS && (int)row < half_h; row++) {
			/*
			 * Map display row to circular buffer row. Row 0 (top of display)
			 * shows newest data (waterfall_row - 1), scrolling down to older.
			 */
			ma_uint32 buf_row = (viz->waterfall_row + VIZ_WATERFALL_ROWS - 1 - row) % VIZ_WATERFALL_ROWS;
			y = y_offset + row;

			/* iterate over bins */
			for (bin = 1; bin < max_bin; bin++) {
				Uint8 r, g, b;
				float freq_ratio;

				mag = viz->waterfall[ch][buf_row * num_bins + bin];

				/* convert to intensity */
				if (viz->db_scale) {
					db = 20.0f * log10f(mag + 1e-6f);
					intensity = (db + 60.0f) / 60.0f;
				} else {
					intensity = mag * 2.0f;
				}
				intensity = CLAMP(intensity, 0.0f, 1.0f);

				/* map bin to x position */
				if (viz->log_freq) {
					freq_ratio = logf((float)bin) / logf((float)max_bin);
					x1 = (int)(freq_ratio * win_w);
					freq_ratio = logf((float)(bin + 1)) / logf((float)max_bin);
					x2 = (int)(freq_ratio * win_w);
				} else {
					x1 = bin * win_w / max_bin;
					x2 = (bin + 1) * win_w / max_bin;
				}
				if (x2 <= x1) x2 = x1 + 1;

				/* color based on intensity */
				theme_gradient(theme_colors, intensity, &r, &g, &b);
				SDL_SetRenderDrawColor(viz->renderer, r, g, b, 255);

				/* draw horizontal segment for this bin */
				for (x = x1; x < x2 && x < win_w; x++) {
					SDL_RenderPoint(viz->renderer, (float)x, (float)y);
				}
			}
		}
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

		switch (viz->mode) {
			case VIZ_MODE_WAVEFORM:
				render_waveform(viz);
				break;
			case VIZ_MODE_SPECTRUM:
				render_spectrum(viz);
				break;
			case VIZ_MODE_SPECTROGRAM:
				render_spectrogram(viz);
				break;
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
	viz->zoom = 4.0f;
	viz->amp_scale = 1.0f;
	viz->smoothing = 0.8f;
	viz->auto_max_bin = 128.0f;
	viz->log_freq = MA_FALSE;
	viz->db_scale = MA_FALSE;
	viz->paused = MA_FALSE;
	viz->theme = 0;

	/* initialize FFT */
	result = fft_init(&viz->fft, 1024);
	if (result != MA_SUCCESS) {
		cleanup_visualizer(viz);
		free_visualizer_slot(slot);
		pthread_mutex_unlock(&g_visualizers_mutex);
		return PL_resource_error("memory");
	}

	/* allocate smoothed magnitudes and waterfall buffers */
	viz->smoothed_magnitudes[0] = (float *)ma_malloc((viz->fft.fft_size / 2) * sizeof(float), NULL);
	viz->smoothed_magnitudes[1] = (float *)ma_malloc((viz->fft.fft_size / 2) * sizeof(float), NULL);
	viz->waterfall[0] = (float *)ma_malloc(VIZ_WATERFALL_ROWS * (viz->fft.fft_size / 2) * sizeof(float), NULL);
	viz->waterfall[1] = (float *)ma_malloc(VIZ_WATERFALL_ROWS * (viz->fft.fft_size / 2) * sizeof(float), NULL);

	if (!viz->smoothed_magnitudes[0] || !viz->smoothed_magnitudes[1] ||
	    !viz->waterfall[0] || !viz->waterfall[1]) {
		cleanup_visualizer(viz);
		free_visualizer_slot(slot);
		pthread_mutex_unlock(&g_visualizers_mutex);
		return PL_resource_error("memory");
	}

	memset(viz->smoothed_magnitudes[0], 0, (viz->fft.fft_size / 2) * sizeof(float));
	memset(viz->smoothed_magnitudes[1], 0, (viz->fft.fft_size / 2) * sizeof(float));
	memset(viz->waterfall[0], 0, VIZ_WATERFALL_ROWS * (viz->fft.fft_size / 2) * sizeof(float));
	memset(viz->waterfall[1], 0, VIZ_WATERFALL_ROWS * (viz->fft.fft_size / 2) * sizeof(float));
	viz->waterfall_row = 0;

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
