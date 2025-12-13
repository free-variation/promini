/*
 * clock.c - Clock and tempo synchronization for promini
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#include "promini.h"

/******************************************************************************
 * GLOBAL VARIABLES
 *****************************************************************************/

promini_clock_t g_clock = {0};
clock_route_t g_clock_routes[MAX_CLOCK_ROUTES] = {{0}};


/******************************************************************************
 * SLOT MANAGEMENT
 *****************************************************************************/

/*
 * allocate_clock_route_slot()
 * Finds a free clock route slot and marks it as in use.
 * Returns slot index, or -1 if all slots are full.
 */
static int allocate_clock_route_slot(void)
{
	int i;
	for (i = 0; i < MAX_CLOCK_ROUTES; i++) {
		if (!g_clock_routes[i].in_use) {
			g_clock_routes[i].in_use = MA_TRUE;
			return i;
		}
	}
	return -1;
}

/*
 * free_clock_route_slot()
 * Frees a clock route slot.
 */
static void free_clock_route_slot(int index)
{
	if (index >= 0 && index < MAX_CLOCK_ROUTES) {
		g_clock_routes[index].in_use = MA_FALSE;
	}
}

/******************************************************************************
 * CLOCK PROCESSING
 *****************************************************************************/

/*
 * clock_init()
 * Initialize the global clock sound.
 */
void clock_init(ma_uint32 sample_rate)
{
	ma_waveform_config wf_config;
	ma_waveform* waveform;

	/* Allocate waveform */
	waveform = (ma_waveform*)malloc(sizeof(ma_waveform));

	wf_config = ma_waveform_config_init(
		ma_format_f32,
		1,
		sample_rate,
		ma_waveform_type_square,
		1.0,
		(CLOCK_DEFAULT_BPM * CLOCK_PPQN) / 60.0
	);
	ma_waveform_init(&wf_config, waveform);

	/* Create sound from waveform */
	ma_sound_init_from_data_source(
		g_engine,
		waveform,
		MA_SOUND_FLAG_NO_SPATIALIZATION,
		NULL,
		&g_clock.sound
	);

	/* Set volume to 0 (silent) */
	ma_sound_set_volume(&g_clock.sound, 0.0f);

	/* Start the sound so it keeps ticking */
	ma_sound_start(&g_clock.sound);

	g_clock.bpm = CLOCK_DEFAULT_BPM;
	g_clock.running = MA_FALSE;
}

/*
 * clock_uninit()
 * Cleanup clock resources.
 */
void clock_uninit(void)
{
	ma_waveform* wf;

	ma_sound_stop(&g_clock.sound);
	wf = (ma_waveform*)ma_sound_get_data_source(&g_clock.sound);
	ma_sound_uninit(&g_clock.sound);
	ma_waveform_uninit(wf);
	free(wf);
}


/******************************************************************************
 * HELPERS
 *****************************************************************************/

/*
 * set_bmp_from_term()
 * Extracts BPM from term, validates positive, sets on global clock.
 * Returns TRUE on success, raises appropriate error on failure.
 */
static foreign_t set_bpm_from_term(term_t term)
{
	double bpm;
	ma_waveform *wf;

	if (!PL_get_float(term, &bpm)) {
		int bpm_int;
		if (!PL_get_integer(term, &bpm_int)) {
			return PL_type_error("number", term);
		}
		bpm = (double)bpm_int;
	}

	if (bpm <= 0.0) {
		return PL_domain_error("positive_number", term);
	}

	g_clock.bpm = (float)bpm;
	wf = (ma_waveform*)ma_sound_get_data_source(&g_clock.sound);
	ma_waveform_set_frequency(wf, (bpm * CLOCK_PPQN) / 60.0);

	return TRUE;
}

/******************************************************************************
 * PROLOG INTERFACE
 *****************************************************************************/



/*
 * pl_clock_set_bpm()
 * clock_set_bpm(+BPM)
 * Sets the BPM of the clock.
 */
static foreign_t pl_clock_set_bpm(term_t bpm_term)
{
	return set_bpm_from_term(bpm_term);
}

/*
 * pl_clock_get_bpm()
 * clock_get_bpm(-BPM)
 * Gets the BPM of the clock.
 */
static foreign_t pl_clock_get_bpm(term_t bpm_term)
{

	return PL_unify_float(bpm_term, (double)g_clock.bpm);
}

/*
 * pl_clock_start()
 * clock_start()
 * Starts the clock running.
 */
static foreign_t pl_clock_start(void)
{
	g_clock.running = MA_TRUE;
	return TRUE;
}

/*
 * pl_clock_stop()
 * clock_stop
 * Stops the clock.
 */
static foreign_t pl_clock_stop(void)
{
	g_clock.running = MA_FALSE;
	return TRUE;
}

/*
 * pl_clock_is_running()
 * clock_is_running
 * Succeeds if clock is running.
 */
static foreign_t pl_clock_is_running(void)
{
	return g_clock.running ? TRUE : FALSE;
}

/*
 * pl_clock_get_beat_position()
 * clock_get_beat_position(-Position)
 * Gets the current beat position of the clock.
 */
static foreign_t pl_clock_get_beat_position(term_t position_term)
{
	ma_waveform *wf = (ma_waveform *)ma_sound_get_data_source(&g_clock.sound);
	return PL_unify_float(position_term, wf->time / CLOCK_PPQN);
}

/******************************************************************************
 * MODULE REGISTRATION
 *****************************************************************************/

/*
 * clock_register_predicates()
 * Register clock-related Prolog predicates.
 */
install_t clock_register_predicates(void)
{
	PL_register_foreign("clock_set_bpm", 1, pl_clock_set_bpm, 0);
	PL_register_foreign("clock_get_bpm", 1, pl_clock_get_bpm, 0);
	PL_register_foreign("clock_start", 0, pl_clock_start, 0);
	PL_register_foreign("clock_stop", 0, pl_clock_stop, 0);
	PL_register_foreign("clock_is_running", 0, pl_clock_is_running, 0);
	PL_register_foreign("clock_get_beat_position", 1, pl_clock_get_beat_position, 0);
}

/*
 * uninstall_clock()
 * Cleanup clock resources.
 */
install_t uninstall_clock(void)
{
	int i;

	pthread_mutex_lock(&g_mod_mutex);

	clock_uninit();

	for (i = 0; i < MAX_CLOCK_ROUTES; i++) {
		g_clock_routes[i].in_use = MA_FALSE;
	}

	pthread_mutex_unlock(&g_mod_mutex);
}
