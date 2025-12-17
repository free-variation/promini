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
	ma_waveform *waveform;

	/* Allocate waveform */
	waveform = (ma_waveform*)ma_malloc(sizeof(ma_waveform), NULL);

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
	ma_waveform *wf;

	ma_sound_stop(&g_clock.sound);
	wf = (ma_waveform*)ma_sound_get_data_source(&g_clock.sound);
	ma_sound_uninit(&g_clock.sound);
	ma_waveform_uninit(wf);
	ma_free(wf, NULL);
}

/*
 * update_clock_routes()
 * Updates clock routes of the specified type.
 * For CLOCK_ROUTE_SYNC: sets LFO frequency or delay time from BPM.
 * For CLOCK_ROUTE_PULSE: triggers targets on beat.
 */
void update_clock_routes(clock_route_type_t route_type)
{
	int i;
	clock_route_t *route;
	ma_waveform *wf;
	float freq;
	ma_uint32 delay_frames;
	ma_uint32 sample_rate;

	wf = (ma_waveform *)ma_sound_get_data_source(&g_clock.sound);
	sample_rate = ma_engine_get_sample_rate(g_engine);

	for (i = 0; i < MAX_CLOCK_ROUTES; i++) {
		route = &g_clock_routes[i];
		if (!route->in_use || route->route_type != route_type) continue;

		if (route_type == CLOCK_ROUTE_SYNC) {
			switch (route->target_type) {
				case CLOCK_TARGET_LFO:
					freq = (g_clock.bpm / 60.0f) * (CLOCK_PPQN / route->division);
					ma_waveform_set_frequency(&((mod_source_t *)route->target_slot)->source.waveform, freq);
					break;
				case CLOCK_TARGET_DELAY:
				case CLOCK_TARGET_PING_PONG_DELAY:
					delay_frames = (ma_uint32)((60.0f / g_clock.bpm) * (route->division / CLOCK_PPQN) * sample_rate);
					((ping_pong_delay_node_t *)route->target_slot)->target_delay_in_frames = delay_frames;
					break;
				default:
					break;
			}
		} else {
			if (floor(wf->time / route->division) > floor(g_clock.last_time / route->division)) {
				switch (route->target_type) {
					case CLOCK_TARGET_LFO:
						ma_waveform_seek_to_pcm_frame(&((mod_source_t *)route->target_slot)->source.waveform, 0);
						break;
					case CLOCK_TARGET_ENVELOPE:
						((mod_source_t *)route->target_slot)->source.envelope.stage = 0;
						((mod_source_t *)route->target_slot)->source.envelope.stage_progress = 0.0f;
						break;
					case CLOCK_TARGET_GRANULAR:
						trigger_grain((granular_delay_t *)route->target_slot);
						break;
					default:
						break;
				}
			}
		}
	}

	if (route_type == CLOCK_ROUTE_PULSE) {
		g_clock.last_time = wf->time;
	}
}


				
/******************************************************************************
 * HELPERS
 *****************************************************************************/

/*
 * set_bpm_from_term()
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

	update_clock_routes(CLOCK_ROUTE_SYNC);

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
	ma_waveform *wf = (ma_waveform *)ma_sound_get_data_source(&g_clock.sound);
	g_clock.last_time = wf->time;
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
	ma_waveform *wf = (ma_waveform *)ma_sound_get_data_source(&g_clock.sound);
	g_clock.running = MA_FALSE;
	ma_waveform_seek_to_pcm_frame(wf, 0);
	g_clock.last_time = 0.0;
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

/*
 * pl_clock_route_init()
 * clock_route_init(+TargetType, +Target, +RouteType, +Division, -Route)
 * Creates a clock route to a target with specified division.
 * TargetType: lfo, envelop, granular, delay, ping_pong_delay
 * RouteType: pulse, sync
 */
static foreign_t pl_clock_route_init(
		term_t target_type_term, term_t target_term,
		term_t route_type_term, term_t division_term,
		term_t route_term)
{
	atom_t target_type_atom;
  	atom_t route_type_atom;
  	double division;
  	int slot;
  	int target_slot;
  	clock_route_t *route;
  	clock_target_type_t target_type;
  	clock_route_type_t route_type;
  	void *target_ptr;

  	if (!PL_get_atom(target_type_term, &target_type_atom)) {
  		return PL_type_error("atom", target_type_term);
  	}
  	if (!PL_get_atom(route_type_term, &route_type_atom)) {
  		return PL_type_error("atom", route_type_term);
  	}
  	if (!PL_get_float(division_term, &division)) {
  		int div_int;
  		if (!PL_get_integer(division_term, &div_int)) {
  			return PL_type_error("number", division_term);
  		}
  		division = (double)div_int;
  	}

  	/* Parse target type */
  	if (target_type_atom == PL_new_atom("lfo")) {
  		target_type = CLOCK_TARGET_LFO;
  		if (!get_typed_handle(target_term, "mod_source", &target_slot)) {
  			return PL_type_error("mod_source", target_term);
  		}
  		target_ptr = &g_mod_sources[target_slot];
  	} else if (target_type_atom == PL_new_atom("envelope")) {
  		target_type = CLOCK_TARGET_ENVELOPE;
  		if (!get_typed_handle(target_term, "mod_source", &target_slot)) {
  			return PL_type_error("mod_source", target_term);
  		}
  		target_ptr = &g_mod_sources[target_slot];
  	} else if (target_type_atom == PL_new_atom("granular")) {
  		target_type = CLOCK_TARGET_GRANULAR;
  		if (!get_typed_handle(target_term, "granular", &target_slot)) {
  			return PL_type_error("granular", target_term);
  		}
  		target_ptr = &g_granular_delays[target_slot];
  	} else if (target_type_atom == PL_new_atom("delay")) {
  		target_type = CLOCK_TARGET_DELAY;
  		target_ptr = get_effect_pointer(target_term);
  		if (target_ptr == NULL) {
  			return PL_type_error("effect", target_term);
  		}
  	} else if (target_type_atom == PL_new_atom("ping_pong_delay")) {
  		target_type = CLOCK_TARGET_PING_PONG_DELAY;
  		target_ptr = get_effect_pointer(target_term);
  		if (target_ptr == NULL) {
  			return PL_type_error("effect", target_term);
  		}
  	} else {
  		return PL_domain_error("target_type", target_type_term);
  	}

  	/* Parse route type */
  	if (route_type_atom == PL_new_atom("pulse")) {
  		route_type = CLOCK_ROUTE_PULSE;
  	} else if (route_type_atom == PL_new_atom("sync")) {
  		route_type = CLOCK_ROUTE_SYNC;
  	} else {
  		return PL_domain_error("route_type", route_type_term);
  	}

  	slot = allocate_clock_route_slot();
  	if (slot < 0) {
  		return PL_resource_error("clock_route_slots");
  	}

  	route = &g_clock_routes[slot];
  	route->target_type = target_type;
  	route->target_slot = target_ptr;
  	route->route_type = route_type;
  	route->division = (float)division;
  	route->phase_offset = 0.0f;

  	/* For sync routes, update immediately */
  	if (route_type == CLOCK_ROUTE_SYNC) {
  		update_clock_routes(CLOCK_ROUTE_SYNC);
  	}

  	return unify_typed_handle(route_term, "clock_route", slot);
}

/*
 * pl_clock_route_uninit()
 * clock_route_uninit(+Route)
 * Removes a clock route.
 */
static foreign_t pl_clock_route_uninit(term_t route_term)
{
	int slot;

	if (!get_typed_handle(route_term, "clock_route", &slot)) {
		return PL_type_error("clock_route", route_term);
	}
	if (slot < 0 || slot >= MAX_CLOCK_ROUTES || !g_clock_routes[slot].in_use) {
		return PL_existence_error("clock_route", route_term);
	}

	free_clock_route_slot(slot);
	return TRUE;
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
	PL_register_foreign("clock_route_init", 5, pl_clock_route_init, 0);
	PL_register_foreign("clock_route_uninit", 1, pl_clock_route_uninit, 0);
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
