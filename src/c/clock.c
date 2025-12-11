/*
 * clock.c - Clock and tempo synchronization for promini
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#include "promini.h"

/******************************************************************************
 * GLOBAL VARIABLES
 *****************************************************************************/

promini_clock_t g_clocks[MAX_CLOCKS] = {{0}};
clock_route_t g_clock_routes[MAX_CLOCK_ROUTES] = {{0}};

/******************************************************************************
 * SLOT MANAGEMENT
 *****************************************************************************/

/*
 * allocate_clock_slot()
 * Finds a free clock slot and marks it as in use.
 * Returns slot index, or -1 if all slots are full.
 */
static int allocate_clock_slot(void)
{
	int i;
	for (i = 0; i < MAX_CLOCKS; i++) {
		if (!g_clocks[i].in_use) {
			g_clocks[i].in_use = MA_TRUE;
			g_clocks[i].bpm = 120.0f;
			g_clocks[i].running = MA_FALSE;
			g_clocks[i].beat_position = 0.0;
			return i;
		}
	}
	return -1;
}

/*
 * free_clock_slot()
 * Frees a clock slot.
 */
static void free_clock_slot(int index)
{
	if (index >= 0 && index < MAX_CLOCKS) {
		g_clocks[index].in_use = MA_FALSE;
	}
}

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
 * MODULE REGISTRATION
 *****************************************************************************/

/*
 * clock_register_predicates()
 * Register clock-related Prolog predicates.
 */
install_t clock_register_predicates(void)
{
	/* predicates will be added incrementally */
}

/*
 * uninstall_clock()
 * Cleanup clock resources.
 */
install_t uninstall_clock(void)
{
	int i;

	pthread_mutex_lock(&g_mod_mutex);

	for (i = 0; i < MAX_CLOCKS; i++) {
		g_clocks[i].in_use = MA_FALSE;
	}
	for (i = 0; i < MAX_CLOCK_ROUTES; i++) {
		g_clock_routes[i].in_use = MA_FALSE;
	}

	pthread_mutex_unlock(&g_mod_mutex);
}
