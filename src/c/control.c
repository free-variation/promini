/*
 * control.c - Gamepad/controller input
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#include "promini.h"
#include <poll.h>
#include <unistd.h>

/******************************************************************************
 * GLOBAL VARIABLES
 *****************************************************************************/

static ma_bool32 g_control_initialized = MA_FALSE;
static PL_dispatch_hook_t g_old_dispatch_hook = NULL;

/******************************************************************************
 * DISPATCH HOOK
 *****************************************************************************/

/*
 * dispatch_sdl_events()
 * Called by Prolog while REPL waits for input.
 * Pumps SDL events on the main thread.
 */
static int dispatch_sdl_events(int fd)
{
	struct pollfd fds[1];

	if (g_control_initialized) {
		SDL_PumpEvents();
	}

	/* Check if fd has input */
	fds[0].fd = fd;
	fds[0].events = POLLIN;

	if (poll(fds, 1, 10) > 0) {
		if (fds[0].revents & POLLIN) {
			return PL_DISPATCH_INPUT;
		}
	}

	return PL_DISPATCH_TIMEOUT;
}

/******************************************************************************
 * HELPER FUNCTIONS
 *****************************************************************************/

static struct {
	const char *name;
	SDL_GamepadAxis axis;
} axis_map[] = {
	{"left_x", SDL_GAMEPAD_AXIS_LEFTX},
	{"left_y", SDL_GAMEPAD_AXIS_LEFTY},
	{"right_x", SDL_GAMEPAD_AXIS_RIGHTX},
	{"right_y", SDL_GAMEPAD_AXIS_RIGHTY},
	{"left_trigger", SDL_GAMEPAD_AXIS_LEFT_TRIGGER},
	{"right_trigger", SDL_GAMEPAD_AXIS_RIGHT_TRIGGER},
	{NULL, 0}
};

/*
 * get_axis_from_atom()
 * Convert Prolog atom to SDL_GamepadAxis.
 * Returns 1 on success, 0 on failure.
 */
int get_axis_from_atom(atom_t atom, SDL_GamepadAxis *axis)
{
	const char *name;
	int i;

	name = PL_atom_chars(atom);
	for (i = 0; axis_map[i].name != NULL; i++) {
		if (strcmp(name, axis_map[i].name) == 0) {
			*axis = axis_map[i].axis;
			return 1;
		}
	}
	return 0;
}

/*
 * get_gamepad_ptr()
 * Extract SDL_Gamepad pointer from control(Ptr) term.
 * Returns NULL on failure.
 */
SDL_Gamepad *get_gamepad_ptr(term_t gamepad_term)
{
	term_t ptr_term;
	void *ptr;
	atom_t name;
	size_t arity;

	if (!PL_get_name_arity(gamepad_term, &name, &arity)) return NULL;
	if (arity != 1) return NULL;

	ptr_term = PL_new_term_ref();
	if (!PL_get_arg(1, gamepad_term, ptr_term)) return NULL;
	if (!PL_get_pointer(ptr_term, &ptr)) return NULL;

	return (SDL_Gamepad *)ptr;
}

/*
 * get_dpad_axis()
 * Read d-pad buttons and return virtual axis value [-1, 0, or 1].
 * and sets *value to -1, 0, or 1.  Returns 0 if not a dpad axis, 1 otherwise.
 */
static float get_dpad_axis(SDL_Gamepad *gp, const char *axis_name, float *value)
{
	if (strcmp(axis_name, "dpad_x") == 0) {
		int left = SDL_GetGamepadButton(gp, SDL_GAMEPAD_BUTTON_DPAD_LEFT);
		int right = SDL_GetGamepadButton(gp, SDL_GAMEPAD_BUTTON_DPAD_RIGHT);
		*value = (float)(right - left);
		return 1;
	} else if (strcmp(axis_name, "dpad_y") == 0) {
		int down = SDL_GetGamepadButton(gp, SDL_GAMEPAD_BUTTON_DPAD_DOWN);
		int up = SDL_GetGamepadButton(gp, SDL_GAMEPAD_BUTTON_DPAD_UP);
		*value = (float)(down - up);
		return 1;
	}
	return 0;
}



/******************************************************************************
 * PROLOG PREDICATES
 *****************************************************************************/

/*
 * pl_control_init()
 * Initialize gamepad subsystem.
 */
static foreign_t pl_control_init(void)
{
	int i;

	if (g_control_initialized) return TRUE;

	/* 1. Force SDL to listen for input even without a focused window */
    SDL_SetHint(SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS, "1");

    /* 2. Initialize VIDEO along with GAMEPAD. 
       On macOS, this is often required to wake up the main runloop 
       so that IOKit callbacks can fire. */
    if (!SDL_Init(SDL_INIT_VIDEO | SDL_INIT_GAMEPAD)) {
        return FALSE;
    }

    /* 3. Pump events to catch the initial connection messages */
    /* A quick burst is usually enough once the subsystems are correct */
    for (i = 0; i < 10; i++) {
        SDL_PumpEvents();
        SDL_Delay(10);
    }

    /* 4. Install dispatch hook to pump events while REPL waits */
    g_old_dispatch_hook = PL_dispatch_hook(dispatch_sdl_events);

	g_control_initialized = MA_TRUE;
	return TRUE;
}

/*
 * pl_control_shutdown()
 * Free gamepad resources.
 */
static foreign_t pl_control_shutdown(void)
{
	if (!g_control_initialized) return TRUE;

	/* Restore old dispatch hook */
	PL_dispatch_hook(g_old_dispatch_hook);
	g_old_dispatch_hook = NULL;

	SDL_Quit();

	g_control_initialized = MA_FALSE;
	return TRUE;
}

/*
 * pl_control_gamepads()
 * control_gamepads(-Gamepads)
 * Returns list of connected gamepads as gamepad(Id, Name) terms.
 */
static foreign_t pl_control_gamepads(term_t gamepads_term)
{
	int count;
	int i;
	SDL_JoystickID *gamepads;
	term_t list;
	functor_t functor;

	if (!g_control_initialized) return FALSE;

	gamepads = SDL_GetGamepads(&count);
	if (gamepads == NULL) {
		return PL_unify_nil(gamepads_term);
	}

	list = PL_new_term_ref();
	PL_put_nil(list);
	functor = PL_new_functor(PL_new_atom("gamepad"), 2);

	for (i = count - 1; i >= 0; i--) {
		term_t gp = PL_new_term_ref();
		term_t args = PL_new_term_refs(2);

		if (!PL_put_integer(args+0, (int)gamepads[i])) {
			SDL_free(gamepads);
			return FALSE;
		}
		if (!PL_put_atom_chars(args+1, SDL_GetGamepadNameForID(gamepads[i]))) {
			SDL_free(gamepads);
			return FALSE;
		}
		if (!PL_cons_functor_v(gp, functor, args)) {
			SDL_free(gamepads);
			return FALSE;
		}
		if (!PL_cons_list(list, gp, list)) {
			SDL_free(gamepads);
			return FALSE;
		}
	}

	SDL_free(gamepads);
	return PL_unify(gamepads_term, list);
}

/*
 * pl_control_open()
 * control_open(+Id, -Gamepad)
 * Opens a gamepad and return control(Ptr) handle
 */
static foreign_t pl_control_open(term_t id_term, term_t gamepad_term)
{
	int id;
	SDL_Gamepad *gp;
	functor_t functor;
	term_t ptr_term;

	if (!g_control_initialized) return FALSE;
	if (!PL_get_integer(id_term, &id)) return FALSE;

	gp = SDL_OpenGamepad((SDL_JoystickID)id);
	if (gp == NULL) return FALSE;

	functor = PL_new_functor(PL_new_atom("control"), 1);
	ptr_term = PL_new_term_ref();
	if (!PL_put_pointer(ptr_term, gp)) return FALSE;

	return PL_unify_term(gamepad_term, PL_FUNCTOR, functor, PL_TERM, ptr_term);
}


/*
 * pl_control_close()
 * control_close(+Gamepad)
 * Closes a gamepad handle.
 */
static foreign_t pl_control_close(term_t gamepad_term)
{
	SDL_Gamepad *gp;

	gp = get_gamepad_ptr(gamepad_term);
	if (gp == NULL) return FALSE;

	SDL_CloseGamepad(gp);
	return TRUE;
}

/*
 * pl_control_pump()
 * control_pump
 * Pump SDL events. Call periodically from a Prolog thread.
 */
static foreign_t pl_control_pump(void)
{
	if (!g_control_initialized) return FALSE;
	SDL_PumpEvents();
	return TRUE;
}

/*
 * pl_control_axis()
 * control_axis(+Gamepad, +Axis, -Value)
 * Read axis value from gamepad. Value is -1.0 to 1.0.
 * Axis is one of: left_x, left_y, right_x, right_y, 
 * 		left_trigger, right_trigger
 *    	dpad_x, dpady
 */
static foreign_t pl_control_axis(term_t gamepad_term, term_t axis_term, term_t value_term)
{
	SDL_Gamepad *gp;
	atom_t axis_atom;
	SDL_GamepadAxis axis;
	Sint16 raw;
	float value;
	const char *axis_name;

	gp = get_gamepad_ptr(gamepad_term);
	if (gp == NULL) return FALSE;

	if (!PL_get_atom(axis_term, &axis_atom)) return FALSE;

	SDL_PumpEvents();

	/* check for virtual d-pad axes first */
	axis_name = PL_atom_chars(axis_atom);
	if (get_dpad_axis(gp, axis_name, &value)) {
		return PL_unify_float(value_term, value);
	}

	/* Real DSL axis */
	if (!get_axis_from_atom(axis_atom, &axis)) return FALSE;
	raw = SDL_GetGamepadAxis(gp, axis);
	value = raw / 32767.0f;

	return PL_unify_float(value_term, value);
}



/******************************************************************************
 * REGISTRATION
 *****************************************************************************/

/*
 * control_register_predicates()
 * Register control predicates with Prolog.
 */
install_t control_register_predicates(void)
{
	PL_register_foreign("control_init", 0, pl_control_init, 0);
	PL_register_foreign("control_shutdown", 0, pl_control_shutdown, 0);
	PL_register_foreign("control_gamepads", 1, pl_control_gamepads, 0);
	PL_register_foreign("control_open", 2, pl_control_open, 0);
	PL_register_foreign("control_close", 1, pl_control_close, 0);
	PL_register_foreign("control_pump", 0, pl_control_pump, 0);
	PL_register_foreign("control_axis", 3, pl_control_axis, 0);
}

/*
 * uninstall_control()
 * Cleanup on module unload.
 */
install_t uninstall_control(void)
{
	if (g_control_initialized) {
		SDL_Quit();
		g_control_initialized = MA_FALSE;
	}
}
