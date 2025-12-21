/*
 * control.c - gamepad and keyboard subsystem.
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#include "promini.h"
#include <limits.h>
#include <poll.h>
#include <unistd.h>
#include <SDL3_ttf/SDL_ttf.h>

#define GET_KEYBOARD(handle_term, kb_var, slot_var) \
    do { \
        if (!get_typed_handle(handle_term, "keyboard", &slot_var)) { \
            return PL_type_error("keyboard", handle_term); \
        } \
        if (slot_var < 0 || slot_var >= MAX_KEYBOARDS || !g_keyboards[slot_var].in_use) { \
            return PL_existence_error("keyboard", handle_term); \
        } \
        kb_var = &g_keyboards[slot_var]; \
    } while (0)

#define GET_ROW(kb_var, row_term, row_var, kr_var) \
    do { \
        if (!PL_get_integer(row_term, &row_var)) return FALSE; \
        if (row_var < 0 || row_var >= KEYBOARD_ROWS) { \
            return PL_domain_error("keyboard_row", row_term); \
        } \
        kr_var = &kb_var->rows[row_var]; \
    } while (0)

/******************************************************************************
 * GLOBAL VARIABLES
 *****************************************************************************/

static ma_bool32 g_control_initialized = MA_FALSE;
static PL_dispatch_hook_t g_old_dispatch_hook = NULL;
static TTF_Font *g_font = NULL;
static int g_voice_allocation_counter = 0;
static int g_log_target_keyboard = -1;	/* keyboard slot for log messages, -1 = console */

keyboard_t g_keyboards[MAX_KEYBOARDS] = {{0}};

/* forward declarations */
static void update_keyboard_title(keyboard_t *kb);
static void gate_off_voice_envelopes(keyboard_row_t *kr, int pool_idx);

/******************************************************************************
 * SLOT MANAGEMENT
 *****************************************************************************/

/*
 * allocate_keyboard_slot()
 * Find a free keyboard slot and mark it in use.
 * Returns slot index, or -1 if none available.
 */
static int allocate_keyboard_slot(void)
{
	int i;

	for(i = 0; i < MAX_KEYBOARDS; i++) {
		if (!g_keyboards[i].in_use) {
			g_keyboards[i].in_use = MA_TRUE;
			return i;
		}
	}

	return -1;
}

/*
 * find_keyboard_by_window()
 * Find keyboard slot that owns the given window ID.
 * Returns slot index, or -1 if not found.
 */
static int find_keyboard_by_window(SDL_WindowID window_id)
{
	int i;
	for (i = 0; i < MAX_KEYBOARDS; i++) {
		if (g_keyboards[i].in_use && g_keyboards[i].window != NULL &&
				SDL_GetWindowID(g_keyboards[i].window) == window_id) {
			return i;
		}
	}
	return -1;
}

/*
 * free_keyboard_slot()
 * Clean up and release a keyboard slot.
 */
static void free_keyboard_slot(int slot)
{
	keyboard_t *kb;
	int row, col, pool_idx;
	keyboard_row_t *kr;

	if (slot < 0 || slot >= MAX_KEYBOARDS) return;
	kb = &g_keyboards[slot];
	if (!kb->in_use) return;

	/* gate off all active envelopes */
	for (row = 0; row < KEYBOARD_ROWS; row++) {
		kr = &kb->rows[row];
		if (kr->target_type != KEYBOARD_TARGET_SYNTH) continue;
		for (col = 0; col < KEYBOARD_KEYS_PER_ROW; col++) {
			pool_idx = kr->target.synth.key_to_voice[col];
			if (pool_idx >= 0) {
				gate_off_voice_envelopes(kr, pool_idx);
			}
		}
	}

	if (kb->renderer != NULL) {
		SDL_DestroyRenderer(kb->renderer);
		kb->renderer = NULL;
	}
	if (kb->window != NULL) {
		SDL_DestroyWindow(kb->window);
		kb->window = NULL;
	}

	/* clear key state */
	for (row = 0; row < KEYBOARD_ROWS; row++) {
		for (col = 0; col < KEYBOARD_KEYS_PER_ROW; col++) {
			kb->keys_pressed[row][col] = MA_FALSE;
		}
	}
	for (col = 0; col < 5; col++) {
		kb->mod_keys[col] = MA_FALSE;
	}

	kb->in_use = MA_FALSE;
}

/*
 * control_set_log_message()
 * Set log message for display.
 * Routes to keyboard window if registered, otherwise console.
 */
void control_set_log_message(const char *msg)
{
	keyboard_t *kb;

	if (g_log_target_keyboard >= 0 &&
			g_log_target_keyboard < MAX_KEYBOARDS &&
			g_keyboards[g_log_target_keyboard].in_use) {
		kb = &g_keyboards[g_log_target_keyboard];
		strncpy(kb->log_message, msg, sizeof(kb->log_message) - 1);
		kb->log_message[sizeof(kb->log_message) - 1] = '\0';
	} else {
		printf("%s\n", msg);
	}
}

/*
 * pl_keyboard_set_log_target()
 * Register a keyboard as the log message target.
 * keyboard_set_log_target(+Keyboard)
 */
static foreign_t pl_keyboard_set_log_target(term_t handle_term)
{
	int slot;
	keyboard_t *kb;

	GET_KEYBOARD(handle_term, kb, slot);
	g_log_target_keyboard = slot;
	return TRUE;
}

/******************************************************************************
 * KEYBOARD AND WINDOW MANAGEMENT
 *****************************************************************************/

/*
 * keyboard_render()
 * Draw the 4x10 key grid plus mod key row.
 * When a key is pressed, display the semitone value.
 */
static void keyboard_render(void)
{
	int row, col, i, j;
	int win_w, win_h;
	float cell_w, cell_h;
	float mod_widths[5];
	float mod_x;
	SDL_FRect rect;
	keyboard_row_t *kr;
	int degree, octave;
	float semitones;
	char text[16];
	SDL_Surface *surface;
	SDL_Texture *texture;
	SDL_FRect text_rect;
	SDL_Color text_color = {255, 255, 255, 255};
	keyboard_t *kb;

	for (i = 0; i < MAX_KEYBOARDS; i++)
	{
		kb = &g_keyboards[i];
		if (!kb->in_use || kb->window == NULL) continue;

		SDL_GetWindowSize(kb->window, &win_w, &win_h);
		cell_w = win_w / 10.0f;
		cell_h = win_h / 6.0f;

		/* clear background */
		SDL_SetRenderDrawColor(kb->renderer, 32, 32, 32, 0);
		SDL_RenderClear(kb->renderer);

		/* draw log message in top row */
		if (kb->log_message[0] != '\0' && g_font != NULL) {
			surface = TTF_RenderText_Blended(g_font, kb->log_message, 0, text_color);
			if (surface != NULL) {
				texture = SDL_CreateTextureFromSurface(kb->renderer, surface);
				if (texture != NULL) {
					text_rect.w = (float)surface->w;
					text_rect.h = (float)surface->h;
					text_rect.x = 10;
					text_rect.y = (cell_h - text_rect.h) / 2;
					SDL_RenderTexture(kb->renderer, texture, NULL, &text_rect);
					SDL_DestroyTexture(texture);
				}
				SDL_DestroySurface(surface);
			}
		}

		/* draw 4x10 key rectangles */
		for (row = 0; row < 4; row++) {
			for (col = 0; col < 10; col++) {
				if (kb->keys_pressed[row][col]) {
					SDL_SetRenderDrawColor(kb->renderer, 200, 120, 60, 220);
				} else {
					SDL_SetRenderDrawColor(kb->renderer, 80, 80, 80, 200);
				}
				rect.x = col * cell_w + 2;
				rect.y = (row + 1) * cell_h + 2;
				rect.w = cell_w - 4;
				rect.h = cell_h - 4;
				SDL_RenderFillRect(kb->renderer, &rect);

				/* draw semitone value if key is pressed */
				if (kb->keys_pressed[row][col] && g_font != NULL) {
					kr = &kb->rows[row];
					if (kr->mode_length > 0) {
						degree = col % kr->mode_length;
						octave = col / kr->mode_length;
						semitones = octave * 12.0f + kr->mode[degree];
						snprintf(text, sizeof(text), "%.1f", semitones);

						surface = TTF_RenderText_Blended(g_font, text, 0, text_color);
						if (surface != NULL) {
							texture = SDL_CreateTextureFromSurface(kb->renderer, surface);
							if (texture != NULL) {
								text_rect.w = (float)surface->w;
								text_rect.h = (float)surface->h;
								text_rect.x = rect.x + (rect.w - text_rect.w) / 2;
								text_rect.y = rect.y + (rect.h - text_rect.h) / 2;
								SDL_RenderTexture(kb->renderer, texture, NULL, &text_rect);
								SDL_DestroyTexture(texture);
							}
							SDL_DestroySurface(surface);
						}
					}
				}
			}
		}

		/* draw mod key row: l_shift, l_option, space, r_option, r_shift */
		mod_widths[0] = cell_w * 1.5f;      /* l_shift */
		mod_widths[1] = cell_w * 1.25f;     /* l_option */
		mod_widths[2] = cell_w * 4.5f;      /* space */
		mod_widths[3] = cell_w * 1.25f;     /* r_option */
		mod_widths[4] = cell_w * 1.5f;      /* r_shift */

		mod_x = 0;
		for (j = 0; j < 5; j++) {
			if (kb->mod_keys[j]) {
				SDL_SetRenderDrawColor(kb->renderer, 200, 120, 60, 220);
			} else {
				SDL_SetRenderDrawColor(kb->renderer, 80, 80, 80, 200);
			}
			rect.x = mod_x + 2;
			rect.y = 5 * cell_h + 2;
			rect.w = mod_widths[j] - 4;
			rect.h = cell_h - 4;
			SDL_RenderFillRect(kb->renderer, &rect);
			mod_x += mod_widths[j];
		}

		SDL_RenderPresent(kb->renderer);
	}
}

/*
 * scancode_to_mod()
 * Map SDL scancode to mod key index (0-4).
 * Returns 1 if valid mod key, 0 otherwise.
 */
static int scancode_to_mod(SDL_Scancode sc, int *index)
{
	switch (sc) {
		case SDL_SCANCODE_LSHIFT: *index = 0; return 1;
		case SDL_SCANCODE_LALT:   *index = 1; return 1;
		case SDL_SCANCODE_SPACE:  *index = 2; return 1;
		case SDL_SCANCODE_RALT:   *index = 3; return 1;
		case SDL_SCANCODE_RSHIFT: *index = 4; return 1;
		default: return 0;
	}
}

/*
 * scancode_to_key()
 * Map SDL scancode to row (0-3) and col (0-9).
 * Returns 1 if valid key, 0 otherwise.
 */
static int scancode_to_key(SDL_Scancode sc, int *row, int *col)
{
	int i;

	/* Row 0: 1 2 3 4 5 6 7 8 9 0 */
	if (sc >= SDL_SCANCODE_1 && sc <= SDL_SCANCODE_9) {
		*row = 0; *col = sc - SDL_SCANCODE_1; return 1;
	}
	if (sc == SDL_SCANCODE_0) {
		*row = 0; *col = 9; return 1;
	}

	/* Row 1: Q W E R T Y U I O P */
	{
		static SDL_Scancode row1[] = {
			SDL_SCANCODE_Q, SDL_SCANCODE_W, SDL_SCANCODE_E, SDL_SCANCODE_R,
			SDL_SCANCODE_T, SDL_SCANCODE_Y, SDL_SCANCODE_U, SDL_SCANCODE_I,
			SDL_SCANCODE_O, SDL_SCANCODE_P
		};
		for (i = 0; i < 10; i++) {
			if (sc == row1[i]) { *row = 1; *col = i; return 1; }
		}
	}

	/* Row 2: A S D F G H J K L ; */
	{
		static SDL_Scancode row2[] = {
			SDL_SCANCODE_A, SDL_SCANCODE_S, SDL_SCANCODE_D, SDL_SCANCODE_F,
			SDL_SCANCODE_G, SDL_SCANCODE_H, SDL_SCANCODE_J, SDL_SCANCODE_K,
			SDL_SCANCODE_L, SDL_SCANCODE_SEMICOLON
		};
		for (i = 0; i < 10; i++) {
			if (sc == row2[i]) { *row = 2; *col = i; return 1; }
		}
	}

	/* Row 3: Z X C V B N M , . / */
	{
		static SDL_Scancode row3[] = {
			SDL_SCANCODE_Z, SDL_SCANCODE_X, SDL_SCANCODE_C, SDL_SCANCODE_V,
			SDL_SCANCODE_B, SDL_SCANCODE_N, SDL_SCANCODE_M, SDL_SCANCODE_COMMA,
			SDL_SCANCODE_PERIOD, SDL_SCANCODE_SLASH
		};
		for (i = 0; i < 10; i++) {
			if (sc == row3[i]) { *row = 3; *col = i; return 1; }
		}
	}

	return 0;
}

/*
 * find_free_voice()
 * Find a voice the pool not currently assigned to any key.
 * If all voices are in use, steal the oldest one.
 * Returns pool index.
 */
static int find_free_voice(keyboard_row_t *kr)
{
	int i, j;
	ma_bool32 in_use;
	int oldest_idx = 0;
	int oldest_order = INT_MAX;

	for (i = 0; i < kr->target.synth.pool_size; i++) {
		in_use = MA_FALSE;
		for (j = 0; j < KEYBOARD_KEYS_PER_ROW; j++) {
			if (kr->target.synth.key_to_voice[j] == i) {
				in_use = MA_TRUE;
				/* track oldest for stealing */
				if (kr->target.synth.allocation_order[i] < oldest_order) {
					oldest_order = kr->target.synth.allocation_order[i];
					oldest_idx = i;
				}
				break;
			}
		}
		if (!in_use) return i;
	}

	/* all in use, steal oldest */
	return oldest_idx;
}

/*
 * handle_granular_key_down()
 * Handle key press for granular row.
 * Trigger gain with pitch.
 */
static void handle_granular_key_down(keyboard_row_t *kr, int col)
{
	int slot, degree, octave;
	float semitones;

	slot = kr->target.granular_slot;
	if (slot < 0 || slot >= MAX_GRANULAR_DELAYS || !g_granular_delays[slot].in_use) {
		return;
	}

	degree = col % kr->mode_length;
	octave = col / kr->mode_length;
	semitones = (float)(kr->octave_offset + octave) * 12.0f + kr->mode[degree];
	trigger_grain_pitched(&g_granular_delays[slot], semitones);
}

/*
 * gate_off_voice_envelopes()
 * Gate off all envelopes for a voice in the pool.
 */
static void gate_off_voice_envelopes(keyboard_row_t *kr, int pool_idx)
{
	int e;
	for (e = 0; e < kr->target.synth.envelope_counts[pool_idx]; e++) {
		envelope_gate_off(kr->target.synth.envelope_slots[pool_idx][e]);
	}
}

/*
 * handle_synth_key_down()
 * Handle key press for synth row - allocate voice, set freq, gate on.
 */
  static void handle_synth_key_down(keyboard_row_t *kr, int col)
  {
        int pool_idx, voice_slot, degree, octave, e;
        float semitones;
        double freq;

		if (kr->target.synth.key_to_voice[col] != -1) return;  /* key already held */

        if (kr->target.synth.pool_size <= 0) return;

		pool_idx = find_free_voice(kr);
		voice_slot = kr->target.synth.voice_slots[pool_idx];
		degree = col % kr->mode_length;
		octave = col / kr->mode_length;
		semitones = (float)(kr->octave_offset + octave) * 12.0f + kr->mode[degree];
        freq = 440.0 * pow(2.0, semitones / 12.0);

		/* gate off and clear old key if stealing */
		for (e = 0; e < KEYBOARD_KEYS_PER_ROW; e++) {
			if (kr->target.synth.key_to_voice[e] == pool_idx) {
				gate_off_voice_envelopes(kr, pool_idx);
				kr->target.synth.key_to_voice[e] = -1;
				break;
			}
		}

		kr->target.synth.key_to_voice[col] = pool_idx;
		kr->target.synth.allocation_order[pool_idx] = ++g_voice_allocation_counter;
		voice_set_frequency(voice_slot, freq);

		for (e = 0; e < kr->target.synth.envelope_counts[pool_idx]; e++) {
			envelope_gate_on(kr->target.synth.envelope_slots[pool_idx][e]);
		}
}

/*
 * handle_synth_key_up()
 * Handle key release for synth row - gate off envelopes, release voice.
 */
static void handle_synth_key_up(keyboard_row_t *kr, int col)
{
	int pool_idx, e;

	pool_idx = kr->target.synth.key_to_voice[col];
	if (pool_idx < 0) return;

	/* gate off all envelopes */
	for (e = 0; e < kr->target.synth.envelope_counts[pool_idx]; e++) {
		envelope_gate_off(kr->target.synth.envelope_slots[pool_idx][e]);
	}

	kr->target.synth.key_to_voice[col] = -1;
}

/*
 * keyboard_handle_event()
 * Handle SDL event if it belongs to the keyboard window.
 */
void keyboard_handle_event(SDL_Event *event)
{
	int row, col, mod_index;
	SDL_WindowID event_wid;
	int slot;
	keyboard_t *kb;

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

	slot = find_keyboard_by_window(event_wid);
	if (slot < 0) return;
	kb = &g_keyboards[slot];

	if (event->type == SDL_EVENT_WINDOW_CLOSE_REQUESTED) {
		free_keyboard_slot(slot);
	} else if (event->type == SDL_EVENT_KEY_DOWN) {
		if (event->key.scancode == SDL_SCANCODE_ESCAPE) {
			free_keyboard_slot(slot);
		} else if (scancode_to_key(event->key.scancode, &row, &col)) {
			kb->keys_pressed[row][col] = MA_TRUE;
			if (kb->rows[row].target_type == KEYBOARD_TARGET_GRANULAR) {
				handle_granular_key_down(&kb->rows[row], col);
			} else if (kb->rows[row].target_type == KEYBOARD_TARGET_SYNTH) {
				handle_synth_key_down(&kb->rows[row], col);
			}
		} else if (scancode_to_mod(event->key.scancode, &mod_index)) {
			kb->mod_keys[mod_index] = MA_TRUE;
		}
	} else if (event->type == SDL_EVENT_KEY_UP) {
		if (scancode_to_key(event->key.scancode, &row, &col)) {
			kb->keys_pressed[row][col] = MA_FALSE;
			if (kb->rows[row].target_type == KEYBOARD_TARGET_SYNTH) {
				handle_synth_key_up(&kb->rows[row], col);
			}
		} else if (scancode_to_mod(event->key.scancode, &mod_index)) {
			kb->mod_keys[mod_index] = MA_FALSE;
		}
	}
}

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
	SDL_Event event;

	if (g_control_initialized) {
		/* move pending OS events into SDL's internal queue */
		SDL_PumpEvents();

		/* process all queued events */
		while (SDL_PollEvent(&event)) {
			keyboard_handle_event(&event);
			visualizer_handle_event(&event);
		}

		keyboard_render();
		visualizer_render_all();
	}

	/* Poll stdin with 10ms timeout - allows event loop to run while REPL waits */
	fds[0].fd = fd;
	fds[0].events = POLLIN;

	if (poll(fds, 1, 10) > 0) {
		if (fds[0].revents & POLLIN) {
			/* Tell Prolog stdin has data, go read it */
			return PL_DISPATCH_INPUT;
		}
	}

	/* Tell Prolog no input yet, call me again - keeps dispatch hook running */
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

static struct {
	const char *name;
	SDL_GamepadButton button;
} button_map[] = {
	{"a", SDL_GAMEPAD_BUTTON_SOUTH},
	{"b", SDL_GAMEPAD_BUTTON_EAST},
	{"x", SDL_GAMEPAD_BUTTON_WEST},
	{"y", SDL_GAMEPAD_BUTTON_NORTH},
	{"lb", SDL_GAMEPAD_BUTTON_LEFT_SHOULDER},
	{"rb", SDL_GAMEPAD_BUTTON_RIGHT_SHOULDER},
	{"l3", SDL_GAMEPAD_BUTTON_LEFT_STICK},
	{"r3", SDL_GAMEPAD_BUTTON_RIGHT_STICK},
	{"back", SDL_GAMEPAD_BUTTON_BACK},
	{"start", SDL_GAMEPAD_BUTTON_START},
	{"guide", SDL_GAMEPAD_BUTTON_GUIDE},
	{"dpad_up", SDL_GAMEPAD_BUTTON_DPAD_UP},
	{"dpad_down", SDL_GAMEPAD_BUTTON_DPAD_DOWN},
	{"dpad_left", SDL_GAMEPAD_BUTTON_DPAD_LEFT},
	{"dpad_right", SDL_GAMEPAD_BUTTON_DPAD_RIGHT},
	{"l4", SDL_GAMEPAD_BUTTON_LEFT_PADDLE1},
	{"r4", SDL_GAMEPAD_BUTTON_RIGHT_PADDLE1},
	{"pl", SDL_GAMEPAD_BUTTON_LEFT_PADDLE2},
	{"pr", SDL_GAMEPAD_BUTTON_RIGHT_PADDLE2},
	{"touchpad", SDL_GAMEPAD_BUTTON_TOUCHPAD},
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
 * get_button_from_atom()
 * Convert Prolog atom to SDL_GamepadButton.
 * Returns 1 on success, 0 on failure.
 */
int get_button_from_atom(atom_t atom, SDL_GamepadButton *button)
{
	const char *name;
	int i;

	name = PL_atom_chars(atom);
	for (i = 0; button_map[i].name != NULL; i++) {
		if (strcmp(name, button_map[i].name) == 0) {
			*button = button_map[i].button;
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

    /* 4. Initialize SDL_ttf and load font */
    if (!TTF_Init()) {
        SDL_Quit();
        return FALSE;
    }
    g_font = TTF_OpenFont("fonts/Game Of Squids.ttf", 24);
    if (g_font == NULL) {
        TTF_Quit();
        SDL_Quit();
        return FALSE;
    }

    /* 5. Install dispatch hook to pump events while REPL waits */
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

	/* Clean up font and TTF */
	if (g_font != NULL) {
		TTF_CloseFont(g_font);
		g_font = NULL;
	}
	TTF_Quit();
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

/*
 * pl_keyboard_init()
 * keyboard_init(-Keyboard)
 * Create a new keyboard window with unconfigured rows.
 */
static foreign_t pl_keyboard_init(term_t kb_term)
{
	int slot, row, i;
	keyboard_t *kb;
	functor_t functor;
	term_t slot_term;
	static float d_minor[] = {0.0f, 2.0f, 3.0f, 5.0f, 7.0f, 8.0f, 10.0f};
	int octaves[] = {2, 1, 0, -1};

	slot = allocate_keyboard_slot();
	if (slot < 0) return FALSE;
	kb = &g_keyboards[slot];

	kb->window = SDL_CreateWindow("promini keyboard", 640, 300,
			SDL_WINDOW_RESIZABLE | SDL_WINDOW_TRANSPARENT | SDL_WINDOW_UTILITY);
	if (kb->window == NULL) {
		kb->in_use = MA_FALSE;
		return FALSE;
	}

	/* offset window position based on slot to avoid stacking */
	{
		int x, y;
		SDL_GetWindowPosition(kb->window, &x, &y);
		SDL_SetWindowPosition(kb->window, x + slot * 30, y + slot * 30);
	}

	kb->renderer = SDL_CreateRenderer(kb->window, NULL);
	if (kb->renderer == NULL) {
		SDL_DestroyWindow(kb->window);
		kb->window = NULL;
		kb->in_use = MA_FALSE;
		return FALSE;
	}

	/* Initialize all rows as unconfigured */
	for (row = 0; row < KEYBOARD_ROWS; row++) {
		for (i = 0; i < 7; i++) {
			kb->rows[row].mode[i] = d_minor[i];
		}
		kb->rows[row].mode_length = 7;
		kb->rows[row].octave_offset = octaves[row];
		kb->rows[row].target_type = KEYBOARD_TARGET_NONE;
		kb->rows[row].target.synth.pool_size = 0;
		for (i = 0; i < KEYBOARD_KEYS_PER_ROW; i++) {
			kb->rows[row].target.synth.key_to_voice[i] = -1;
		}
	}

	update_keyboard_title(kb);

	/* return keyboard(Slot) handle */
	functor = PL_new_functor(PL_new_atom("keyboard"), 1);
	slot_term = PL_new_term_ref();
	if (!PL_put_integer(slot_term, slot)) return FALSE;

	return PL_unify_term(kb_term, PL_FUNCTOR, functor, PL_TERM, slot_term);
}

/*                                                                                                           
 * pl_keyboard_uninit()                                                                                      
 * keyboard_uninit(+Keyboard)                                                                                
 * Destroy a keyboard window.                                                                                
 */                                                                                                          
static foreign_t pl_keyboard_uninit(term_t kb_term)                                                          
{                                                                                                            
	int slot;                                                                                              

	if (!get_typed_handle(kb_term, "keyboard", &slot)) return FALSE;                                       
	if (slot < 0 || slot >= MAX_KEYBOARDS) return FALSE;                                                   
	if (!g_keyboards[slot].in_use) return FALSE;                                                           

	free_keyboard_slot(slot);                                                                              
	return TRUE;                                                                                           
  } 

/*
 * pl_keyboard_row_set()
 * keyboard_row_set(+Keyboard, +Row, +Options)
 * Configure a keyboard row's mode and octave.
 * Options: mode=List, octave=Int
 */
static foreign_t pl_keyboard_row_set(term_t kb_term, term_t row_term, term_t opts_term)
{
	int slot, row, octave, count;
	keyboard_t *kb;
	keyboard_row_t *kr;

	GET_KEYBOARD(kb_term, kb, slot);
	GET_ROW(kb, row_term, row, kr);

	/* parse mode list if provided */
	count = get_param_float_list(opts_term, "mode", kr->mode, MAX_MODE_INTERVALS);
	if (count > 0) {
		kr->mode_length = count;
	}

	/* parse octave if provided */
	if (get_param_int(opts_term, "octave", &octave)) {
		kr->octave_offset = octave;
	}

	return TRUE;
}

/*
 * update_keyboard_title()
 * Update window title to show connected sources.
 */
static void update_keyboard_title(keyboard_t *kb)
{
	char title[256];
	char *p;
	int row, len, i, j;
	int granular_seen[KEYBOARD_ROWS];
	int voice_seen[MAX_POOL_VOICES * KEYBOARD_ROWS];
	int num_granular = 0;
	int num_voices = 0;
	int slot, already_seen;

	p = title;

	for (row = 0; row < KEYBOARD_ROWS; row++) {
		if (kb->rows[row].target_type == KEYBOARD_TARGET_GRANULAR) {
			slot = kb->rows[row].target.granular_slot;

			/* check if already seen */
			already_seen = 0;
			for (i = 0; i < num_granular; i++) {
				if (granular_seen[i] == slot) {
					already_seen = 1;
					break;
				}
			}
			if (already_seen) continue;
			granular_seen[num_granular++] = slot;

			len = snprintf(p, sizeof(title) - (p - title),
				"%sgranular(%d)",
				(p == title) ? "" : ", ",
				slot);
			p += len;
		} else if (kb->rows[row].target_type == KEYBOARD_TARGET_SYNTH) {
			for (j = 0; j < kb->rows[row].target.synth.pool_size; j++) {
				slot = kb->rows[row].target.synth.voice_slots[j];

				/* check if already seen */
				already_seen = 0;
				for (i = 0; i < num_voices; i++) {
					if (voice_seen[i] == slot) {
						already_seen = 1;
						break;
					}
				}
				if (already_seen) continue;
				voice_seen[num_voices++] = slot;

				len = snprintf(p, sizeof(title) - (p - title),
					"%svoice(%d)",
					(p == title) ? "" : ", ",
					slot);
				p += len;
			}
		}
	}

	if (p == title) {
		snprintf(title, sizeof(title), "keyboard");
	}

	SDL_SetWindowTitle(kb->window, title);
}

/*
 * pl_keyboard_connect()
 * keyboard_connect(+Keyboard, +Row, +Target)
 * Connect a keyboard row to a target (granular(N) for now).
 */
static foreign_t pl_keyboard_connect(term_t kb_term, term_t row_term, term_t target_term)
{
	int slot, row, target_slot;
	keyboard_t *kb;
	keyboard_row_t *kr;

	GET_KEYBOARD(kb_term, kb, slot);
	GET_ROW(kb, row_term, row, kr);

	if (get_typed_handle(target_term, "granular", &target_slot)) {
		if (target_slot < 0 || target_slot >= MAX_GRANULAR_DELAYS) return FALSE;
		kr->target_type = KEYBOARD_TARGET_GRANULAR;
		kr->target.granular_slot = target_slot;
		update_keyboard_title(kb);
		return TRUE;
	}

	return FALSE;
}

/* 
 * pl_keyboard_row_add_voice()
 * keyboard_row_add_voice(+Keyboard, +Row, +Voice, +Envelopes)
 * Add a voice and list of envelopes to a row's synth pool
 */
static foreign_t pl_keyboard_row_add_voice(
		term_t kb_term,
		term_t row_term,
		term_t voice_term,
		term_t envelopes_term)
{
	keyboard_t *kb;
	int kb_slot, row, voice_slot, envelope_slot;
	int envelope_count = 0;
	keyboard_row_t *kr;
	term_t head = PL_new_term_ref();
	term_t list = PL_copy_term_ref(envelopes_term);

	GET_KEYBOARD(kb_term, kb, kb_slot);
	GET_ROW(kb, row_term, row, kr);

	if (!get_typed_handle(voice_term, "voice", &voice_slot)) {
		return PL_type_error("voice", voice_term);
	}

	if (kr->target.synth.pool_size >= MAX_POOL_VOICES) {
		return PL_resource_error("pool_voices");
	}

	/* parse envelope list */
	while (PL_get_list(list, head, list)) {
		if (envelope_count >= MAX_ENVELOPES_PER_VOICE) {
			return PL_resource_error("envelopes_per_voice");
		}
		if (!get_typed_handle(head, "mod_source", &envelope_slot)) {
			return PL_type_error("mod_source", head);
		}
		kr->target.synth.envelope_slots[kr->target.synth.pool_size][envelope_count] = envelope_slot;
		envelope_count++;
	}

	kr->target_type = KEYBOARD_TARGET_SYNTH;
	kr->target.synth.voice_slots[kr->target.synth.pool_size] = voice_slot;
	kr->target.synth.envelope_counts[kr->target.synth.pool_size] = envelope_count;
	kr->target.synth.pool_size++;

	update_keyboard_title(kb);
	return TRUE;
}

/*
 * clear_voice_slot()
 * Clear a voice slot in a row's synth pool.
 */
static void clear_voice_slot(keyboard_row_t *kr, int idx)
{
	int j;

	kr->target.synth.voice_slots[idx] = -1;
	kr->target.synth.envelope_counts[idx] = 0;
	for (j = 0; j < MAX_ENVELOPES_PER_VOICE; j++) {
		kr->target.synth.envelope_slots[idx][j] = -1;
	}
}


/*
 * pl_keyboard_row_remove_voice()
 * keyboard_row_remove_voice(+Keyboard, +Row, +Voice)
 * Remove a voice from a row's synth pool.
 */
static foreign_t pl_keyboard_row_remove_voice(term_t kb_term, term_t row_term, term_t voice_term)
{
	keyboard_t *kb;
	int kb_slot, row, voice_slot, i, j, k, last;
	keyboard_row_t *kr;

	GET_KEYBOARD(kb_term, kb, kb_slot);
	GET_ROW(kb, row_term, row, kr);

	if (!get_typed_handle(voice_term, "voice", &voice_slot)) {
		return PL_type_error("voice", voice_term);
	}

	/* find voice in pool */
	for (i = 0; i < kr->target.synth.pool_size; i++) {
		if (kr->target.synth.voice_slots[i] == voice_slot) {
			/* gate off if any key is using this voice */
			for (k = 0; k < KEYBOARD_KEYS_PER_ROW; k++) {
				if (kr->target.synth.key_to_voice[k] == i) {
					gate_off_voice_envelopes(kr, i);
					kr->target.synth.key_to_voice[k] = -1;
					break;
				}
			}

			/* swap with last entry */
			last = kr->target.synth.pool_size - 1;
			if (i != last) {
				/* update key_to_voice mappings pointing to last */
				for (k = 0; k < KEYBOARD_KEYS_PER_ROW; k++) {
					if (kr->target.synth.key_to_voice[k] == last) {
						kr->target.synth.key_to_voice[k] = i;
					}
				}

				kr->target.synth.voice_slots[i] = kr->target.synth.voice_slots[last];
				kr->target.synth.envelope_counts[i] = kr->target.synth.envelope_counts[last];
				kr->target.synth.allocation_order[i] = kr->target.synth.allocation_order[last];
				for (j = 0; j < MAX_ENVELOPES_PER_VOICE; j++) {
					kr->target.synth.envelope_slots[i][j] = kr->target.synth.envelope_slots[last][j];
				}
			}

			clear_voice_slot(kr, last);
			kr->target.synth.pool_size--;

			/* if pool empty, reset to NONE */
			if (kr->target.synth.pool_size == 0) {
				kr->target_type = KEYBOARD_TARGET_NONE;
			}

			return TRUE;
		}
	}

	return FALSE;
}

/*
 * pl_keyboard_row_clear()
 * keyboard_row_clear(+Keyboard, +Row)
 * Remove all voices from a row's synth pool.
 */
static foreign_t pl_keyboard_row_clear(term_t kb_term, term_t row_term)
{
	keyboard_t *kb;
	int kb_slot, row, i, pool_idx;
	keyboard_row_t *kr;

	GET_KEYBOARD(kb_term, kb, kb_slot);
	GET_ROW(kb, row_term, row, kr);

	/* gate off all active envelopes */
	for (i = 0; i < KEYBOARD_KEYS_PER_ROW; i++) {
		pool_idx = kr->target.synth.key_to_voice[i];
		if (pool_idx >= 0) {
			gate_off_voice_envelopes(kr, pool_idx);
		}
	}

	/* clear all voice slots */
	for (i = 0; i < kr->target.synth.pool_size; i++) {
		clear_voice_slot(kr, i);
	}

	kr->target.synth.pool_size = 0;
	kr->target_type = KEYBOARD_TARGET_NONE;

	/* clear key mappings */
	for (i = 0; i < KEYBOARD_KEYS_PER_ROW; i++) {
		kr->target.synth.key_to_voice[i] = -1;
	}

	return TRUE;
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
	PL_register_foreign("keyboard_init", 1, pl_keyboard_init, 0);
	PL_register_foreign("keyboard_uninit", 1, pl_keyboard_uninit, 0);
	PL_register_foreign("keyboard_connect", 3, pl_keyboard_connect, 0);
	PL_register_foreign("keyboard_row_set", 3, pl_keyboard_row_set, 0);
	PL_register_foreign("keyboard_row_add_voice", 4, pl_keyboard_row_add_voice, 0);
	PL_register_foreign("keyboard_row_clear", 2, pl_keyboard_row_clear, 0);
	PL_register_foreign("keyboard_row_remove_voice", 3, pl_keyboard_row_remove_voice, 0);
	PL_register_foreign("keyboard_set_log_target", 1, pl_keyboard_set_log_target, 0);
}

/*
 * uninstall_control()
 * Cleanup on module unload.
 */
install_t uninstall_control(void)
{
	int i;

	if (g_control_initialized) {
		for (i = 0; i < MAX_KEYBOARDS; i++) {
			free_keyboard_slot(i);
        }

		if (g_font != NULL) {
			TTF_CloseFont(g_font);
			g_font = NULL;
		}

		TTF_Quit();
		SDL_Quit();
		g_control_initialized = MA_FALSE;
	}
}
