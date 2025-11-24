/*
 * synth.c - Additive synthesizer using miniaudio
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */


#include "sampler_internal.h"


/******************************************************************************
 * TYPE DEFINITIONS
 *****************************************************************************/

#define MAX_VOICES 256

typedef struct {
	ma_waveform waveform;
	ma_sound sound;
	ma_bool32 in_use;
	double frequency;
	double phase;
	float pan;
} synth_voice_t;

 #define GET_VOICE_FROM_HANDLE(handle_term, voice_var, slot_var) \
  	do { \
  		if (!PL_get_integer(handle_term, &slot_var)) { \
  			return PL_type_error("integer", handle_term); \
  		} \
  		if (slot_var < 0 || slot_var >= MAX_VOICES || !g_voices[slot_var].in_use) { \
  			return PL_existence_error("voice", handle_term); \
  		} \
  		voice_var = &g_voices[slot_var]; \
  	} while(0)
/******************************************************************************
 * GLOBAL VARIABLES
 *****************************************************************************/

static synth_voice_t g_voices[MAX_VOICES] = {{0}};

/*
 * Thread safety - mutex for protecting voice allocation
 */
pthread_mutex_t g_voices_mutex = PTHREAD_MUTEX_INITIALIZER;

/******************************************************************************
 * HELPER FUNCTIONS
 *****************************************************************************/

/*
 * allocate_voice_slot()
 * Finds a free voice slot and marks it as in use.
 * Returns slot index, or -1 if all slots are full.
 * Thread-safe: protected by g_voices_mutex
 */
static int allocate_voice_slot(void)
{
	int i;
	int slot = -1;

	pthread_mutex_lock(&g_voices_mutex);
	for (i = 0; i < MAX_VOICES; i++) {
		if (!g_voices[i].in_use) {
			g_voices[i].in_use = MA_TRUE;
			slot = i;
			break;
		}
	}
	pthread_mutex_unlock(&g_voices_mutex);

	return slot;
}

/*
 * free_voice_slot()
 * Frees a voice slot and its resources.
 * Thread-safe: protected by g_voices_mutex
 */
static void free_voice_slot(int index)
{
	if (index >= 0 && index < MAX_VOICES) {
		pthread_mutex_lock(&g_voices_mutex);

		if (g_voices[index].in_use) {
			ma_sound_uninit(&g_voices[index].sound);
			ma_waveform_uninit(&g_voices[index].waveform);
			g_voices[index].in_use = MA_FALSE;
		}

		pthread_mutex_unlock(&g_voices_mutex);
	}
}

/******************************************************************************
 * VOICE MANAGEMENT
 *****************************************************************************/

/*
 * pl_synth_voice_create()
 * synth_voice_create(-Handle)
 * Creates a new synthesizer voice with default parameters.
 */
static foreign_t pl_synth_voice_create(term_t handle)
{
	int slot;
  	ma_waveform_config waveform_config;
  	ma_format format;
  	ma_uint32 channels;
  	ma_uint32 sample_rate;
  	ma_result result;

	ENSURE_ENGINE_INITIALIZED();

	slot = allocate_voice_slot();
	if (slot < 0) {
		return PL_resource_error("voice_slots");
	}

	get_engine_format_info(&format, &channels, &sample_rate);

	waveform_config = ma_waveform_config_init(
			format,
			channels, 
			sample_rate,
			ma_waveform_type_sine,
			1.0,
			440.0
			);

	result = ma_waveform_init(&waveform_config, &g_voices[slot].waveform);
	if (result != MA_SUCCESS) {
		free_voice_slot(slot);
		return FALSE;
	}

	result = ma_sound_init_from_data_source(
			g_engine,
			&g_voices[slot].waveform,
			MA_SOUND_FLAG_NO_SPATIALIZATION | MA_SOUND_FLAG_STREAM,
			NULL,
			&g_voices[slot].sound
			);
	if (result != MA_SUCCESS) {
		ma_waveform_uninit(&g_voices[slot].waveform);
		free_voice_slot(slot);
		return FALSE;
	}

	ma_sound_set_looping(&g_voices[slot].sound, MA_TRUE);

	g_voices[slot].frequency = 440.0;
	g_voices[slot].phase = 0.0;
	g_voices[slot].pan = 0.0;

	return PL_unify_integer(handle, slot);
}

/*
 * pl_sampler_synth_voices_in_use()
 * sampler_synth_voices_in_use(-Count)
 * Returns the number of voices currently in use.
 */
static foreign_t pl_synth_voices_in_use(term_t count)
{
	int i;
	int in_use = 0;

	for (i = 0; i < MAX_VOICES; i++) {
		if (g_voices[i].in_use) {
			in_use++;
		}
	}

	return PL_unify_integer(count, in_use);
}

/*
 * pl_synth_voice_start()
 * sampler_synth_voice_start(+Handle)
 * Starts playing a voice.
 */
static foreign_t pl_synth_voice_start(term_t handle)
{
	int slot;
	synth_voice_t* voice;
	ma_result result;

	GET_VOICE_FROM_HANDLE(handle, voice, slot);

	result = ma_sound_start(&voice->sound);
	if (result != MA_SUCCESS) {
		return FALSE;
	}

	return TRUE;
}

/*
 * pl_synth_voice_stop()
 * sampler_synth_voice_stop(+Handle)
 * Stops playing a voice.
 */
static foreign_t pl_synth_voice_stop(term_t handle)
{
	int slot;
	synth_voice_t* voice;
	ma_result result;

	GET_VOICE_FROM_HANDLE(handle, voice, slot);

	result = ma_sound_stop(&voice->sound);
	if (result != MA_SUCCESS) {
		return FALSE;
	}

	return TRUE;
}

/*
 * pl_synth_voice_set_frequency()
 * sampler_synth_voice_set_frequency(+Handle, +Frequency)
 * Sets the frequency of a voice.
 */
static foreign_t pl_synth_voice_set_frequency(term_t handle, term_t freq_term)
{
	int slot;
	synth_voice_t* voice;
	double frequency;

	GET_VOICE_FROM_HANDLE(handle, voice, slot);

  	if (!PL_get_float(freq_term, &frequency)) {
  		return PL_type_error("float", freq_term);
  	}

  	if (frequency <= 0.0) {
  		return PL_domain_error("positive_frequency", freq_term);
  	}

	ma_waveform_set_frequency(&voice->waveform, frequency);
	voice->frequency = frequency;

	return TRUE;
}

/*
 * pl_synth_voice_get_frequency()
 * sampler_synth_voice_get_frequency(+Handle, -Frequency)
 * Gets the frequency of a voice.
 */
static foreign_t pl_synth_voice_get_frequency(term_t handle, term_t freq_term)
{
	int slot;
	synth_voice_t* voice;

	GET_VOICE_FROM_HANDLE(handle, voice, slot);

	return PL_unify_float(freq_term, voice->frequency);
}

/*
 * pl_synth_voice_unload()
 * sampler_synth_voice_unload(+Handle)
 * Unloads a voice and frees its resources.
 */
static foreign_t pl_synth_voice_unload(term_t handle)
{
	int slot;

	if (!PL_get_integer(handle, &slot)) {
		return PL_type_error("integer", handle);
	}

	if (slot < 0 || slot >= MAX_VOICES || !g_voices[slot].in_use) {
		return PL_existence_error("voice", handle);
	}

	free_voice_slot(slot);
	return TRUE;
}

/******************************************************************************
 * PROLOG INTERFACE
 *****************************************************************************/


/*
 * synth_register_predicates()
 * Register synth foreign predicates with SWI-Prolog.
 */
install_t synth_register_predicates(void)
{
	PL_register_foreign("sampler_synth_voices_in_use", 1, pl_synth_voices_in_use, 0);
	PL_register_foreign("sampler_synth_voice_create", 1, pl_synth_voice_create, 0);
	PL_register_foreign("sampler_synth_voice_start", 1, pl_synth_voice_start, 0);
	PL_register_foreign("sampler_synth_voice_stop", 1, pl_synth_voice_stop, 0);
	PL_register_foreign("sampler_synth_voice_set_frequency", 2, pl_synth_voice_set_frequency, 0);
	PL_register_foreign("sampler_synth_voice_get_frequency", 2, pl_synth_voice_get_frequency, 0);
	PL_register_foreign("sampler_synth_voice_unload", 1, pl_synth_voice_unload, 0);
}

/* uninstall_synth()
 * Called when the foreign library is unloaded.
 * Cleans up all voices.
 */
install_t uninstall_synth(void)
{
	int i;

	for (i = 0; i < MAX_VOICES; i++) {
		if (g_voices[i].in_use) {
			free_voice_slot(i);
		}
	}
}
