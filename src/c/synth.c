/*
 * synth.c - Additive synthesizer using miniaudio
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */


#include "sampler_internal.h"
#include <time.h>


/******************************************************************************
 * TYPE DEFINITIONS
 *****************************************************************************/

#define MAX_OSCILLATORS 256

typedef struct {
	ma_waveform waveform;
	ma_sound sound;
	ma_bool32 in_use;
	int voice_index;
} synth_oscillator_t;


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

#define GET_OSCILLATOR_FROM_HANDLE(handle_term, osc_var, slot_var) \
	do { \
		if (!PL_get_integer(handle_term, &slot_var)) { \
			return PL_type_error("integer", handle_term); \
		} \
		if (slot_var < 0 || slot_var >= MAX_OSCILLATORS || !g_oscillators[slot_var].in_use) { \
			return PL_existence_error("oscillator", handle_term); \
		} \
		osc_var = &g_oscillators[slot_var]; \
	} while(0)
/******************************************************************************
 * GLOBAL VARIABLES
 *****************************************************************************/

synth_voice_t g_voices[MAX_VOICES] = {{0}};
static synth_oscillator_t g_oscillators[MAX_OSCILLATORS] = {{0}};

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
	int i;

	if (index >= 0 && index < MAX_VOICES) {
		pthread_mutex_lock(&g_voices_mutex);

		if (g_voices[index].in_use) {
			free_effect_chain(g_voices[index].effect_chain);
			g_voices[index].effect_chain = NULL;

			if (g_voices[index].is_voice) {
				/* free all oscillators belonging to this voice */
				for (i = 0; i < MAX_OSCILLATORS; i++) {
					if (g_oscillators[i].in_use && g_oscillators[i].voice_index == index) {
						ma_sound_uninit(&g_oscillators[i].sound);
						ma_waveform_uninit(&g_oscillators[i].waveform);
						g_oscillators[i].in_use = MA_FALSE;
					}
				}
			}

			ma_sound_group_uninit(&g_voices[index].group);
			g_voices[index].in_use = MA_FALSE;
		}

		pthread_mutex_unlock(&g_voices_mutex);
	}
}

/*
 * allocate_oscillator_slot()
 * Finds a free oscillator slot and marks it as in use.
 * Returns slot index, or -1 if all slots are full.
 */
static int allocate_oscillator_slot(void)
{
	int i;
	int slot = -1;

	for (i = 0; i < MAX_OSCILLATORS; i++) {
		if (!g_oscillators[i].in_use) {
			g_oscillators[i].in_use = MA_TRUE;
			slot = i;
			break;
		}
	}

	return slot;
}

/*
 * free_oscillator_slot()
 * Frees an oscillator slot and its resources.
 */
static void free_oscillator_slot(int index)
{
	if (index >= 0 && index < MAX_OSCILLATORS && g_oscillators[index].in_use) {
		ma_sound_uninit(&g_oscillators[index].sound);
		ma_waveform_uninit(&g_oscillators[index].waveform);
		g_oscillators[index].in_use = MA_FALSE;
		g_oscillators[index].voice_index = -1;
	}
}


/******************************************************************************
 * VOICE MANAGEMENT
 *****************************************************************************/

/*
 * pl_synth_voice_create()
 * sampler_synth_voice_create(-Handle)
 * Creates a new empty voice group.
 */
static foreign_t pl_synth_voice_create(term_t handle)
{
	int slot;
	ma_result result;

	ENSURE_ENGINE_INITIALIZED();

	slot = allocate_voice_slot();
	if (slot < 0) {
		return PL_resource_error("voice_slots");
	}

	result = ma_sound_group_init(g_engine, 0, NULL, &g_voices[slot].group);
	if (result != MA_SUCCESS) {
		free_voice_slot(slot);
		return FALSE;
	}

	ma_sound_group_stop(&g_voices[slot].group);
	g_voices[slot].is_voice = MA_FALSE;
	g_voices[slot].effect_chain = NULL;

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
 * Starts playing a voice by starting all its oscillators.
 */
static foreign_t pl_synth_voice_start(term_t handle)
{
	int slot;
	int i;
	synth_voice_t* voice;

	GET_VOICE_FROM_HANDLE(handle, voice, slot);

	if (!voice->is_voice) {
		return FALSE;
	}

	ma_sound_group_start(&voice->group);

	for (i = 0; i < MAX_OSCILLATORS; i++) {
		if (g_oscillators[i].in_use && g_oscillators[i].voice_index == slot) {
			(void)ma_sound_start(&g_oscillators[i].sound);
		}
	}

	return TRUE;
}

/*
 * pl_synth_voice_stop()
 * sampler_synth_voice_stop(+Handle)
 * Stops playing a voice by stopping all its oscillators.
 */
static foreign_t pl_synth_voice_stop(term_t handle)
{
	int slot;
	int i;
	synth_voice_t* voice;

	GET_VOICE_FROM_HANDLE(handle, voice, slot);

	if (!voice->is_voice) {
		return TRUE;
	}

	for (i = 0; i < MAX_OSCILLATORS; i++) {
		if (g_oscillators[i].in_use && g_oscillators[i].voice_index == slot) {
			(void)ma_sound_stop(&g_oscillators[i].sound);
		}
	}

	ma_sound_group_stop(&voice->group);

	return TRUE;
}


/*
 * pl_synth_voice_fade()
 * sampler_synth_voice_fade(+Handle, +TargetVolume, +Ms)
 * Fades a voice to target volume over specified milliseconds.
 * Use -1.0 for TargetVolume to fade from current volume.
 */
static foreign_t pl_synth_voice_fade(term_t handle, term_t volume_term, term_t ms_term)
{
	int slot;
	synth_voice_t* voice;
	double volume;
	int ms;

	GET_VOICE_FROM_HANDLE(handle, voice, slot);

	if (!PL_get_float(volume_term, &volume)) {
		return PL_type_error("float", volume_term);
	}

	if (!PL_get_integer(ms_term, &ms)) {
		return PL_type_error("integer", ms_term);
	}

	if (ms < 0) {
		return PL_domain_error("non_negative_integer", ms_term);
	}

	ma_sound_group_set_fade_in_milliseconds(&voice->group, -1, (float)volume, (ma_uint64)ms);

	return TRUE;
}

/*
 * pl_synth_voice_set_pan()
 * sampler_synth_voice_set_pan(+Handle, +Pan)
 * Sets the pan position of a voice (-1.0 left, 0.0 center, 1.0 right).
 */
static foreign_t pl_synth_voice_set_pan(term_t handle, term_t pan_term)
{
	int slot;
	synth_voice_t* voice;
	double pan;

	GET_VOICE_FROM_HANDLE(handle, voice, slot);

	if (!PL_get_float(pan_term, &pan)) {
		return PL_type_error("float", pan_term);
	}

	if (pan < -1.0 || pan > 1.0) {
		return PL_domain_error("pan_range", pan_term);
	}

	ma_sound_group_set_pan(&voice->group, (float)pan);

	return TRUE;
}

/*
 * pl_synth_voice_get_pan()
 * sampler_synth_voice_get_pan(+Handle, -Pan)
 * Gets the pan position of a voice.
 */
static foreign_t pl_synth_voice_get_pan(term_t handle, term_t pan_term)
{
	int slot;
	synth_voice_t* voice;

	GET_VOICE_FROM_HANDLE(handle, voice, slot);

	return PL_unify_float(pan_term, ma_sound_group_get_pan(&voice->group));
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
 * OSCILLATOR MANAGEMENT
 *****************************************************************************/

/*
 * pl_synth_oscillator_add()
 * sampler_synth_oscillator_add(+VoiceHandle, +Frequency, +Phase, -OscHandle)
 * Adds a sine oscillator to a voice.
 */
static foreign_t pl_synth_oscillator_add(term_t voice_handle, term_t freq_term,
                                         term_t phase_term, term_t osc_handle)
{
	int voice_slot, osc_slot;
	synth_voice_t* voice;
	double frequency, phase;
	ma_waveform_config waveform_config;
	ma_format format;
	ma_uint32 channels;
	ma_uint32 sample_rate;
	ma_result result;

	GET_VOICE_FROM_HANDLE(voice_handle, voice, voice_slot);

	if (!PL_get_float(freq_term, &frequency)) {
		return PL_type_error("float", freq_term);
	}
	if (frequency <= 0.0) {
		return PL_domain_error("positive_frequency", freq_term);
	}

	if (!PL_get_float(phase_term, &phase)) {
		return PL_type_error("float", phase_term);
	}

	osc_slot = allocate_oscillator_slot();
	if (osc_slot < 0) {
		return PL_resource_error("oscillator_slots");
	}

	get_engine_format_info(&format, &channels, &sample_rate);

	waveform_config = ma_waveform_config_init(
		format,
		channels,
		sample_rate,
		ma_waveform_type_sine,
		1.0,
		frequency
	);

	result = ma_waveform_init(&waveform_config, &g_oscillators[osc_slot].waveform);
	if (result != MA_SUCCESS) {
		free_oscillator_slot(osc_slot);
		return FALSE;
	}

	/* Set initial phase */
	ma_waveform_seek_to_pcm_frame(&g_oscillators[osc_slot].waveform,
		(ma_uint64)(phase * sample_rate / frequency));

	result = ma_sound_init_from_data_source(
		g_engine,
		&g_oscillators[osc_slot].waveform,
		MA_SOUND_FLAG_NO_SPATIALIZATION | MA_SOUND_FLAG_STREAM,
		&voice->group,
		&g_oscillators[osc_slot].sound
	);
	if (result != MA_SUCCESS) {
		ma_waveform_uninit(&g_oscillators[osc_slot].waveform);
		free_oscillator_slot(osc_slot);
		return FALSE;
	}

	ma_sound_set_looping(&g_oscillators[osc_slot].sound, MA_TRUE);
	g_oscillators[osc_slot].voice_index = voice_slot;
	voice->is_voice = MA_TRUE;

	/* If voice is already playing, start the oscillator immediately */
	if (ma_sound_group_is_playing(&voice->group)) {
		ma_sound_start(&g_oscillators[osc_slot].sound);
	}

	return PL_unify_integer(osc_handle, osc_slot);
}

/*
 * pl_synth_oscillator_remove()
 * sampler_synth_oscillator_remove(+OscHandle)
 * Removes an oscillator from its voice.
 */
static foreign_t pl_synth_oscillator_remove(term_t osc_handle)
{
	int osc_slot, voice_slot, i;
	synth_oscillator_t* osc;
	ma_bool32 has_oscillators;

	GET_OSCILLATOR_FROM_HANDLE(osc_handle, osc, osc_slot);

	voice_slot = osc->voice_index;

	ma_sound_uninit(&osc->sound);
	ma_waveform_uninit(&osc->waveform);
	osc->in_use = MA_FALSE;
	osc->voice_index = -1;

	/* Check if voice still has any oscillators */
	has_oscillators = MA_FALSE;
	for (i = 0; i < MAX_OSCILLATORS; i++) {
		if (g_oscillators[i].in_use && g_oscillators[i].voice_index == voice_slot) {
			has_oscillators = MA_TRUE;
			break;
		}
	}

	if (!has_oscillators && voice_slot >= 0 && voice_slot < MAX_VOICES) {
		g_voices[voice_slot].is_voice = MA_FALSE;
	}

	return TRUE;
}

/*
 * pl_synth_oscillator_fade()
 * sampler_synth_oscillator_fade(+OscHandle, +TargetVolume, +Ms)
 * Fades an oscillator to target volume over specified milliseconds.
 */
static foreign_t pl_synth_oscillator_fade(term_t osc_handle, term_t volume_term, term_t ms_term)
{
	int osc_slot;
	synth_oscillator_t* osc;
	double volume;
	int ms;

	GET_OSCILLATOR_FROM_HANDLE(osc_handle, osc, osc_slot);

	if (!PL_get_float(volume_term, &volume)) {
		return PL_type_error("float", volume_term);
	}

	if (!PL_get_integer(ms_term, &ms)) {
		return PL_type_error("integer", ms_term);
	}

	if (ms < 0) {
		return PL_domain_error("non_negative_integer", ms_term);
	}

	ma_sound_set_fade_in_milliseconds(&osc->sound, -1, (float)volume, (ma_uint64)ms);

	return TRUE;
}

/*
 * pl_synth_oscillator_set_frequency()
 * sampler_synth_oscillator_set_frequency(+OscHandle, +Frequency)
 * Sets the frequency of an oscillator.
 */
static foreign_t pl_synth_oscillator_set_frequency(term_t osc_handle, term_t freq_term)
{
	int osc_slot;
	synth_oscillator_t* osc;
	double frequency;

	GET_OSCILLATOR_FROM_HANDLE(osc_handle, osc, osc_slot);

	if (!PL_get_float(freq_term, &frequency)) {
		return PL_type_error("float", freq_term);
	}
	if (frequency <= 0.0) {
		return PL_domain_error("positive_frequency", freq_term);
	}

	ma_waveform_set_frequency(&osc->waveform, frequency);

	return TRUE;
}

/*
 * pl_synth_oscillator_get_frequency()
 * sampler_synth_oscillator_get_frequency(+OscHandle, -Frequency)
 * Gets the frequency of an oscillator.
 */
static foreign_t pl_synth_oscillator_get_frequency(term_t osc_handle, term_t freq_term)
{
	int osc_slot;
	synth_oscillator_t* osc;

	GET_OSCILLATOR_FROM_HANDLE(osc_handle, osc, osc_slot);

	return PL_unify_float(freq_term, osc->waveform.config.frequency);
}

/*
 * pl_synth_oscillator_set_phase()
 * sampler_synth_oscillator_set_phase(+OscHandle, +Phase)
 * Sets the phase of an oscillator (0.0 to 1.0).
 */
static foreign_t pl_synth_oscillator_set_phase(term_t osc_handle, term_t phase_term)
{
	int osc_slot;
	synth_oscillator_t* osc;
	double phase;
	ma_uint32 sample_rate;

	GET_OSCILLATOR_FROM_HANDLE(osc_handle, osc, osc_slot);

	if (!PL_get_float(phase_term, &phase)) {
		return PL_type_error("float", phase_term);
	}

	get_engine_format_info(NULL, NULL, &sample_rate);
	ma_waveform_seek_to_pcm_frame(&osc->waveform,
		(ma_uint64)(phase * sample_rate / osc->waveform.config.frequency));

	return TRUE;
}

/*
 * pl_synth_oscillator_get_phase()
 * sampler_synth_oscillator_get_phase(+OscHandle, -Phase)
 * Gets the phase of an oscillator (0.0 to 1.0).
 */
static foreign_t pl_synth_oscillator_get_phase(term_t osc_handle, term_t phase_term)
{
	int osc_slot;
	synth_oscillator_t* osc;
	double phase;

	GET_OSCILLATOR_FROM_HANDLE(osc_handle, osc, osc_slot);

	/* ma_waveform stores phase as 'time' which is cycles completed */
	phase = osc->waveform.time - floor(osc->waveform.time);

	return PL_unify_float(phase_term, phase);
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
	PL_register_foreign("sampler_synth_voice_fade", 3, pl_synth_voice_fade, 0);
	PL_register_foreign("sampler_synth_voice_set_pan", 2, pl_synth_voice_set_pan, 0);
	PL_register_foreign("sampler_synth_voice_get_pan", 2, pl_synth_voice_get_pan, 0);
	PL_register_foreign("sampler_synth_voice_unload", 1, pl_synth_voice_unload, 0);
	PL_register_foreign("sampler_synth_oscillator_add", 4, pl_synth_oscillator_add, 0);
	PL_register_foreign("sampler_synth_oscillator_remove", 1, pl_synth_oscillator_remove, 0);
	PL_register_foreign("sampler_synth_oscillator_fade", 3, pl_synth_oscillator_fade, 0);
	PL_register_foreign("sampler_synth_oscillator_set_frequency", 2, pl_synth_oscillator_set_frequency, 0);
	PL_register_foreign("sampler_synth_oscillator_get_frequency", 2, pl_synth_oscillator_get_frequency, 0);
	PL_register_foreign("sampler_synth_oscillator_set_phase", 2, pl_synth_oscillator_set_phase, 0);
	PL_register_foreign("sampler_synth_oscillator_get_phase", 2, pl_synth_oscillator_get_phase, 0);
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
