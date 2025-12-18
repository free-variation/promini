/*
 * synth.c - Additive synthesizer using miniaudio
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */


#include "promini.h"
#include <time.h>


#define GET_VOICE_FROM_HANDLE(handle_term, voice_var, slot_var) \
	do { \
		if (!get_typed_handle(handle_term, "voice", &slot_var)) { \
			return PL_type_error("voice", handle_term); \
		} \
		if (slot_var < 0 || slot_var >= MAX_VOICES || !g_voices[slot_var].in_use) { \
			return PL_existence_error("voice", handle_term); \
		} \
		voice_var = &g_voices[slot_var]; \
	} while(0)

#define GET_OSCILLATOR_FROM_HANDLE(handle_term, osc_var, slot_var) \
	do { \
		if (!get_typed_handle(handle_term, "oscillator", &slot_var)) { \
			return PL_type_error("oscillator", handle_term); \
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
synth_oscillator_t g_oscillators[MAX_OSCILLATORS] = {{0}};

/*
 * Thread safety - mutex for protecting voice allocation
 */
pthread_mutex_t g_voices_mutex = PTHREAD_MUTEX_INITIALIZER;

/* forward declarations */
void free_voice_pool(int count, int *slots);

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
						if (g_oscillators[i].is_noise) {
							ma_noise_uninit(&g_oscillators[i].source.noise, NULL);
						} else {
							ma_waveform_uninit(&g_oscillators[i].source.waveform);
						}
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
 * init_voice_at_slot()
 * Initialize a voice at an already-allocated slot.
 */
static ma_result init_voice_at_slot(int slot)
{
	ma_result result;

	result = ma_sound_group_init(g_engine, 0, NULL, &g_voices[slot].group);
	if (result != MA_SUCCESS) {
		return result;
	}

	ma_sound_group_stop(&g_voices[slot].group);
	g_voices[slot].is_voice = MA_FALSE;
	g_voices[slot].effect_chain = NULL;
	
	return MA_SUCCESS;
}

/*
 * allocate_voice_pool()
 * Allocate multiple voice slots for polyphonic keyboard.
 * Stores slot indices in the provided array.
 * Returns MA_SUCCESS or error code.
 */
ma_result allocate_voice_pool(int count, int *slots)
{
	int i;
	ma_result result;

	for (i = 0; i < count; i++) {
		slots[i] = allocate_voice_slot();
		if (slots[i] < 0) {
			free_voice_pool(i, slots);
			return MA_OUT_OF_MEMORY;
		}

		result = init_voice_at_slot(slots[i]);
		if (result != MA_SUCCESS) {
			free_voice_slot(slots[i]);
			free_voice_pool(i, slots);
			return result;
		}
	}

	return MA_SUCCESS;
}

/*
 * free_voice_pool()
 * Free multiple voice slots.
 */
void free_voice_pool(int count, int *slots)
{
	int i;

	for (i = 0; i < count; i++) {
		if (slots[i] >= 0) {
			free_voice_slot(slots[i]);
		}
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
			g_oscillators[i].is_noise = MA_FALSE;
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
		if (g_oscillators[index].is_noise) {
			ma_noise_uninit(&g_oscillators[index].source.noise, NULL);
		} else {
			ma_waveform_uninit(&g_oscillators[index].source.waveform);
		}
		g_oscillators[index].in_use = MA_FALSE;
		g_oscillators[index].voice_index = -1;
	}
}


/******************************************************************************
 * VOICE MANAGEMENT
 *****************************************************************************/

/*
 * pl_synth_voice_init()
 * synth_voice_init(-Voice)
 * Creates a new empty voice group. Returns voice(N).
 */
static foreign_t pl_synth_voice_init(term_t handle)
{
	int slot;
	ma_result result;

	ENSURE_ENGINE_INITIALIZED();

	slot = allocate_voice_slot();
	if (slot < 0) {
		return PL_resource_error("voice_slots");
	}

	result = init_voice_at_slot(slot);
	if (result != MA_SUCCESS) {
		free_voice_slot(slot);
		return FALSE;
	}

	return unify_typed_handle(handle, "voice", slot);
}

/*
 * pl_synth_voices_in_use()
 * synth_voices_in_use(-Count)
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
 * synth_voice_start(+Voice)
 * Starts playing a voice by starting all its oscillators.
 */
static foreign_t pl_synth_voice_start(term_t handle)
{
	int slot;
	int i;
	synth_voice_t *voice;

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
 * synth_voice_stop(+Voice)
 * Stops playing a voice by stopping all its oscillators.
 */
static foreign_t pl_synth_voice_stop(term_t handle)
{
	int slot;
	int i;
	synth_voice_t *voice;

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
 * pl_synth_voice_uninit()
 * synth_voice_uninit(+Voice)
 * Unloads a voice and frees its resources.
 */
static foreign_t pl_synth_voice_uninit(term_t handle)
{
	int slot;

	if (!get_typed_handle(handle, "voice", &slot)) {
		return PL_type_error("voice", handle);
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
 * synth_oscillator_add(+Voice, +Frequency, +Phase, -Oscillator)
 * Adds a sine oscillator to a voice. Returns oscillator(N).
 */
static foreign_t pl_synth_oscillator_add(term_t voice_handle, term_t freq_term,
                                         term_t phase_term, term_t osc_handle)
{
	int voice_slot, osc_slot;
	synth_voice_t *voice;
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

	result = ma_waveform_init(&waveform_config, &g_oscillators[osc_slot].source.waveform);
	if (result != MA_SUCCESS) {
		free_oscillator_slot(osc_slot);
		return FALSE;
	}

	/* Set initial phase */
	ma_waveform_seek_to_pcm_frame(&g_oscillators[osc_slot].source.waveform,
		(ma_uint64)(phase * sample_rate / frequency));

	result = ma_sound_init_from_data_source(
		g_engine,
		&g_oscillators[osc_slot].source.waveform,
		MA_SOUND_FLAG_NO_SPATIALIZATION | MA_SOUND_FLAG_STREAM,
		&voice->group,
		&g_oscillators[osc_slot].sound
	);
	if (result != MA_SUCCESS) {
		ma_waveform_uninit(&g_oscillators[osc_slot].source.waveform);
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

	return unify_typed_handle(osc_handle, "oscillator", osc_slot);
}

/*
 * pl_synth_oscillator_remove()
 * synth_oscillator_remove(+Oscillator)
 * Removes an oscillator from its voice.
 */
static foreign_t pl_synth_oscillator_remove(term_t osc_handle)
{
	int osc_slot, voice_slot, i;
	synth_oscillator_t *osc;
	ma_bool32 has_oscillators;

	GET_OSCILLATOR_FROM_HANDLE(osc_handle, osc, osc_slot);

	voice_slot = osc->voice_index;

	ma_sound_uninit(&osc->sound);
	if (osc->is_noise) {
		ma_noise_uninit(&osc->source.noise, NULL);
	} else {
		ma_waveform_uninit(&osc->source.waveform);
	}
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
 * pl_synth_noise_add()
 * synth_noise_add(+Voice, +Type, -Oscillator)
 * Adds a noise generator to a voice. Returns oscillator(N).
 * Type is one of: white, pink, brownian
 */
static foreign_t pl_synth_noise_add(term_t voice_handle, term_t type_term, term_t noise_handle)
{
	int voice_slot, osc_slot;
	synth_voice_t *voice;
	char *type_str;
	ma_noise_type noise_type;
	ma_noise_config noise_config;
	ma_format format;
	ma_uint32 channels, sample_rate;
	ma_result result;

	GET_VOICE_FROM_HANDLE(voice_handle, voice, voice_slot);

	if (!PL_get_atom_chars(type_term, &type_str)) {
		return PL_type_error("atom", type_term);
	}

	if (strcmp(type_str, "white") == 0) {
		noise_type = ma_noise_type_white;
	} else if (strcmp(type_str, "pink") == 0) {
		noise_type = ma_noise_type_pink;
	} else if (strcmp(type_str, "brownian") == 0) {
		noise_type = ma_noise_type_brownian;
	} else {
		return PL_domain_error("noise_type", type_term);
	}

	osc_slot = allocate_oscillator_slot();
	if (osc_slot < 0) {
		return PL_resource_error("oscillator_slots");
	}

	g_oscillators[osc_slot].is_noise = MA_TRUE;

	get_engine_format_info(&format, &channels, &sample_rate);

	noise_config = ma_noise_config_init(
		format,
		channels,
		noise_type,
		0,    /* seed: 0 = default */
		1.0   /* amplitude */
	);

	result = ma_noise_init(&noise_config, NULL, &g_oscillators[osc_slot].source.noise);
	if (result != MA_SUCCESS) {
		free_oscillator_slot(osc_slot);
		return FALSE;
	}

	result = ma_sound_init_from_data_source(
		g_engine,
		&g_oscillators[osc_slot].source.noise,
		MA_SOUND_FLAG_NO_SPATIALIZATION | MA_SOUND_FLAG_STREAM,
		&voice->group,
		&g_oscillators[osc_slot].sound
	);
	if (result != MA_SUCCESS) {
		ma_noise_uninit(&g_oscillators[osc_slot].source.noise, NULL);
		free_oscillator_slot(osc_slot);
		return FALSE;
	}

	ma_sound_set_looping(&g_oscillators[osc_slot].sound, MA_TRUE);
	g_oscillators[osc_slot].voice_index = voice_slot;
	voice->is_voice = MA_TRUE;

	/* If voice is already playing, start the noise immediately */
	if (ma_sound_group_is_playing(&voice->group)) {
		ma_sound_start(&g_oscillators[osc_slot].sound);
	}

	return unify_typed_handle(noise_handle, "oscillator", osc_slot);
}

/*
 * pl_synth_oscillator_fade()
 * synth_oscillator_fade(+Oscillator, +TargetVolume, +Ms)
 * Fades an oscillator to target volume over specified milliseconds.
 */
static foreign_t pl_synth_oscillator_fade(term_t osc_handle, term_t volume_term, term_t ms_term)
{
	int osc_slot;
	synth_oscillator_t *osc;
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
 * pl_synth_oscillator_set_volume()
 * synth_oscillator_set_volume(+Oscillator, +Volume)
 * Sets the volume of an oscillator or noise source.
 */
static foreign_t pl_synth_oscillator_set_volume(term_t osc_handle, term_t volume_term)
{
	int osc_slot;
	synth_oscillator_t *osc;
	double volume;

	GET_OSCILLATOR_FROM_HANDLE(osc_handle, osc, osc_slot);

	if (!PL_get_float(volume_term, &volume)) {
		return PL_type_error("float", volume_term);
	}

	ma_sound_set_volume(&osc->sound, (float)volume);

	return TRUE;
}

/*
 * pl_synth_oscillator_get_volume()
 * synth_oscillator_get_volume(+Oscillator, -Volume)
 * Gets the volume of an oscillator or noise source.
 */
static foreign_t pl_synth_oscillator_get_volume(term_t osc_handle, term_t volume_term)
{
	int osc_slot;
	synth_oscillator_t *osc;

	GET_OSCILLATOR_FROM_HANDLE(osc_handle, osc, osc_slot);

	return PL_unify_float(volume_term, ma_sound_get_volume(&osc->sound));
}

/*
 * pl_synth_oscillator_set_frequency()
 * synth_oscillator_set_frequency(+Oscillator, +Frequency)
 * Sets the frequency of an oscillator.
 */
static foreign_t pl_synth_oscillator_set_frequency(term_t osc_handle, term_t freq_term)
{
	int osc_slot;
	synth_oscillator_t *osc;
	double frequency;

	GET_OSCILLATOR_FROM_HANDLE(osc_handle, osc, osc_slot);

	if (!PL_get_float(freq_term, &frequency)) {
		return PL_type_error("float", freq_term);
	}
	if (frequency <= 0.0) {
		return PL_domain_error("positive_frequency", freq_term);
	}

	ma_waveform_set_frequency(&osc->source.waveform, frequency);

	return TRUE;
}

/*
 * pl_synth_oscillator_get_frequency()
 * synth_oscillator_get_frequency(+Oscillator, -Frequency)
 * Gets the frequency of an oscillator.
 */
static foreign_t pl_synth_oscillator_get_frequency(term_t osc_handle, term_t freq_term)
{
	int osc_slot;
	synth_oscillator_t *osc;

	GET_OSCILLATOR_FROM_HANDLE(osc_handle, osc, osc_slot);

	return PL_unify_float(freq_term, osc->source.waveform.config.frequency);
}

/*
 * pl_synth_oscillator_set_phase()
 * synth_oscillator_set_phase(+Oscillator, +Phase)
 * Sets the phase of an oscillator (0.0 to 1.0).
 */
static foreign_t pl_synth_oscillator_set_phase(term_t osc_handle, term_t phase_term)
{
	int osc_slot;
	synth_oscillator_t *osc;
	double phase;
	ma_uint32 sample_rate;

	GET_OSCILLATOR_FROM_HANDLE(osc_handle, osc, osc_slot);

	if (!PL_get_float(phase_term, &phase)) {
		return PL_type_error("float", phase_term);
	}

	get_engine_format_info(NULL, NULL, &sample_rate);
	ma_waveform_seek_to_pcm_frame(&osc->source.waveform,
		(ma_uint64)(phase * sample_rate / osc->source.waveform.config.frequency));

	return TRUE;
}

/*
 * pl_synth_oscillator_get_phase()
 * synth_oscillator_get_phase(+Oscillator, -Phase)
 * Gets the phase of an oscillator (0.0 to 1.0).
 */
static foreign_t pl_synth_oscillator_get_phase(term_t osc_handle, term_t phase_term)
{
	int osc_slot;
	synth_oscillator_t *osc;
	double phase;

	GET_OSCILLATOR_FROM_HANDLE(osc_handle, osc, osc_slot);

	/* ma_waveform stores phase as 'time' which is cycles completed */
	phase = osc->source.waveform.time - floor(osc->source.waveform.time);

	return PL_unify_float(phase_term, phase);
}


/*
 * voice_set_frequency()
 * Sets the frequency of a voice by scaling all its oscillators.
 * Uses th efrist oscillator as the reference (tonic).
 */
void voice_set_frequency(int voice_slot, double frequency)
{
	int i;
	int first_osc = -1;
	double current_freq, ratio;

	/* find first oscillator for this voice */
	for (i = 0; i < MAX_OSCILLATORS; i++) {
		if (g_oscillators[i].in_use && !g_oscillators[i].is_noise &&
				g_oscillators[i].voice_index == voice_slot) {
			first_osc = i;
			break;
		}
	}

	if (first_osc < 0) return;

	current_freq = g_oscillators[first_osc].source.waveform.config.frequency;
	if (current_freq <= 0.0) return;

	ratio = frequency / current_freq;

	/* scale all oscillators */
	for (i = 0; i < MAX_OSCILLATORS; i++) {
		if (g_oscillators[i].in_use && !g_oscillators[i].is_noise &&
				g_oscillators[i].voice_index == voice_slot) {
			double new_freq = g_oscillators[i].source.waveform.config.frequency * ratio;
			ma_waveform_set_frequency(&g_oscillators[i].source.waveform, new_freq);
		}
	}
}

/*
 * pl_synth_voice_set_frequency()
 * synth_voice_set_frequency(+Voice, +Frequency)
 * Sets the frequency of a voice by scaling all its oscillators.
 */
static foreign_t pl_synth_voice_set_frequency(term_t voice_handle, term_t freq_term)
{
	int voice_slot;
	synth_voice_t *voice;
	double frequency;

	GET_VOICE_FROM_HANDLE(voice_handle, voice, voice_slot);

	if (!PL_get_float(freq_term, &frequency)) {
		return PL_type_error("float", freq_term);
	}
	if (frequency <= 0.0) {
		return PL_domain_error("positive_frequency", freq_term);
	}

	voice_set_frequency(voice_slot, frequency);

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
	PL_register_foreign("synth_voices_in_use", 1, pl_synth_voices_in_use, 0);
	PL_register_foreign("synth_voice_init", 1, pl_synth_voice_init, 0);
	PL_register_foreign("synth_voice_start", 1, pl_synth_voice_start, 0);
	PL_register_foreign("synth_voice_stop", 1, pl_synth_voice_stop, 0);
	PL_register_foreign("synth_voice_uninit", 1, pl_synth_voice_uninit, 0);
	PL_register_foreign("synth_oscillator_add", 4, pl_synth_oscillator_add, 0);
	PL_register_foreign("synth_oscillator_remove", 1, pl_synth_oscillator_remove, 0);
	PL_register_foreign("synth_noise_add", 3, pl_synth_noise_add, 0);
	PL_register_foreign("synth_oscillator_fade", 3, pl_synth_oscillator_fade, 0);
	PL_register_foreign("synth_oscillator_set_volume", 2, pl_synth_oscillator_set_volume, 0);
	PL_register_foreign("synth_oscillator_get_volume", 2, pl_synth_oscillator_get_volume, 0);
	PL_register_foreign("synth_oscillator_set_frequency", 2, pl_synth_oscillator_set_frequency, 0);
	PL_register_foreign("synth_oscillator_get_frequency", 2, pl_synth_oscillator_get_frequency, 0);
	PL_register_foreign("synth_oscillator_set_phase", 2, pl_synth_oscillator_set_phase, 0);
	PL_register_foreign("synth_oscillator_get_phase", 2, pl_synth_oscillator_get_phase, 0);
	PL_register_foreign("synth_voice_set_frequency", 2, pl_synth_voice_set_frequency, 0);
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
