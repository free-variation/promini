/*
 * sampler_internal.h - Shared definitions for sampler and synth modules
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#ifndef SAMPLER_INTERNAL_H
#define SAMPLER_INTERNAL_H

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <pthread.h>
#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include "../../include/miniaudio.h"

/*
 * Shared global engine
 * Initialized by sampler.c, shared with synth.c
 */
extern ma_engine* g_engine;

/* Forward declaration for initialization */
extern foreign_t pl_sampler_init(void);

/*
 * Macro to ensure engine is initialized before operations
 */
#define ENSURE_ENGINE_INITIALIZED() \
    do { \
        if (g_engine == NULL) { \
            if (!pl_sampler_init()) { \
                return FALSE; \
            } \
        } \
    } while(0)

/*
 * Thread safety - mutexes for protecting global state
 */
extern pthread_mutex_t g_sounds_mutex;
extern pthread_mutex_t g_data_buffers_mutex;
extern pthread_mutex_t g_capture_devices_mutex;
extern pthread_mutex_t g_voices_mutex;

/*
 * Sound management
 */
#define MAX_SOUNDS 1024

/* Effect types */
typedef enum {
	EFFECT_NONE = 0,
	EFFECT_BITCRUSH,
	EFFECT_ENVELOPE,
	EFFECT_REVERB,
	EFFECT_LPF,
	EFFECT_HPF,
	EFFECT_BPF,
	EFFECT_NOTCH,
	EFFECT_PEAK,
	EFFECT_LOSHELF,
	EFFECT_HISHELF
} effect_type_t;

/* Effect chain node */
typedef struct effect_node {
	effect_type_t type;
	ma_node_base* effect_node;
	struct effect_node* next;
} effect_node_t;

/* Sound slot */
typedef struct {
	ma_sound* sound;
	ma_audio_buffer* audio_buffer;
	int data_buffer_index;
	ma_bool32 in_use;
	effect_node_t* effect_chain;
} sound_slot_t;

/* Helper macro to validate and retrieve sound from handle */
#define GET_SOUND_WITH_SLOT(handle_term, sound_var, slot_var) \
    do { \
        if (!PL_get_integer(handle_term, &slot_var)) { \
            return PL_type_error("integer", handle_term); \
        } \
        if (slot_var < 0 || slot_var >= MAX_SOUNDS || !g_sounds[slot_var].in_use) { \
            return PL_existence_error("sound", handle_term); \
        } \
        sound_var = g_sounds[slot_var].sound; \
    } while (0)

/* Bitcrush effect node */
typedef struct {
	ma_node_base base;
	ma_uint32 target_bits;
	ma_uint32 target_sample_rate;
	float* hold_samples;
	ma_uint64 hold_counter;
	ma_uint32 hold_interval;
} bitcrush_node_t;

/* ADBR envelope node */
typedef struct {
	ma_node_base base;
	float attack;
	float decay;
	float Break;
	float release;
	float break_level;
	float duration_ms;
	ma_bool32 loop;
	ma_uint32 stage;
	float stage_progress;
} adbr_envelope_node_t;

/* Low-pass filter node */
typedef struct {
	ma_lpf_node node;
	double cutoff_frequency;
	ma_uint32 order;
} lpf_node_t;

/* High-pass filter node */
typedef struct {
	ma_hpf_node node;
	double cutoff_frequency;
	ma_uint32 order;
} hpf_node_t;

/* Shared sound slots array */
extern sound_slot_t g_sounds[MAX_SOUNDS];

/* Helper functions (implemented in sampler.c) */
extern ma_result attach_effect_node_to_sound(sound_slot_t* sound_slot, ma_node_base* effect_node, effect_type_t type);
extern void get_engine_format_info(ma_format* format, ma_uint32* channels, ma_uint32* sampleRate);

/* Module registration functions */
extern install_t sampler_register_predicates(void);
extern install_t synth_register_predicates(void);
extern install_t effects_register_predicates(void);

/* Module cleanupfunctions */
extern install_t uninstall_sampler(void);
extern install_t uninstall_synth(void);
extern install_t uninstall_effects(void);

#endif /* SAMPLER_INTERNAL_H */
