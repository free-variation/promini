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
	EFFECT_DELAY,
	EFFECT_PING_PONG_DELAY
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

/* Band-pass filter node */
typedef struct {
	ma_bpf_node node;
	double cutoff_frequency;
	ma_uint32 order;
} bpf_node_t;

/* Delay node */
typedef struct {
	ma_delay_node node;
	ma_uint32 delay_in_frames;
	float wet;
	float dry;
	float decay;
} delay_node_t;

/* Ping-pong delay node */
typedef struct {
	ma_node_base base;
	float* buffer_l;
	float* buffer_r;
	ma_uint32 buffer_size;
	ma_uint32 cursor;
	ma_uint32 delay_in_frames;
	float feedback;
	float wet;
	float dry;

	ma_uint32 smoothing_mode; /* 0 = none, 1 = smooth (pitch shift), 2 = crossfade */
	ma_uint32 target_delay_in_frames; /* for smoothing mode */
	ma_uint32 old_delay_in_frames; /* for crossfade mode */
	ma_uint32 crossfade_counter;
	ma_uint32 crossfade_length;
	float smoothing_speed; /* samples per frame to move, e.g. 0.1 to 8.0 */
	float smooth_delay_position; /* fractional delay position for smooth transitions */
} ping_pong_delay_node_t;

/* Delay line with multiple tap points (for reverb) */
typedef struct {
	float* buffer;
	ma_uint32 mask;
	ma_uint32 main_delay;
	ma_uint32 tap1, tap2, tap3;
} reverb_delay_line_t;

/* Tank half for reverb (one side of figure-8) */
typedef struct {
	float* decay_diff1_buf;
	ma_uint32 decay_diff1_mask;
	ma_uint32 decay_diff1_delay;

	reverb_delay_line_t pre_damp;

	float damping_state;

	float* decay_diff2_buf;
	ma_uint32 decay_diff2_mask;
	ma_uint32 decay_diff2_delay;
	ma_uint32 decay_diff2_tap1;  /* output tap offset */
	ma_uint32 decay_diff2_tap2;  /* output tap offset */

	reverb_delay_line_t post_damp;
} reverb_tank_half_t;

/* Complete reverb channel (L or R) */
typedef struct {
	float* predelay_buf;
	ma_uint32 predelay_mask;

	float input_lpf_state;

	float* diffuser_buf[4];
	ma_uint32 diffuser_mask[4];
	ma_uint32 diffuser_delay[4];

	reverb_tank_half_t tank[2];
} reverb_channel_t;

/* Pitch shifter for shimmer effect (4-grain overlap-add) */
typedef struct {
	float* buffer;
	ma_uint32 size;
	ma_uint32 write_pos;
	/* 4 grains, each with: delay position, ramp level, ramp slope */
	float dsamp[4];
	float dsamp_slope[4];
	float ramp[4];
	float ramp_slope[4];
	ma_uint32 counter;
	int stage;
} reverb_pitchshift_t;

/* True stereo Dattorro reverb */
typedef struct {
	ma_node_base base;

	/* true stereo: two complete channels */
	reverb_channel_t channels[2];

	/* shimmer pitch shifters (2 per channel for dual shimmer) */
	reverb_pitchshift_t shimmer[2][2];

	/* user parameters */
	float predelay_ms;
	float bandwidth;
	float decay;
	float damping;
	float mod_rate;
	float mod_depth;
	float shimmer1_shift; /* semitones, 0 = off */
	float shimmer1_mix;   /* 0-1 */
	float shimmer2_shift;
	float shimmer2_mix;
	float width;
	float cross_feed;
	float low_cut;  /* Hz, 0 = disabled */
	float high_cut; /* Hz, 0 = disabled */
	float wet;
	float dry;

	/* internal state */
	ma_uint32 t;
	float mod_phase;
	float hpf_l, hpf_r;
	float lpf_l, lpf_r;
} reverb_node_t;
/* Shared sound slots array */

extern sound_slot_t g_sounds[MAX_SOUNDS];

/* Helper functions (implemented in sampler.c) */
extern ma_result attach_effect_node_to_sound(sound_slot_t* sound_slot, ma_node_base* effect_node, effect_type_t type);
extern void get_engine_format_info(ma_format* format, ma_uint32* channels, ma_uint32* sampleRate);

/* Reverb functions (implemented in reverb.c) */
extern ma_node_vtable reverb_vtable;
extern ma_result init_reverb_node(reverb_node_t* reverb, ma_uint32 sample_rate);
extern void free_reverb_node(reverb_node_t* reverb);

/* Module registration functions */
extern install_t sampler_register_predicates(void);
extern install_t synth_register_predicates(void);
extern install_t effects_register_predicates(void);

/* Module cleanupfunctions */
extern install_t uninstall_sampler(void);
extern install_t uninstall_synth(void);
extern install_t uninstall_effects(void);

#endif /* SAMPLER_INTERNAL_H */
