/*
 * promini.h - Shared definitions for promini modules
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

#ifndef PROMINI_H
#define PROMINI_H

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <pthread.h>
#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include "../../include/miniaudio.h"

/*
 * Shared global engine
 * Initialized by promini.c, shared with other modules
 */
extern ma_engine* g_engine;

/* Forward declaration for initialization */
extern foreign_t pl_promini_init(void);

/*
 * Macro to ensure engine is initialized before operations
 */
#define ENSURE_ENGINE_INITIALIZED() \
    do { \
        if (g_engine == NULL) { \
            if (!pl_promini_init()) { \
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
#define MAX_DATA_BUFFERS 256

typedef struct {
	ma_audio_buffer* buffer;
	void *pData;
	ma_uint32 refcount;
	ma_bool32 in_use;
} data_slot_t;

/* Effect types */
typedef enum {
	EFFECT_NONE = 0,
	EFFECT_BITCRUSH,
	EFFECT_REVERB,
	EFFECT_LPF,
	EFFECT_HPF,
	EFFECT_BPF,
	EFFECT_DELAY,
	EFFECT_PING_PONG_DELAY,
	EFFECT_PAN
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

/*
 * Synth voice management
 */
#define MAX_VOICES 256

/* Synth voice slot */
typedef struct {
	ma_sound_group group;
	ma_bool32 in_use;
	ma_bool32 is_voice;
	effect_node_t* effect_chain;
} synth_voice_t;

#define MAX_OSCILLATORS 256

typedef struct {
	union {
		ma_waveform waveform;
		ma_noise noise;
	} source;
	ma_sound sound;
	ma_bool32 in_use;
	ma_bool32 is_noise;
	int voice_index;
} synth_oscillator_t;



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
	float decay_diff1_out;

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
	float mod_phase[4];
	float hpf_l, hpf_r;
	float lpf_l, lpf_r;
	float dc_block_l, dc_block_r;
} reverb_node_t;

/* pan effect node */
typedef struct {
	ma_node_base base;
	float current_pan;
	float target_pan;
} pan_node_t;

/*
 * Modulation system
 */

#define MAX_MOD_SOURCES 64
#define MAX_MOD_ROUTES 256

/* modulation source types */
typedef enum {
	MOD_SOURCE_NONE = 0,
	MOD_SOURCE_WAVEFORM,
	MOD_SOURCE_NOISE,
	MOD_SOURCE_SAMPLER,
	MOD_SOURCE_ENVELOPE
} mod_source_type_t;

/* modulation source */
typedef struct {
	mod_source_type_t type;
	ma_bool32 in_use;
	union {
		ma_waveform waveform;
		ma_noise noise;
		struct {
			int data_slot;
			float cursor;
			float rate;
		} sampler;
		struct {
			float attack;      /* proportion of duration */
			float decay;       /* proportion of duration */
			float brk;         /* proportion of duration */
			float release;     /* proportion of duration */
			float break_level; /* level at break point (0-1) */
			float duration_ms; /* total envelope time */
			ma_bool32 loop;    /* restart after release */
			ma_uint32 stage;   /* 0=attack, 1=decay, 2=break, 3=release, 4=done */
			float stage_progress; /* 0-1 within current stage */
		} envelope;
	} source;
	ma_bool32 sh_enabled;
	ma_uint32 sh_interval;
	ma_uint32 sh_counter;
	float sh_held_value;
	float current_value;
} mod_source_t;

/* modulation setter function */
typedef void (*mod_setter_t)(void* target, float value, ma_uint32 frame_count);

/* modulation route */
typedef struct {
	ma_bool32 in_use;
	int source_slot;
	void* target;
	mod_setter_t setter;
	float depth;
	float offset;
	float slew;
	float current_value;
} mod_route_t;

/* modulaation arrays and mutex */
extern mod_source_t g_mod_sources[MAX_MOD_SOURCES];
extern mod_route_t g_mod_routes[MAX_MOD_ROUTES];
extern pthread_mutex_t g_mod_mutex;

/* modulation functions in mod.c */
extern void process_modulation(ma_uint32 frame_count, ma_uint32 sample_rate);
extern install_t mod_register_predicates(void);
extern install_t uninstall_mod(void);

/* Shared arrays */
extern sound_slot_t g_sounds[MAX_SOUNDS];
extern synth_voice_t g_voices[MAX_VOICES];
extern synth_oscillator_t g_oscillators[MAX_OSCILLATORS];

/* Helper functions (implemented in promini.c) */
extern void get_engine_format_info(ma_format* format, ma_uint32* channels, ma_uint32* sampleRate);
extern void free_effect_chain(effect_node_t* effect);
extern data_slot_t* get_data_slot(int index);

/* Effect functions (implemented in effects.c) */
extern ma_result attach_effect_node(ma_node* source_node, effect_node_t** effect_chain, ma_node_base* effect_node, effect_type_t type);

/* Reverb functions (implemented in reverb.c) */
extern ma_node_vtable reverb_vtable;
extern ma_result init_reverb_node(reverb_node_t* reverb, ma_uint32 sample_rate);
extern void free_reverb_node(reverb_node_t* reverb);

/* Module registration functions */
extern install_t promini_register_predicates(void);
extern install_t synth_register_predicates(void);
extern install_t effects_register_predicates(void);

/* Module cleanup functions */
extern install_t uninstall_promini(void);
extern install_t uninstall_synth(void);
extern install_t uninstall_effects(void);

#endif /* PROMINI_H */
