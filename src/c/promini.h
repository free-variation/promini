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
#include <SDL3/SDL.h>

/*
 * Shared global engine
 * Initialized by promini.c, shared with other modules
 */
extern ma_engine *g_engine;

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
 * Utility macro to clamp a value to a range
 */
#define CLAMP(val, min, max) ((val) < (min) ? (min) : ((val) > (max) ? (max) : (val)))

/*
 * One-pole filter for exponential smoothing
 */
#define ONE_POLE(out, in, coeff) ((out) += (coeff) * ((in) - (out)))

/*
 * Semitones to pitch ratio
 */
#define SEMITONES_TO_RATIO(semi) (powf(2.0f, (semi) / 12.0f))

/*
 * Thread safety - mutexes for protecting global state
 */
extern pthread_mutex_t g_sounds_mutex;
extern pthread_mutex_t g_audio_buffers_mutex;
extern pthread_mutex_t g_capture_devices_mutex;
extern pthread_mutex_t g_voices_mutex;
extern pthread_mutex_t g_summing_mutex;
extern pthread_mutex_t g_mod_mutex;
extern pthread_mutex_t g_images_mutex;
extern pthread_mutex_t g_image_synths_mutex;
extern pthread_mutex_t g_granular_mutex;
extern pthread_mutex_t g_visualizers_mutex;

/*
 * Sound management
 */
#define MAX_SOUNDS 1024
#define MAX_AUDIO_BUFFERS 256

typedef struct {
	ma_audio_buffer *buffer;
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
	EFFECT_PAN,
	EFFECT_VCA,
	EFFECT_MOOG,
	EFFECT_COMPRESSOR
} effect_type_t;

/* Effect chain node */
typedef struct effect_node {
	effect_type_t type;
	ma_node_base *effect_node;
	struct effect_node *next;
} effect_node_t;

/* Sound slot */
typedef struct {
	ma_sound *sound;
	ma_audio_buffer *audio_buffer;
	int audio_buffer_index;
	ma_bool32 in_use;
	effect_node_t *effect_chain;
} sound_slot_t;

typedef struct {
	float *samples;
	ma_uint64 capacity_frames;
	ma_uint64 write_pos;
	ma_uint32 channels;
	ma_format format;
} ring_buffer_t;

/* Capture device slot */
#define MAX_CAPTURE_DEVICES 8

typedef struct {
	ma_node_base node;
	ma_device device;
	ring_buffer_t buffer;
	ma_bool32 in_use;
} capture_slot_t;

extern capture_slot_t g_capture_devices[MAX_CAPTURE_DEVICES];

/*
 * Synth voice management
 */
#define MAX_VOICES 256

/* Synth voice slot */
typedef struct {
	ma_sound_group group;
	ma_bool32 in_use;
	ma_bool32 is_voice;
	effect_node_t *effect_chain;
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
        if (!get_typed_handle(handle_term, "sound", &slot_var)) { \
            return PL_type_error("sound", handle_term); \
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
	float *hold_samples;
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
	float *buffer_l;
	float *buffer_r;
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
	float *buffer;
	ma_uint32 mask;
	ma_uint32 main_delay;
	ma_uint32 tap1, tap2, tap3;
} reverb_delay_line_t;

/* Tank half for reverb (one side of figure-8) */
typedef struct {
	float *decay_diff1_buf;
	ma_uint32 decay_diff1_mask;
	ma_uint32 decay_diff1_delay;
	float decay_diff1_out;

	reverb_delay_line_t pre_damp;

	float damping_state;

	float *decay_diff2_buf;
	ma_uint32 decay_diff2_mask;
	ma_uint32 decay_diff2_delay;
	ma_uint32 decay_diff2_tap1;  /* output tap offset */
	ma_uint32 decay_diff2_tap2;  /* output tap offset */

	reverb_delay_line_t post_damp;
} reverb_tank_half_t;

/* Complete reverb channel (L or R) */
typedef struct {
	float *predelay_buf;
	ma_uint32 predelay_mask;

	float input_lpf_state;

	float *diffuser_buf[4];
	ma_uint32 diffuser_mask[4];
	ma_uint32 diffuser_delay[4];

	reverb_tank_half_t tank[2];
} reverb_channel_t;

/* Pitch shifter for shimmer effect (4-grain overlap-add) */
typedef struct {
	float *buffer;
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

/* VCA effect node */
typedef struct {
	ma_node_base base;
	float current_gain;
	float target_gain;
} vca_node_t;

/* Summing node */
#define MAX_SUMMING_NODES 64

typedef struct {
	ma_node_base base;
	effect_node_t *effect_chain;
	ma_bool32 in_use;
} summing_node_t;

extern summing_node_t g_summing_nodes[MAX_SUMMING_NODES];

/* Moog ladder filter node */
typedef struct {
	ma_node_base base;
	
	double v[4][2];
	double dv[4][2];
	double tv[4][2];
	float prev_input[2]; /* previous input sample per channel for interpolation */

	float current_cutoff;
	float target_cutoff;
	float current_resonance;
	float target_resonance;
	float drive;
} moog_node_t;

#define AMP_TO_DB(amp) (20.0f * log10f((amp) + 1e-10f))
#define DB_TO_AMP(db)  (powf(10.0f, (db) / 20.0f))

/* Compressor effect node (with look-ahead) */
typedef struct {
	ma_node_base base;
	float threshold;		/* level where compression starts (0.0 - 1.0) */
	float ratio;			/* compression ratio (1.0 = none, INFINITY = limiter) */
	float knee;				/* soft knee width in dB (0.0 = hard knee) */
	float attack_coeff;		/* envelope follower attack coefficient */
	float release_coeff;	/* envelope follower release coefficient */
	float makeup_gain;		/* output gain compensation */
	float envelope;			/* current envelope level */
	float *delay_buffer;	/* look-ahead delay buffer */
	ma_uint32 delay_frames;	/* look-ahead delay in frames */
	ma_uint32 delay_pos;	/* current write position in delay buffer */
} compressor_node_t;

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
	MOD_SOURCE_ENVELOPE,
	MOD_SOURCE_GAMEPAD,
	MOD_SOURCE_KEYBOARD
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
		struct {
			SDL_Gamepad *gamepad;
			SDL_GamepadAxis axis;
			float dead_zone;
			int dpad_axis;  /* 0=normal, 1=dpad_x, 2=dpad_y */
		} gamepad;
		struct {
			SDL_Scancode scancode;
			float attack_ms;
			float release_ms;
			float value;
			ma_bool32 invert;
		} keyboard;
	} source;
	ma_bool32 sh_enabled;
	ma_uint32 sh_interval;
	ma_uint32 sh_counter;
	float sh_held_value;
	float current_value;
} mod_source_t;

/* forward declare for setter */
typedef struct mod_route mod_route_t;

/* modulation setter function */
typedef void (*mod_setter_t)(void *target, float value, ma_uint32 frame_count, mod_route_t *route);

/* modulation route */
struct mod_route {
	ma_bool32 in_use;
	int source_slot;
	void *target;
	mod_setter_t setter;
	float depth;
	float offset;
	float slew;
	float current_value;
	ma_bool32 rate_mode;
};

/* master clock */

#define CLOCK_DEFAULT_BPM 128
#define CLOCK_PPQN 24
#define MAX_CLOCK_ROUTES 256

typedef struct {
	ma_sound sound;
	float bpm;
	ma_bool32 running;
	double last_time;
} promini_clock_t;

typedef enum {
	CLOCK_TARGET_LFO,
	CLOCK_TARGET_ENVELOPE,
	CLOCK_TARGET_GRANULAR,
	CLOCK_TARGET_PING_PONG_DELAY,
	CLOCK_TARGET_DELAY
} clock_target_type_t;

typedef enum {
	CLOCK_ROUTE_PULSE,
	CLOCK_ROUTE_SYNC
} clock_route_type_t;

typedef struct {
	ma_bool32 in_use;
	clock_route_type_t route_type;
	void *target_slot;
	clock_target_type_t target_type;
	float division;
	float phase_offset;
} clock_route_t;

/* modulation arrays */
extern mod_source_t g_mod_sources[MAX_MOD_SOURCES];
extern mod_route_t g_mod_routes[MAX_MOD_ROUTES];

extern promini_clock_t g_clock;
extern clock_route_t g_clock_routes[MAX_CLOCK_ROUTES];

/* modulation functions in mod.c */
extern void process_modulation(ma_uint32 frame_count, ma_uint32 sample_rate);
extern install_t mod_register_predicates(void);
extern install_t uninstall_mod(void);

extern void clock_init(ma_uint32 sample_rate);
extern void clock_uninit(void);
extern install_t clock_register_predicates(void);
extern install_t uninstall_clock(void);
extern void update_clock_routes(clock_route_type_t);

/* image slot type */

#define MAX_IMAGES 64

typedef struct {
	unsigned char *pixels;	/* original image data */
	unsigned char *buffer;	/* working buffer (may be smaller) */
	int width;
	int height;
	int buf_width;			/* buffer dimensions (time steps) */
	int buf_height;			/* buffer dimensions (oscillators) */
	int channels;
	ma_bool32 in_use;
} image_slot_t;

/* Image synth node */
# define MAX_IMAGE_SYNTHS 64
# define MAX_IMAGE_SYNTH_ROWS 128

typedef enum {
	IMAGE_SYNTH_ADDITIVE,
	IMAGE_SYNTH_WAVEFORM
} image_synth_mode_t;

typedef struct {
	ma_node_base base;
	ma_bool32 in_use;
	effect_node_t *effect_chain;

	int image_slot;
	int channel;
	ma_bool32 playing;
	image_synth_mode_t mode;

	union {
		struct {
			float bpm;
			float beats_per_scan;
			ma_bool32 looping;
			float scan_position;
			float phases[MAX_IMAGE_SYNTH_ROWS];
			float frequencies[MAX_IMAGE_SYNTH_ROWS];
			float amplitudes[MAX_IMAGE_SYNTH_ROWS];
		} additive;
		struct {
			float frequency;
			float amplitude;
			float phase;
			int row;
		} waveform;
	} params;
} image_synth_node_t;

/*
 * Granular delay
 */
#define MAX_GRANULAR_DELAYS 16
#define MAX_GRAINS 128
#define MAX_MODE_INTERVAL 24


typedef struct {
	ma_uint64 position;
	ma_uint64 size;
	float cursor;
	float pitch_ratio;
	float envelope;
	float pan;
	float envelope_phase;
	int direction;
	ma_bool32 active;
} grain_t;

typedef struct {
	ma_node_base base;
	ma_bool32 in_use;
	effect_node_t *effect_chain;

	ring_buffer_t buffer;
	ma_bool32 recording;

	grain_t grains[MAX_GRAINS];

	float density;
	float regularity;
	float position;
	float position_spray;
	float size_ms;
	float size_spray;
	float reverse_probability;
	float pitch;
	float envelope;
	float pan;
	float pan_spray;

	float mode[MAX_MODE_INTERVAL];
	int mode_length;
	int deviation_up;
	int deviation_down;

	float density_accumulator;
	float num_grains_smoothed;
	float gain_normalization;
	ma_bool32 normalize;
	ma_uint64 frames_recorded; 
} granular_delay_t;

/* keyboard state */

#define KEYBOARD_ROWS 4
#define KEYBOARD_KEYS_PER_ROW 10
#define MAX_MODE_INTERVALS 12

typedef enum {
	KEYBOARD_TARGET_NONE,
	KEYBOARD_TARGET_GRANULAR
} keyboard_target_type_t;

typedef struct {
	float mode[MAX_MODE_INTERVALS];
	int mode_length;
	int octave_offset;
	keyboard_target_type_t target_type;
	int granular_slot;
} keyboard_row_t;

typedef struct {
	ma_bool32 active;
	SDL_Window *window;
	SDL_Renderer *renderer;
	ma_bool32 keys_pressed[KEYBOARD_ROWS][KEYBOARD_KEYS_PER_ROW];
	keyboard_row_t rows[KEYBOARD_ROWS];
	ma_bool32 mod_keys[5];  /* l_shift, l_option, space, r_option, r_shift */
} keyboard_state_t;

extern keyboard_state_t g_keyboard;

/* Visualizer types */
typedef enum {
	VIZ_MODE_WAVEFORM,
	VIZ_MODE_SPECTRUM,
	VIZ_MODE_SPECTROGRAM
} viz_mode_t;

#define MAX_VISUALIZERS 16
#define VIZ_BUFFER_FRAMES 8192
#define VIZ_WATERFALL_ROWS 256
#define VIZ_TRIGGER_SEARCH_FRAMES 1024

typedef struct {
	float *real;
	float *imag;
	float *magnitudes;
	float *window;
	ma_uint32 fft_size;
} fft_state_t;

typedef struct {
	ma_node_base base;
	ma_bool32 in_use;

	/* audio tap - audio thread writes, render thread reads */
	ring_buffer_t buffer;

	/* FFT state */
	fft_state_t fft;
	float *smoothed_magnitudes[2];

	/* waterfall history (per channel) */
	float *waterfall[2];
	ma_uint32 waterfall_row;

	/* display */
	viz_mode_t mode;
	ma_bool32 triggered;
	SDL_Window *window;
	SDL_Renderer *renderer;

	/* display parameters */
	float zoom;
	float amp_scale;
	float smoothing;
	float auto_max_bin;
	ma_bool32 log_freq;
	ma_bool32 db_scale;
	ma_bool32 paused;
	ma_uint32 theme;
} visualizer_node_t;

extern visualizer_node_t g_visualizers[MAX_VISUALIZERS];

/* Shared arrays */
extern sound_slot_t g_sounds[MAX_SOUNDS];
extern synth_voice_t g_voices[MAX_VOICES];
extern synth_oscillator_t g_oscillators[MAX_OSCILLATORS];
extern image_slot_t g_images[MAX_IMAGES];
extern image_synth_node_t g_image_synths[MAX_IMAGE_SYNTHS];
extern granular_delay_t g_granular_delays[MAX_GRANULAR_DELAYS];
extern visualizer_node_t g_visualizers[MAX_VISUALIZERS];

/* Helper functions (implemented in promini.c) */
extern void get_engine_format_info(ma_format *format, ma_uint32 *channels, ma_uint32 *sampleRate);
extern void free_effect_chain(effect_node_t *effect);
extern data_slot_t *get_data_slot(int index);
extern ma_bool32 get_source_from_term(term_t source_term, ma_node** source_node, effect_node_t** chain);
extern float audio_buffer_read_interpolated(data_slot_t *slot, float position, ma_uint32 channel, ma_uint64 size_in_frames, ma_uint32 channels);

/* Typed handle helpers (implemented in promini.c) */
extern int unify_typed_handle(term_t term, const char *type, int slot);
extern int get_typed_handle(term_t term, const char *type, int *slot);

/* Parameter parsing helpers for key=value lists (implemented in promini.c) */
extern ma_bool32 get_param_int(term_t params, const char *key, int *value);
extern ma_bool32 get_param_float(term_t params, const char *key, float *value);
extern ma_bool32 get_param_bool(term_t params, const char *key, ma_bool32 *value);
extern ma_bool32 get_param_double(term_t params, const char *key, double *value);
extern int get_param_float_list(term_t params, const char *key, float *out, int max_len);

/* Ring buffer */
extern ma_result ring_buffer_init(ring_buffer_t *rb, ma_uint64 capacity_frames, ma_uint32 channels, ma_format format);
extern void ring_buffer_free(ring_buffer_t *rb);
extern void ring_buffer_write(ring_buffer_t *rb, const void *input, ma_uint32 frame_count);
extern void ring_buffer_read(ring_buffer_t *rb, void *output, ma_uint64 delay_frames, ma_uint32 frame_count);
extern float ring_buffer_read_interpolated(ring_buffer_t *rb, float position, ma_uint32 channel);

/* Effect functions (implemented in effects.c) */
extern ma_node *get_effect_chain_tail(effect_node_t *chain);
extern ma_result attach_effect_node(ma_node *source_node, effect_node_t** effect_chain, ma_node_base *effect_node, effect_type_t type);
extern void *get_effect_pointer(term_t term);

/* Reverb functions (implemented in reverb.c) */
extern ma_node_vtable reverb_vtable;
extern ma_result init_reverb_node(reverb_node_t *reverb, ma_uint32 sample_rate);
extern void free_reverb_node(reverb_node_t *reverb);

/*  Gamepad and keyboard functions (implemented in control.c) */
extern int get_axis_from_atom(atom_t atom_term, SDL_GamepadAxis *axis);
extern SDL_Gamepad *get_gamepad_ptr(term_t gamepad_term);
extern void process_keyboard_event(SDL_Event *event);

/* Capture functions (implemented in capture.c) */
extern capture_slot_t *get_capture_device(int index);

/* Audio buffer creation (implemented in promini.c) */
extern int create_audio_buffer_from_pcm(
		void *data, 
		ma_format format, 
		ma_uint32 channels, 
		ma_uint64 frame_count, 
		ma_uint32 sample_rate);

/* Grain triggering (implemented in granular.c) */
extern int trigger_grain(granular_delay_t *g);
extern int trigger_grain_pitched(granular_delay_t *g, float semitones);

/* Module registration functions */
extern install_t promini_register_predicates(void);
extern install_t synth_register_predicates(void);
extern install_t effects_register_predicates(void);
extern install_t mixer_register_predicates(void);
extern install_t image_register_predicates(void);
extern install_t control_register_predicates(void);
extern install_t granular_register_predicates(void);
extern install_t capture_register_predicates(void);
extern install_t keyboard_register_predicates(void);
extern install_t visualizer_register_predicates(void);

/* Event handlers and render functions */
extern void keyboard_handle_event(SDL_Event *event);
extern void visualizer_handle_event(SDL_Event *event);
extern void visualizer_render_all(void);

/* Module cleanup functions */
extern install_t uninstall_promini(void);
extern install_t uninstall_synth(void);
extern install_t uninstall_effects(void);
extern install_t uninstall_mixer(void);
extern install_t uninstall_image(void);
extern install_t uninstall_control(void);
extern install_t uninstall_granular(void);
extern install_t uninstall_capture(void);
extern install_t uninstall_keyboard(void);
extern install_t uninstall_visualizer(void);
#endif /* PROMINI_H */
