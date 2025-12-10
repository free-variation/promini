/*
 * promini.pro - Prolog interface to miniaudio (engine API)
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

:- module(promini, [
    promini_version/1,
    promini_init/0,
    promini_devices/1,
    sound_load/2,
    sound_unload/1,
    sound_start/1,
    sound_stop/1,
    sound_is_playing/1,
    sound_set_looping/2,
    sound_loop/1,
    sound_no_loop/1,
    sound_is_looping/1,
    audio_load/2,
    audio_unload/1,
    sound_create/2,
    sound_seek/2,
    sound_get_position/2,
    audio_info/2,
    sound_length/2,
    sound_start_at/2,
    sound_set_pitch/2,
    sound_get_pitch/2,
    audio_reverse/2,
    audio_load_reversed/2,
    sound_set_range/3,
    audio_extract/4,
    sound_attach_effect/4,
    sound_attach_bitcrush/4,
    sound_attach_bpf/4,
    sound_attach_delay/5,
    sound_attach_ping_pong_delay/6,
    sound_attach_reverb/3,
    effects/2,
    effect_set_parameters/2,
    effect_detach/1,
    clear_effects/1,
    capture_start/4,
    capture_stop/1,
    capture_get_info/2,
    capture_extract/4,
    synth_voices_in_use/1,
    synth_voice_create/1,
    synth_voice_start/1,
    synth_voice_stop/1,
    synth_voice_unload/1,
    voice_attach_effect/4,
    synth_oscillator_add/4,
    synth_oscillator_remove/1,
    synth_noise_add/3,
    synth_oscillator_fade/3,
    synth_oscillator_set_volume/2,
    synth_oscillator_get_volume/2,
    synth_oscillator_set_frequency/2,
    synth_oscillator_get_frequency/2,
    synth_oscillator_set_phase/2,
    synth_oscillator_get_phase/2,
    mod_lfo_create/3,
    mod_lfo_set_frequency/2,
    mod_lfo_get_frequency/2,
    mod_envelope_create/8,
    mod_envelope_trigger/1,
    mod_source_unload/1,
    mod_route_create/9,
    mod_route_unload/1,
    mod_gamepad_create/3,
    summing_node_create/1,
    summing_node_unload/1,
    summing_node_connect/2,
    summing_node_disconnect/1,
    summing_node_attach_effect/4,
    image_load/2,
    image_properties/4,
    image_unload/1,
    image_write_png/2,
    image_to_grayscale/1,
    image_downsample/3,
    image_quantize/2,
    image_reset/1,
    image_buffer_properties/3,
    image_transpose/2,
    image_synth_create/4,
    image_synth_unload/1,
    image_synth_set_parameters/2,
    image_synth_get_parameters/2,
    image_synth_start/1,
    image_synth_stop/1,
    image_synth_attach_effect/4,
    control_init/0,
    control_shutdown/0,
    control_gamepads/1,
    control_open/2,
    control_close/1,
    control_pump/0,
    control_axis/3,
    granular_create/2,
    granular_destroy/1,
    granular_trigger/1,
    granular_set/2,
    granular_get/2,
    granular_set_mode/4,
    granular_connect/2,
    granular_attach_effect/4
  ]).

:- use_foreign_library('../../lib/promini').

:- initialization(register_cleanup).

register_cleanup :-
    at_halt(unload_foreign_library('../../lib/promini')).

sound_loop(Handle) :-
    sound_set_looping(Handle, true).

sound_no_loop(Handle) :-
    sound_set_looping(Handle, false).

sound_start_at(Handle, Frame) :-
    sound_seek(Handle, Frame),
    sound_start(Handle).

audio_load_reversed(Path, ReversedHandle) :-
    audio_load(Path, TempHandle),
    audio_reverse(TempHandle, ReversedHandle),
    audio_unload(TempHandle).

sound_load(Path, SoundHandle) :-
    audio_load(Path, DataHandle),
    sound_create(DataHandle, SoundHandle),
    audio_unload(DataHandle).

sound_attach_bitcrush(Sound, Bits, SampleRate, Effect) :-
    sound_attach_effect(Sound, bitcrush, [bits=Bits, sample_rate=SampleRate], Effect).

sound_attach_bpf(Sound, Cutoff, Order, Effect) :-
    sound_attach_effect(Sound, bpf, [cutoff=Cutoff, order=Order], Effect).

sound_attach_delay(Sound, DelayInFrames, Decay, Wet, Effect) :-
    sound_attach_effect(Sound, delay, [delay_in_frames=DelayInFrames, decay=Decay, wet=Wet], Effect).

sound_attach_ping_pong_delay(Sound, MaxDelayInFrames, DelayInFrames, Feedback, Wet, Effect) :-
    sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=MaxDelayInFrames, delay_in_frames=DelayInFrames, feedback=Feedback, wet=Wet], Effect).

sound_attach_reverb(Sound, Params, Effect) :-
    sound_attach_effect(Sound, reverb, Params, Effect).

clear_effects(Source) :-
    effects(Source, Effects),
    maplist(effect_to_handle, Effects, Handles),
    maplist(effect_detach, Handles).

effect_to_handle(effect(Source, _Type, Ptr, _Params), effect(Source, Ptr)).