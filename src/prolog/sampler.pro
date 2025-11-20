/*
 * sampler.pro - Prolog interface to miniaudio (engine API)
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

:- module(sampler, [
    sampler_version/1,
    sampler_init/0,
    sampler_devices/1,
    sampler_sound_load/2,
    sampler_sound_unload/1,
    sampler_sound_start/1,
    sampler_sound_stop/1,
    sampler_sound_is_playing/1,
    sampler_sound_loop/1,
    sampler_sound_no_loop/1,
    sampler_sound_is_looping/1,
    sampler_data_load/2,
    sampler_data_unload/1,
    sampler_sound_create/2,
    sampler_sound_seek/2,
    sampler_sound_get_position/2,
    sampler_data_info/2,
    sampler_sound_length/2,
    sampler_sound_start_at/2,
    sampler_sound_set_pitch/2,
    sampler_sound_get_pitch/2,
    sampler_sound_set_pan/2,
    sampler_sound_get_pan/2,
    sampler_sound_set_pan_mode/2,
    sampler_sound_get_pan_mode/2,
    sampler_sound_set_volume/2,
    sampler_sound_get_volume/2,
    sampler_data_reverse/2,
    sampler_data_load_reversed/2,
    sampler_sound_set_range/3,
    sampler_data_extract/4,
    sampler_sound_attach_effect/3,
    sampler_sound_attach_bitcrush/3
  ]).

:- use_foreign_library('../../lib/sampler').

:- initialization(register_cleanup).

register_cleanup :-
    at_halt(unload_foreign_library('../../lib/sampler')).

sampler_sound_loop(Handle) :-
    sampler_sound_set_looping(Handle, true).

sampler_sound_no_loop(Handle) :-
    sampler_sound_set_looping(Handle, false).

sampler_sound_start_at(Handle, Frame) :-
    sampler_sound_seek(Handle, Frame),
    sampler_sound_start(Handle).

sampler_data_load_reversed(Path, ReversedHandle) :-
    sampler_data_load(Path, TempHandle),
    sampler_data_reverse(TempHandle, ReversedHandle),
    sampler_data_unload(TempHandle).

sampler_sound_load(Path, SoundHandle) :-
    sampler_data_load(Path, DataHandle),
    sampler_sound_create(DataHandle, SoundHandle),
    sampler_data_unload(DataHandle).

sampler_sound_attach_bitcrush(Sound, Bits, SampleRate) :-
    sampler_sound_attach_effect(Sound, bitcrush, [bits=Bits, sample_rate=SampleRate]).
