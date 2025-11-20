:- module(test_sampler, []).
:- use_module('src/prolog/sampler.pro').
:- use_module(library(plunit)).

:- begin_tests(sampler_init).

test(version, [nondet]) :-
    sampler_version(Version),
    atom(Version).

test(init) :-
    sampler_init.

test(devices, [nondet]) :-
    sampler_devices(Devices),
    is_list(Devices).

:- end_tests(sampler_init).

:- begin_tests(sampler_data).

test(data_load, [nondet]) :-
    sampler_data_load('audio/counting.wav', Handle),
    integer(Handle),
    sampler_data_unload(Handle).

test(data_info, [nondet]) :-
    sampler_data_load('audio/counting.wav', Handle),
    sampler_data_info(Handle, data_info(Frames, Channels, SampleRate, Duration)),
    integer(Frames),
    Frames > 0,
    integer(Channels),
    Channels > 0,
    integer(SampleRate),
    SampleRate > 0,
    number(Duration),
    Duration > 0,
    sampler_data_unload(Handle).

test(data_reverse, [nondet]) :-
    sampler_data_load('audio/counting.wav', Handle),
    sampler_data_reverse(Handle, Reversed),
    integer(Reversed),
    sampler_data_unload(Handle),
    sampler_data_unload(Reversed).

test(data_load_reversed, [nondet]) :-
    sampler_data_load_reversed('audio/counting.wav', Handle),
    integer(Handle),
    sampler_data_unload(Handle).

test(data_extract, [nondet]) :-
    sampler_data_load('audio/counting.wav', Handle),
    sampler_data_extract(Handle, 0, 1000, Extracted),
    integer(Extracted),
    sampler_data_unload(Handle),
    sampler_data_unload(Extracted).

:- end_tests(sampler_data).

:- begin_tests(sampler_sound).

test(sound_load, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    integer(Sound),
    sampler_sound_unload(Sound).

test(sound_create_from_data, [nondet]) :-
    sampler_data_load('audio/counting.wav', Data),
    sampler_sound_create(Data, Sound),
    integer(Sound),
    sampler_sound_unload(Sound),
    sampler_data_unload(Data).

test(sound_length, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_length(Sound, Length),
    integer(Length),
    Length > 0,
    sampler_sound_unload(Sound).

:- end_tests(sampler_sound).

:- begin_tests(sampler_playback).

test(sound_start_stop, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_start(Sound),
    sampler_sound_is_playing(Sound),
    sleep(0.1),
    sampler_sound_stop(Sound),
    \+ sampler_sound_is_playing(Sound),
    sampler_sound_unload(Sound).

test(sound_looping, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    \+ sampler_sound_is_looping(Sound),
    sampler_sound_loop(Sound),
    sampler_sound_is_looping(Sound),
    sampler_sound_no_loop(Sound),
    \+ sampler_sound_is_looping(Sound),
    sampler_sound_unload(Sound).

test(sound_seek, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_seek(Sound, 1000),
    sampler_sound_get_position(Sound, Pos),
    Pos =:= 1000,
    sampler_sound_unload(Sound).

test(sound_start_at, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_start_at(Sound, 500),
    sampler_sound_is_playing(Sound),
    sampler_sound_get_position(Sound, Pos),
    Pos >= 500,
    sampler_sound_stop(Sound),
    sampler_sound_unload(Sound).

:- end_tests(sampler_playback).

:- begin_tests(sampler_parameters).

test(sound_pitch, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_set_pitch(Sound, 12.0),
    sampler_sound_get_pitch(Sound, Pitch),
    Pitch =:= 12.0,
    sampler_sound_unload(Sound).

test(sound_pan, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_set_pan(Sound, -0.5),
    sampler_sound_get_pan(Sound, Pan),
    Pan =:= -0.5,
    sampler_sound_unload(Sound).

test(sound_pan_mode, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_get_pan_mode(Sound, balance),
    sampler_sound_set_pan_mode(Sound, pan),
    sampler_sound_get_pan_mode(Sound, pan),
    sampler_sound_unload(Sound).

test(sound_volume, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_set_volume(Sound, 0.5),
    sampler_sound_get_volume(Sound, Volume),
    Volume =:= 0.5,
    sampler_sound_unload(Sound).

:- end_tests(sampler_parameters).

:- begin_tests(sampler_range).

test(sound_set_range, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_set_range(Sound, 100, 1000),
    sampler_sound_unload(Sound).

:- end_tests(sampler_range).

:- begin_tests(sampler_effects).

test(attach_bitcrush, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_bitcrush(Sound, 8, 8000),
    sampler_sound_unload(Sound).

test(attach_effect_generic, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, bitcrush, [bits=4, sample_rate=4000]),
    sampler_sound_unload(Sound).

:- end_tests(sampler_effects).

:- begin_tests(sampler_polyphony).

test(multiple_sounds, [nondet]) :-
    sampler_data_load('audio/gong.wav', Data),
    sampler_sound_create(Data, Sound1),
    sampler_sound_create(Data, Sound2),
    sampler_sound_create(Data, Sound3),
    sampler_sound_start(Sound1),
    sampler_sound_start(Sound2),
    sampler_sound_start(Sound3),
    sampler_sound_is_playing(Sound1),
    sampler_sound_is_playing(Sound2),
    sampler_sound_is_playing(Sound3),
    sleep(0.1),
    sampler_sound_stop(Sound1),
    sampler_sound_stop(Sound2),
    sampler_sound_stop(Sound3),
    sampler_sound_unload(Sound1),
    sampler_sound_unload(Sound2),
    sampler_sound_unload(Sound3),
    sampler_data_unload(Data).

:- end_tests(sampler_polyphony).
