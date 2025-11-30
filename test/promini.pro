:- module(test_promini, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(promini_init).

test(version, [nondet]) :-
    promini_version(Version),
    atom(Version).

test(init) :-
    promini_init.

test(devices, [nondet]) :-
    promini_devices(Devices),
    is_list(Devices).

:- end_tests(promini_init).

:- begin_tests(promini_audio).

test(data_load, [nondet]) :-
    audio_load('audio/counting.wav', Handle),
    integer(Handle),
    audio_unload(Handle).

test(data_info, [nondet]) :-
    audio_load('audio/counting.wav', Handle),
    audio_info(Handle, data_info(Frames, Channels, SampleRate, Duration)),
    integer(Frames),
    Frames > 0,
    integer(Channels),
    Channels > 0,
    integer(SampleRate),
    SampleRate > 0,
    number(Duration),
    Duration > 0,
    audio_unload(Handle).

test(data_reverse, [nondet]) :-
    audio_load('audio/counting.wav', Handle),
    audio_reverse(Handle, Reversed),
    integer(Reversed),
    audio_unload(Handle),
    audio_unload(Reversed).

test(data_load_reversed, [nondet]) :-
    audio_load_reversed('audio/counting.wav', Handle),
    integer(Handle),
    audio_unload(Handle).

test(data_extract, [nondet]) :-
    audio_load('audio/counting.wav', Handle),
    audio_extract(Handle, 0, 1000, Extracted),
    integer(Extracted),
    audio_unload(Handle),
    audio_unload(Extracted).

:- end_tests(promini_audio).

:- begin_tests(promini_sound).

test(sound_load, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    integer(Sound),
    sound_unload(Sound).

test(sound_create_from_data, [nondet]) :-
    audio_load('audio/counting.wav', Data),
    sound_create(Data, Sound),
    integer(Sound),
    sound_unload(Sound),
    audio_unload(Data).

test(sound_length, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_length(Sound, Length),
    integer(Length),
    Length > 0,
    sound_unload(Sound).

:- end_tests(promini_sound).

:- begin_tests(promini_playback).

test(sound_start_stop, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_start(Sound),
    sound_is_playing(Sound),
    sleep(0.1),
    sound_stop(Sound),
    \+ sound_is_playing(Sound),
    sound_unload(Sound).

test(sound_looping, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    \+ sound_is_looping(Sound),
    sound_loop(Sound),
    sound_is_looping(Sound),
    sound_no_loop(Sound),
    \+ sound_is_looping(Sound),
    sound_unload(Sound).

test(sound_seek, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_seek(Sound, 1000),
    sound_get_position(Sound, Pos),
    Pos =:= 1000,
    sound_unload(Sound).

test(sound_start_at, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_start_at(Sound, 500),
    sound_is_playing(Sound),
    sound_get_position(Sound, Pos),
    Pos >= 500,
    sound_stop(Sound),
    sound_unload(Sound).

:- end_tests(promini_playback).

:- begin_tests(promini_parameters).

test(sound_pitch, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_set_pitch(Sound, 12.0),
    sound_get_pitch(Sound, Pitch),
    Pitch =:= 12.0,
    sound_unload(Sound).

test(sound_pan, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_set_pan(Sound, -0.5),
    sound_get_pan(Sound, Pan),
    Pan =:= -0.5,
    sound_unload(Sound).

test(sound_pan_mode, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_get_pan_mode(Sound, balance),
    sound_set_pan_mode(Sound, pan),
    sound_get_pan_mode(Sound, pan),
    sound_unload(Sound).

test(sound_volume, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_set_volume(Sound, 0.5),
    sound_get_volume(Sound, Volume),
    Volume =:= 0.5,
    sound_unload(Sound).

:- end_tests(promini_parameters).

:- begin_tests(promini_range).

test(sound_set_range, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_set_range(Sound, 100, 1000),
    sound_unload(Sound).

:- end_tests(promini_range).

:- begin_tests(promini_polyphony).

test(multiple_sounds, [nondet]) :-
    audio_load('audio/gong.wav', Data),
    sound_create(Data, Sound1),
    sound_create(Data, Sound2),
    sound_create(Data, Sound3),
    sound_start(Sound1),
    sound_start(Sound2),
    sound_start(Sound3),
    sound_is_playing(Sound1),
    sound_is_playing(Sound2),
    sound_is_playing(Sound3),
    sleep(0.1),
    sound_stop(Sound1),
    sound_stop(Sound2),
    sound_stop(Sound3),
    sound_unload(Sound1),
    sound_unload(Sound2),
    sound_unload(Sound3),
    audio_unload(Data).

:- end_tests(promini_polyphony).

:- begin_tests(promini_capture).

test(capture_start_stop, [nondet]) :-
    promini_devices(Devices),
    member(device(Name, capture, _), Devices),
    !,
    capture_start(Name, 1.0, Capture, BufferFrames),
    integer(Capture),
    integer(BufferFrames),
    BufferFrames > 0,
    capture_stop(Capture).

test(capture_get_info, [nondet]) :-
    promini_devices(Devices),
    member(device(Name, capture, _), Devices),
    !,
    capture_start(Name, 0.5, Capture, _),
    capture_get_info(Capture, capture_info(WritePos, Capacity, SampleRate)),
    integer(WritePos),
    integer(Capacity),
    integer(SampleRate),
    WritePos >= 0,
    Capacity > 0,
    SampleRate > 0,
    capture_stop(Capture).

test(capture_extract, [nondet]) :-
    promini_devices(Devices),
    member(device(Name, capture, _), Devices),
    !,
    capture_start(Name, 1.0, Capture, _),
    sleep(0.2),
    capture_get_info(Capture, capture_info(WritePos, Capacity, SampleRate)),
    Offset is -(SampleRate // 10),
    Length is SampleRate // 20,
    capture_extract(Capture, Offset, Length, Data),
    integer(Data),
    audio_info(Data, data_info(Frames, Channels, Rate, Duration)),
    Frames =:= Length,
    Channels > 0,
    Rate =:= SampleRate,
    Duration > 0,
    audio_unload(Data),
    capture_stop(Capture).

test(capture_extract_wraparound, [nondet]) :-
    promini_devices(Devices),
    member(device(Name, capture, _), Devices),
    !,
    capture_start(Name, 0.1, Capture, BufferFrames),
    sleep(0.2),
    Offset is -BufferFrames + 100,
    Length is 200,
    capture_extract(Capture, Offset, Length, Data),
    integer(Data),
    audio_unload(Data),
    capture_stop(Capture).

:- end_tests(promini_capture).
