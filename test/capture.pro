:- module(test_capture, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(capture).

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

:- end_tests(capture).
