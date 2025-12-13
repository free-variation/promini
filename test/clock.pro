:- module(test_clock, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(clock, [setup(promini_init)]).

test(clock_set_bpm_float, [nondet]) :-
    clock_set_bpm(120.0).

test(clock_set_bpm_integer, [nondet]) :-
    clock_set_bpm(90).

test(clock_set_bpm_fails_zero, [error(domain_error(positive_number, _))]) :-
    clock_set_bpm(0).

test(clock_set_bpm_fails_negative, [error(domain_error(positive_number, _))]) :-
    clock_set_bpm(-60).

test(clock_get_bpm, [nondet]) :-
    clock_set_bpm(120),
    clock_get_bpm(BPM),
    abs(BPM - 120.0) < 0.001.

test(clock_not_running_initially, [fail]) :-
    clock_stop,
    clock_is_running.

test(clock_start, [nondet]) :-
    clock_start,
    clock_is_running.

test(clock_stop, [fail]) :-
    clock_start,
    clock_stop,
    clock_is_running.

test(clock_get_beat_position, [nondet]) :-
    clock_get_beat_position(Pos),
    number(Pos).

test(clock_beat_position_advances, [nondet]) :-
    clock_start,
    clock_get_beat_position(Pos1),
    sleep(0.5),
    clock_get_beat_position(Pos2),
    Pos2 > Pos1.

:- end_tests(clock).
