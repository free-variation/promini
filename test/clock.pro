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

test(clock_stop_resets_position, [nondet]) :-
    clock_start,
    sleep(0.5),
    clock_stop,
    clock_get_beat_position(Pos),
    Pos < 0.1.

test(clock_get_beat_position, [nondet]) :-
    clock_get_beat_position(Pos),
    number(Pos).

test(clock_beat_position_advances, [nondet]) :-
    clock_start,
    clock_get_beat_position(Pos1),
    sleep(0.5),
    clock_get_beat_position(Pos2),
    Pos2 > Pos1.

test(clock_route_init_lfo_pulse, [nondet]) :-
    mod_lfo_init(sine, 1.0, LFO),
    clock_route_init(lfo, LFO, pulse, 24, Route),
    clock_route_uninit(Route),
    mod_source_uninit(LFO).

test(clock_route_init_lfo_sync, [nondet]) :-
    mod_lfo_init(sine, 1.0, LFO),
    clock_route_init(lfo, LFO, sync, 24, Route),
    clock_route_uninit(Route),
    mod_source_uninit(LFO).

test(clock_route_init_envelope, [nondet]) :-
    mod_envelope_init(0.1, 0.2, 0.3, 0.4, 0.5, 1000.0, false, Env),
    clock_route_init(envelope, Env, pulse, 24, Route),
    clock_route_uninit(Route),
    mod_source_uninit(Env).

test(clock_route_init_granular, [nondet]) :-
    granular_init(1.0, G),
    clock_route_init(granular, G, pulse, 24, Route),
    clock_route_uninit(Route),
    granular_uninit(G).

test(clock_route_uninit_invalid, [error(type_error(clock_route, _))]) :-
    clock_route_uninit(invalid).

test(clock_route_init_invalid_target_type, [error(domain_error(target_type, _))]) :-
    mod_lfo_init(sine, 1.0, LFO),
    clock_route_init(invalid, LFO, pulse, 24, _),
    mod_source_uninit(LFO).

test(clock_route_init_invalid_route_type, [error(domain_error(route_type, _))]) :-
    mod_lfo_init(sine, 1.0, LFO),
    clock_route_init(lfo, LFO, invalid, 24, _),
    mod_source_uninit(LFO).

:- end_tests(clock).
