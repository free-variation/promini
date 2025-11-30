:- module(test_mod, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(mod).

% LFO tests

test(create_lfo_sine, [nondet, cleanup(mod_source_unload(L))]) :-
    mod_lfo_create(sine, 1.0, L),
    integer(L),
    L >= 0.

test(create_lfo_square, [nondet, cleanup(mod_source_unload(L))]) :-
    mod_lfo_create(square, 2.0, L),
    integer(L),
    L >= 0.

test(create_lfo_triangle, [nondet, cleanup(mod_source_unload(L))]) :-
    mod_lfo_create(triangle, 0.5, L),
    integer(L),
    L >= 0.

test(create_lfo_sawtooth, [nondet, cleanup(mod_source_unload(L))]) :-
    mod_lfo_create(sawtooth, 4.0, L),
    integer(L),
    L >= 0.

test(lfo_get_frequency, [nondet, cleanup(mod_source_unload(L))]) :-
    mod_lfo_create(sine, 5.0, L),
    mod_lfo_get_frequency(L, Freq),
    Freq =:= 5.0.

test(lfo_set_frequency, [nondet, cleanup(mod_source_unload(L))]) :-
    mod_lfo_create(sine, 1.0, L),
    mod_lfo_set_frequency(L, 10.0),
    mod_lfo_get_frequency(L, Freq),
    Freq =:= 10.0.

test(lfo_unload, [nondet]) :-
    mod_lfo_create(sine, 1.0, L),
    mod_source_unload(L).

% Route tests

test(create_route, [nondet, cleanup((
    mod_route_unload(R),
    mod_source_unload(L),
    synth_voice_unload(V)
))]) :-
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    mod_lfo_create(sine, 1.0, L),
    mod_route_create(L, oscillator, O, frequency, 100.0, 440.0, 0.0, R),
    integer(R),
    R >= 0.

test(route_unload, [nondet, cleanup((
    mod_source_unload(L),
    synth_voice_unload(V)
))]) :-
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    mod_lfo_create(sine, 1.0, L),
    mod_route_create(L, oscillator, O, frequency, 100.0, 440.0, 0.0, R),
    mod_route_unload(R).

test(source_unload_removes_routes, [nondet, cleanup(synth_voice_unload(V))]) :-
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    mod_lfo_create(sine, 1.0, L),
    mod_route_create(L, oscillator, O, frequency, 100.0, 440.0, 0.0, _R),
    mod_source_unload(L).

:- end_tests(mod).
