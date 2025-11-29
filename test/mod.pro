:- module(test_mod, []).
:- use_module('src/prolog/sampler.pro').
:- use_module(library(plunit)).

:- begin_tests(mod).

% LFO tests

test(create_lfo_sine, [nondet, cleanup(sampler_mod_source_unload(L))]) :-
    sampler_mod_lfo_create(sine, 1.0, L),
    integer(L),
    L >= 0.

test(create_lfo_square, [nondet, cleanup(sampler_mod_source_unload(L))]) :-
    sampler_mod_lfo_create(square, 2.0, L),
    integer(L),
    L >= 0.

test(create_lfo_triangle, [nondet, cleanup(sampler_mod_source_unload(L))]) :-
    sampler_mod_lfo_create(triangle, 0.5, L),
    integer(L),
    L >= 0.

test(create_lfo_sawtooth, [nondet, cleanup(sampler_mod_source_unload(L))]) :-
    sampler_mod_lfo_create(sawtooth, 4.0, L),
    integer(L),
    L >= 0.

test(lfo_get_frequency, [nondet, cleanup(sampler_mod_source_unload(L))]) :-
    sampler_mod_lfo_create(sine, 5.0, L),
    sampler_mod_lfo_get_frequency(L, Freq),
    Freq =:= 5.0.

test(lfo_set_frequency, [nondet, cleanup(sampler_mod_source_unload(L))]) :-
    sampler_mod_lfo_create(sine, 1.0, L),
    sampler_mod_lfo_set_frequency(L, 10.0),
    sampler_mod_lfo_get_frequency(L, Freq),
    Freq =:= 10.0.

test(lfo_unload, [nondet]) :-
    sampler_mod_lfo_create(sine, 1.0, L),
    sampler_mod_source_unload(L).

% Route tests

test(create_route, [nondet, cleanup((
    sampler_mod_route_unload(R),
    sampler_mod_source_unload(L),
    sampler_synth_voice_unload(V)
))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 440.0, 0.0, O),
    sampler_mod_lfo_create(sine, 1.0, L),
    sampler_mod_route_create(L, oscillator, O, frequency, 100.0, 440.0, 0.0, R),
    integer(R),
    R >= 0.

test(route_unload, [nondet, cleanup((
    sampler_mod_source_unload(L),
    sampler_synth_voice_unload(V)
))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 440.0, 0.0, O),
    sampler_mod_lfo_create(sine, 1.0, L),
    sampler_mod_route_create(L, oscillator, O, frequency, 100.0, 440.0, 0.0, R),
    sampler_mod_route_unload(R).

test(source_unload_removes_routes, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 440.0, 0.0, O),
    sampler_mod_lfo_create(sine, 1.0, L),
    sampler_mod_route_create(L, oscillator, O, frequency, 100.0, 440.0, 0.0, _R),
    sampler_mod_source_unload(L).

:- end_tests(mod).
