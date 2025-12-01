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

% Envelope tests

test(create_envelope, [nondet, cleanup(mod_source_unload(E))]) :-
    mod_envelope_create(0.1, 0.2, 0.3, 0.5, 0.4, 1000.0, false, E),
    integer(E),
    E >= 0.

test(create_envelope_looping, [nondet, cleanup(mod_source_unload(E))]) :-
    mod_envelope_create(0.1, 0.1, 0.6, 0.7, 0.2, 500.0, true, E),
    integer(E),
    E >= 0.

test(envelope_trigger, [nondet, cleanup(mod_source_unload(E))]) :-
    mod_envelope_create(0.2, 0.3, 0.3, 0.5, 0.2, 200.0, false, E),
    mod_envelope_trigger(E).

test(envelope_route_to_oscillator, [nondet, cleanup((
    mod_route_unload(R),
    mod_source_unload(E),
    synth_voice_unload(V)
))]) :-
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.5, O),
    mod_envelope_create(0.1, 0.2, 0.5, 0.7, 0.2, 100.0, false, E),
    mod_route_create(E, oscillator, O, frequency, 100.0, 440.0, 0.0, R),
    integer(R),
    R >= 0.

:- end_tests(mod).
