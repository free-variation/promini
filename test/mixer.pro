:- module(test_mixer, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(mixer).

% Summing node creation/unload tests

test(summing_node_init, [nondet]) :-
    summing_node_init(N),
    N = summing_node(_),
    summing_node_uninit(N).

test(summing_node_uninit, [nondet]) :-
    summing_node_init(N),
    summing_node_uninit(N).

% Connect/disconnect tests

test(summing_node_connect_sound, [nondet]) :-
    summing_node_init(N),
    sound_load('audio/guitar.wav', S),
    summing_node_connect(N, S),
    sound_unload(S),
    summing_node_uninit(N).

test(summing_node_connect_voice, [nondet]) :-
    summing_node_init(N),
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.5, _O),
    summing_node_connect(N, V),
    synth_voice_uninit(V),
    summing_node_uninit(N).

test(summing_node_disconnect_sound, [nondet]) :-
    summing_node_init(N),
    sound_load('audio/guitar.wav', S),
    summing_node_connect(N, S),
    summing_node_disconnect(S),
    sound_unload(S),
    summing_node_uninit(N).

test(summing_node_disconnect_voice, [nondet]) :-
    summing_node_init(N),
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.5, _O),
    summing_node_connect(N, V),
    summing_node_disconnect(V),
    synth_voice_uninit(V),
    summing_node_uninit(N).

% Connect with effect chain

test(summing_node_connect_sound_with_effect, [nondet]) :-
    summing_node_init(N),
    sound_load('audio/guitar.wav', S),
    sound_attach_effect(S, vca, [gain=0.5], _Effect),
    summing_node_connect(N, S),
    sound_unload(S),
    summing_node_uninit(N).

test(summing_node_connect_voice_with_effect, [nondet]) :-
    summing_node_init(N),
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.5, _O),
    voice_attach_effect(V, vca, [gain=0.5], _Effect),
    summing_node_connect(N, V),
    synth_voice_uninit(V),
    summing_node_uninit(N).

% Multiple sources to one summing node

test(summing_node_multiple_sources, [nondet]) :-
    summing_node_init(N),
    sound_load('audio/guitar.wav', S1),
    sound_load('audio/counting.wav', S2),
    summing_node_connect(N, S1),
    summing_node_connect(N, S2),
    sound_unload(S1),
    sound_unload(S2),
    summing_node_uninit(N).

% Summing node attach effect tests

test(summing_node_attach_vca, [nondet]) :-
    summing_node_init(N),
    summing_node_attach_effect(N, vca, [gain=0.5], Effect),
    Effect = effect(N, _),
    summing_node_uninit(N).

test(summing_node_attach_lpf, [nondet]) :-
    summing_node_init(N),
    summing_node_attach_effect(N, lpf, [cutoff=1000.0], Effect),
    Effect = effect(N, _),
    summing_node_uninit(N).

test(summing_node_attach_reverb, [nondet]) :-
    summing_node_init(N),
    summing_node_attach_effect(N, reverb, [decay=0.8, wet=0.3], Effect),
    Effect = effect(N, _),
    summing_node_uninit(N).

test(summing_node_with_effect_and_sources, [nondet]) :-
    summing_node_init(N),
    sound_load('audio/guitar.wav', S),
    summing_node_connect(N, S),
    summing_node_attach_effect(N, vca, [gain=0.5], _Effect),
    sound_unload(S),
    summing_node_uninit(N).

test(summing_node_detach_effect, [nondet]) :-
    summing_node_init(N),
    summing_node_attach_effect(N, vca, [gain=0.5], Effect),
    effect_detach(Effect),
    summing_node_uninit(N).

test(summing_node_query_effects, [nondet]) :-
    summing_node_init(N),
    summing_node_attach_effect(N, vca, [gain=0.5], VcaEffect),
    summing_node_attach_effect(N, lpf, [cutoff=1000.0], _),
    effects(N, Effects1),
    length(Effects1, 2),
    member(effect(N, vca, _, [gain=0.5]), Effects1),
    member(effect(N, lpf, _, [cutoff=1000.0, order=2]), Effects1),
    effect_detach(VcaEffect),
    effects(N, Effects2),
    length(Effects2, 1),
    summing_node_uninit(N).

:- end_tests(mixer).
