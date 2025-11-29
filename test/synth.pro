:- module(test_synth, []).
:- use_module('src/prolog/sampler.pro').
:- use_module(library(plunit)).

:- begin_tests(synth).

% Voice tests

test(voices_in_use_initially_zero, [nondet]) :-
    sampler_synth_voices_in_use(Count),
    Count =:= 0.

test(create_voice, [nondet, cleanup(sampler_synth_voice_unload(Handle))]) :-
    sampler_synth_voice_create(Handle),
    integer(Handle),
    Handle >= 0.

test(voices_in_use_after_create, [nondet, cleanup(sampler_synth_voice_unload(Handle))]) :-
    sampler_synth_voices_in_use(Before),
    sampler_synth_voice_create(Handle),
    sampler_synth_voices_in_use(After),
    After =:= Before + 1.

test(default_pan_is_center, [nondet, cleanup(sampler_synth_voice_unload(Handle))]) :-
    sampler_synth_voice_create(Handle),
    sampler_synth_voice_get_pan(Handle, Pan),
    Pan =:= 0.0.

test(set_and_get_pan, [nondet, cleanup(sampler_synth_voice_unload(Handle))]) :-
    sampler_synth_voice_create(Handle),
    sampler_synth_voice_set_pan(Handle, -0.5),
    sampler_synth_voice_get_pan(Handle, Pan),
    Pan =:= -0.5.

% Oscillator tests

test(add_oscillator, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 440.0, 0.0, O),
    integer(O),
    O >= 0.

test(oscillator_get_frequency, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 440.0, 0.0, O),
    sampler_synth_oscillator_get_frequency(O, Freq),
    Freq =:= 440.0.

test(oscillator_set_frequency, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 440.0, 0.0, O),
    sampler_synth_oscillator_set_frequency(O, 880.0),
    sampler_synth_oscillator_get_frequency(O, Freq),
    Freq =:= 880.0.

test(oscillator_get_phase, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 440.0, 0.0, O),
    sampler_synth_oscillator_get_phase(O, Phase),
    Phase >= 0.0,
    Phase < 1.0.

test(oscillator_set_phase, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 440.0, 0.0, O),
    sampler_synth_oscillator_set_phase(O, 0.5),
    sampler_synth_oscillator_get_phase(O, Phase),
    abs(Phase - 0.5) < 0.01.

test(remove_oscillator, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 440.0, 0.0, O),
    sampler_synth_oscillator_remove(O).

test(multiple_oscillators, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 440.0, 0.0, O1),
    sampler_synth_oscillator_add(V, 880.0, 0.0, O2),
    sampler_synth_oscillator_add(V, 1320.0, 0.0, O3),
    sampler_synth_oscillator_get_frequency(O1, F1),
    sampler_synth_oscillator_get_frequency(O2, F2),
    sampler_synth_oscillator_get_frequency(O3, F3),
    F1 =:= 440.0,
    F2 =:= 880.0,
    F3 =:= 1320.0.

test(start_stop_voice_with_oscillator, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 440.0, 0.0, _),
    sampler_synth_voice_start(V),
    sampler_synth_voice_stop(V).

test(start_empty_voice_fails, [fail, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_voice_start(V).

% Oscillator volume tests

test(oscillator_default_volume, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 440.0, 0.0, O),
    sampler_synth_oscillator_get_volume(O, Vol),
    Vol =:= 1.0.

test(oscillator_set_volume, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 440.0, 0.0, O),
    sampler_synth_oscillator_set_volume(O, 0.5),
    sampler_synth_oscillator_get_volume(O, Vol),
    abs(Vol - 0.5) < 0.001.

% Noise tests

test(add_white_noise, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_noise_add(V, white, N),
    integer(N),
    N >= 0.

test(add_pink_noise, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_noise_add(V, pink, N),
    integer(N),
    N >= 0.

test(add_brownian_noise, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_noise_add(V, brownian, N),
    integer(N),
    N >= 0.

test(noise_default_volume, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_noise_add(V, white, N),
    sampler_synth_oscillator_get_volume(N, Vol),
    Vol =:= 1.0.

test(noise_set_volume, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_noise_add(V, white, N),
    sampler_synth_oscillator_set_volume(N, 0.3),
    sampler_synth_oscillator_get_volume(N, Vol),
    abs(Vol - 0.3) < 0.001.

test(start_stop_voice_with_noise, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_noise_add(V, white, _),
    sampler_synth_voice_start(V),
    sampler_synth_voice_stop(V).

test(mixed_oscillator_and_noise, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 440.0, 0.0, O),
    sampler_synth_noise_add(V, pink, N),
    sampler_synth_oscillator_set_volume(O, 0.8),
    sampler_synth_oscillator_set_volume(N, 0.2),
    sampler_synth_voice_start(V),
    sampler_synth_voice_stop(V).

test(remove_noise, [nondet, cleanup(sampler_synth_voice_unload(V))]) :-
    sampler_synth_voice_create(V),
    sampler_synth_noise_add(V, white, N),
    sampler_synth_oscillator_remove(N).

:- end_tests(synth).
