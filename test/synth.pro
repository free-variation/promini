:- module(test_synth, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(synth).

% Voice tests

test(voices_in_use_initially_zero, [nondet]) :-
    synth_voices_in_use(Count),
    Count =:= 0.

test(create_voice, [nondet, cleanup(synth_voice_uninit(Handle))]) :-
    synth_voice_init(Handle),
    Handle = voice(_).

test(voices_in_use_after_create, [nondet, cleanup(synth_voice_uninit(Handle))]) :-
    synth_voices_in_use(Before),
    synth_voice_init(Handle),
    synth_voices_in_use(After),
    After =:= Before + 1.

% Oscillator tests

test(add_oscillator, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    O = oscillator(_).

test(oscillator_get_frequency, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    synth_oscillator_get_frequency(O, Freq),
    Freq =:= 440.0.

test(oscillator_set_frequency, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    synth_oscillator_set_frequency(O, 880.0),
    synth_oscillator_get_frequency(O, Freq),
    Freq =:= 880.0.

test(oscillator_get_phase, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    synth_oscillator_get_phase(O, Phase),
    Phase >= 0.0,
    Phase < 1.0.

test(oscillator_set_phase, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    synth_oscillator_set_phase(O, 0.5),
    synth_oscillator_get_phase(O, Phase),
    abs(Phase - 0.5) < 0.01.

test(remove_oscillator, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    synth_oscillator_remove(O).

test(multiple_oscillators, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O1),
    synth_oscillator_add(V, 880.0, 0.0, O2),
    synth_oscillator_add(V, 1320.0, 0.0, O3),
    synth_oscillator_get_frequency(O1, F1),
    synth_oscillator_get_frequency(O2, F2),
    synth_oscillator_get_frequency(O3, F3),
    F1 =:= 440.0,
    F2 =:= 880.0,
    F3 =:= 1320.0.

test(start_stop_voice_with_oscillator, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, _),
    synth_voice_start(V),
    synth_voice_stop(V).

test(start_empty_voice_fails, [fail, setup(synth_voice_init(V)), cleanup(synth_voice_uninit(V))]) :-
    synth_voice_start(V).

% Oscillator volume tests

test(oscillator_default_volume, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    synth_oscillator_get_volume(O, Vol),
    Vol =:= 1.0.

test(oscillator_set_volume, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    synth_oscillator_set_volume(O, 0.5),
    synth_oscillator_get_volume(O, Vol),
    abs(Vol - 0.5) < 0.001.

% Noise tests

test(add_white_noise, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_noise_add(V, white, N),
    N = oscillator(_).

test(add_pink_noise, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_noise_add(V, pink, N),
    N = oscillator(_).

test(add_brownian_noise, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_noise_add(V, brownian, N),
    N = oscillator(_).

test(noise_default_volume, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_noise_add(V, white, N),
    synth_oscillator_get_volume(N, Vol),
    Vol =:= 1.0.

test(noise_set_volume, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_noise_add(V, white, N),
    synth_oscillator_set_volume(N, 0.3),
    synth_oscillator_get_volume(N, Vol),
    abs(Vol - 0.3) < 0.001.

test(start_stop_voice_with_noise, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_noise_add(V, white, _),
    synth_voice_start(V),
    synth_voice_stop(V).

test(mixed_oscillator_and_noise, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    synth_noise_add(V, pink, N),
    synth_oscillator_set_volume(O, 0.8),
    synth_oscillator_set_volume(N, 0.2),
    synth_voice_start(V),
    synth_voice_stop(V).

test(remove_noise, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_noise_add(V, white, N),
    synth_oscillator_remove(N).

% Voice set frequency tests

test(voice_set_frequency_single_osc, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    synth_voice_set_frequency(V, 880.0),
    synth_oscillator_get_frequency(O, Freq),
    abs(Freq - 880.0) < 0.001.

test(voice_set_frequency_harmonic_ratios, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O1),
    synth_oscillator_add(V, 880.0, 0.0, O2),
    synth_oscillator_add(V, 1320.0, 0.0, O3),
    synth_voice_set_frequency(V, 220.0),
    synth_oscillator_get_frequency(O1, F1),
    synth_oscillator_get_frequency(O2, F2),
    synth_oscillator_get_frequency(O3, F3),
    abs(F1 - 220.0) < 0.001,
    abs(F2 - 440.0) < 0.001,
    abs(F3 - 660.0) < 0.001.

test(voice_set_frequency_no_oscillators, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_voice_set_frequency(V, 440.0).

test(voice_set_frequency_with_noise, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    synth_noise_add(V, white, _),
    synth_voice_set_frequency(V, 880.0),
    synth_oscillator_get_frequency(O, Freq),
    abs(Freq - 880.0) < 0.001.

:- end_tests(synth).
