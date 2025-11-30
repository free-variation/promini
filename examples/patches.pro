/*
 * patches.pro - Example synth patches
 * Each patch is a Prolog goal that sets up voices, effects, modulation routes.
 * Call a patch predicate to start sound, e.g. patch_underwater.
 */

:- use_module('src/prolog/promini.pro').

patch_underwater :-
    % Voice with two detuned oscillators
    synth_voice_create(V),
    synth_oscillator_add(V, 220.0, 0.0, O1),
    synth_oscillator_add(V, 221.5, 0.0, _),    % slight detune

    % LFO for slow vibrato
    mod_lfo_create(sine, 0.3, LFO),
    mod_route_create(LFO, oscillator, O1, frequency, 5.0, 220.0, 0.0, _),

    % Filter
    voice_attach_effect(V, lpf, [cutoff=400, order=4], _),

    % Start
    synth_voice_start(V).
