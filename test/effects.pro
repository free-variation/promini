:- module(test_effects, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(effects).

% Sound effect tests

test(sound_attach_bitcrush, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_bitcrush(Sound, 8, 8000, Effect),
    Effect = effect(sound(Sound), _),
    sound_unload(Sound).

test(sound_attach_lpf, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, lpf, [cutoff=1000.0], Effect),
    Effect = effect(sound(Sound), _),
    sound_unload(Sound).

test(sound_attach_hpf, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, hpf, [cutoff=500.0], Effect),
    Effect = effect(sound(Sound), _),
    sound_unload(Sound).

test(sound_attach_bpf, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_bpf(Sound, 1000.0, 2, Effect),
    Effect = effect(sound(Sound), _),
    sound_unload(Sound).

test(sound_attach_envelope, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_envelope(Sound, 0.1, 0.2, 0.3, 0.5, 1000.0, false, Effect),
    Effect = effect(sound(Sound), _),
    sound_unload(Sound).

test(sound_attach_delay, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_delay(Sound, 22050, 0.5, 0.8, Effect),
    Effect = effect(sound(Sound), _),
    sound_unload(Sound).

test(sound_attach_ping_pong_delay, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_ping_pong_delay(Sound, 44100, 22050, 0.5, 0.8, Effect),
    Effect = effect(sound(Sound), _),
    sound_unload(Sound).

test(sound_attach_reverb, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_reverb(Sound, [decay=0.8, wet=0.3], Effect),
    Effect = effect(sound(Sound), _),
    sound_unload(Sound).

test(sound_set_parameters, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, lpf, [cutoff=1000.0], Effect),
    effect_set_parameters(Effect, [cutoff=2000.0]),
    effects(sound(Sound), [effect(sound(Sound), lpf, _, Params)]),
    memberchk(cutoff=Cutoff, Params),
    abs(Cutoff - 2000.0) < 0.001,
    sound_unload(Sound).

test(sound_detach_effect, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_bitcrush(Sound, 8, 8000, Effect),
    effects(sound(Sound), [_]),
    effect_detach(Effect),
    effects(sound(Sound), []),
    sound_unload(Sound).

% Voice effect tests

test(voice_attach_bitcrush, [nondet]) :-
    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, bitcrush, [bits=8, sample_rate=8000], Effect),
    Effect = effect(voice(Voice), _),
    synth_voice_unload(Voice).

test(voice_attach_lpf, [nondet]) :-
    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, lpf, [cutoff=1000.0], Effect),
    Effect = effect(voice(Voice), _),
    synth_voice_unload(Voice).

test(voice_attach_hpf, [nondet]) :-
    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, hpf, [cutoff=500.0], Effect),
    Effect = effect(voice(Voice), _),
    synth_voice_unload(Voice).

test(voice_attach_bpf, [nondet]) :-
    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, bpf, [cutoff=1000.0, order=2], Effect),
    Effect = effect(voice(Voice), _),
    synth_voice_unload(Voice).

test(voice_attach_envelope, [nondet]) :-
    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, envelope, [attack=0.1, decay=0.2, break=0.3, break_level=0.5, duration_ms=1000.0, loop=false], Effect),
    Effect = effect(voice(Voice), _),
    synth_voice_unload(Voice).

test(voice_attach_delay, [nondet]) :-
    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, delay, [delay_in_frames=22050, decay=0.5, wet=0.8], Effect),
    Effect = effect(voice(Voice), _),
    synth_voice_unload(Voice).

test(voice_attach_ping_pong_delay, [nondet]) :-
    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, ping_pong_delay, [max_delay_in_frames=44100, delay_in_frames=22050, feedback=0.5, wet=0.8], Effect),
    Effect = effect(voice(Voice), _),
    synth_voice_unload(Voice).

test(voice_attach_reverb, [nondet]) :-
    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, reverb, [decay=0.8, wet=0.3], Effect),
    Effect = effect(voice(Voice), _),
    synth_voice_unload(Voice).

test(voice_set_parameters, [nondet]) :-
    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, lpf, [cutoff=1000.0], Effect),
    effect_set_parameters(Effect, [cutoff=2000.0]),
    synth_voice_unload(Voice).

test(voice_detach_effect, [nondet]) :-
    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, bitcrush, [bits=8, sample_rate=8000], Effect),
    effect_detach(Effect),
    synth_voice_unload(Voice).

% Multiple effects on voice

test(voice_multiple_effects, [nondet]) :-
    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, lpf, [cutoff=2000.0], _E1),
    voice_attach_effect(Voice, reverb, [decay=0.7, wet=0.3], _E2),
    synth_voice_unload(Voice).

:- end_tests(effects).
