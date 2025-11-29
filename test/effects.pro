:- module(test_effects, []).
:- use_module('src/prolog/sampler.pro').
:- use_module(library(plunit)).

:- begin_tests(effects).

% Sound effect tests

test(sound_attach_bitcrush, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_bitcrush(Sound, 8, 8000, Effect),
    Effect = effect(sound(Sound), _),
    sampler_sound_unload(Sound).

test(sound_attach_lpf, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, lpf, [cutoff=1000.0], Effect),
    Effect = effect(sound(Sound), _),
    sampler_sound_unload(Sound).

test(sound_attach_hpf, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, hpf, [cutoff=500.0], Effect),
    Effect = effect(sound(Sound), _),
    sampler_sound_unload(Sound).

test(sound_attach_bpf, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_bpf(Sound, 1000.0, 2, Effect),
    Effect = effect(sound(Sound), _),
    sampler_sound_unload(Sound).

test(sound_attach_envelope, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_envelope(Sound, 0.1, 0.2, 0.3, 0.5, 1000.0, false, Effect),
    Effect = effect(sound(Sound), _),
    sampler_sound_unload(Sound).

test(sound_attach_delay, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_delay(Sound, 22050, 0.5, 0.8, Effect),
    Effect = effect(sound(Sound), _),
    sampler_sound_unload(Sound).

test(sound_attach_ping_pong_delay, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_ping_pong_delay(Sound, 44100, 22050, 0.5, 0.8, Effect),
    Effect = effect(sound(Sound), _),
    sampler_sound_unload(Sound).

test(sound_attach_reverb, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_reverb(Sound, [decay=0.8, wet=0.3], Effect),
    Effect = effect(sound(Sound), _),
    sampler_sound_unload(Sound).

test(sound_set_parameters, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, lpf, [cutoff=1000.0], Effect),
    sampler_effect_set_parameters(Effect, [cutoff=2000.0]),
    sampler_sound_effects(Sound, [effect(lpf, _, Params)]),
    memberchk(cutoff=Cutoff, Params),
    abs(Cutoff - 2000.0) < 0.001,
    sampler_sound_unload(Sound).

test(sound_detach_effect, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_bitcrush(Sound, 8, 8000, Effect),
    sampler_sound_effects(Sound, [_]),
    sampler_effect_detach(Effect),
    sampler_sound_effects(Sound, []),
    sampler_sound_unload(Sound).

% Voice effect tests

test(voice_attach_bitcrush, [nondet]) :-
    sampler_synth_voice_create(Voice),
    sampler_synth_oscillator_add(Voice, 440.0, 0.0, _),
    sampler_voice_attach_effect(Voice, bitcrush, [bits=8, sample_rate=8000], Effect),
    Effect = effect(voice(Voice), _),
    sampler_synth_voice_unload(Voice).

test(voice_attach_lpf, [nondet]) :-
    sampler_synth_voice_create(Voice),
    sampler_synth_oscillator_add(Voice, 440.0, 0.0, _),
    sampler_voice_attach_effect(Voice, lpf, [cutoff=1000.0], Effect),
    Effect = effect(voice(Voice), _),
    sampler_synth_voice_unload(Voice).

test(voice_attach_hpf, [nondet]) :-
    sampler_synth_voice_create(Voice),
    sampler_synth_oscillator_add(Voice, 440.0, 0.0, _),
    sampler_voice_attach_effect(Voice, hpf, [cutoff=500.0], Effect),
    Effect = effect(voice(Voice), _),
    sampler_synth_voice_unload(Voice).

test(voice_attach_bpf, [nondet]) :-
    sampler_synth_voice_create(Voice),
    sampler_synth_oscillator_add(Voice, 440.0, 0.0, _),
    sampler_voice_attach_effect(Voice, bpf, [cutoff=1000.0, order=2], Effect),
    Effect = effect(voice(Voice), _),
    sampler_synth_voice_unload(Voice).

test(voice_attach_envelope, [nondet]) :-
    sampler_synth_voice_create(Voice),
    sampler_synth_oscillator_add(Voice, 440.0, 0.0, _),
    sampler_voice_attach_effect(Voice, envelope, [attack=0.1, decay=0.2, break=0.3, break_level=0.5, duration_ms=1000.0, loop=false], Effect),
    Effect = effect(voice(Voice), _),
    sampler_synth_voice_unload(Voice).

test(voice_attach_delay, [nondet]) :-
    sampler_synth_voice_create(Voice),
    sampler_synth_oscillator_add(Voice, 440.0, 0.0, _),
    sampler_voice_attach_effect(Voice, delay, [delay_in_frames=22050, decay=0.5, wet=0.8], Effect),
    Effect = effect(voice(Voice), _),
    sampler_synth_voice_unload(Voice).

test(voice_attach_ping_pong_delay, [nondet]) :-
    sampler_synth_voice_create(Voice),
    sampler_synth_oscillator_add(Voice, 440.0, 0.0, _),
    sampler_voice_attach_effect(Voice, ping_pong_delay, [max_delay_in_frames=44100, delay_in_frames=22050, feedback=0.5, wet=0.8], Effect),
    Effect = effect(voice(Voice), _),
    sampler_synth_voice_unload(Voice).

test(voice_attach_reverb, [nondet]) :-
    sampler_synth_voice_create(Voice),
    sampler_synth_oscillator_add(Voice, 440.0, 0.0, _),
    sampler_voice_attach_effect(Voice, reverb, [decay=0.8, wet=0.3], Effect),
    Effect = effect(voice(Voice), _),
    sampler_synth_voice_unload(Voice).

test(voice_set_parameters, [nondet]) :-
    sampler_synth_voice_create(Voice),
    sampler_synth_oscillator_add(Voice, 440.0, 0.0, _),
    sampler_voice_attach_effect(Voice, lpf, [cutoff=1000.0], Effect),
    sampler_effect_set_parameters(Effect, [cutoff=2000.0]),
    sampler_synth_voice_unload(Voice).

test(voice_detach_effect, [nondet]) :-
    sampler_synth_voice_create(Voice),
    sampler_synth_oscillator_add(Voice, 440.0, 0.0, _),
    sampler_voice_attach_effect(Voice, bitcrush, [bits=8, sample_rate=8000], Effect),
    sampler_effect_detach(Effect),
    sampler_synth_voice_unload(Voice).

% Multiple effects on voice

test(voice_multiple_effects, [nondet]) :-
    sampler_synth_voice_create(Voice),
    sampler_synth_oscillator_add(Voice, 440.0, 0.0, _),
    sampler_voice_attach_effect(Voice, lpf, [cutoff=2000.0], _E1),
    sampler_voice_attach_effect(Voice, reverb, [decay=0.7, wet=0.3], _E2),
    sampler_synth_voice_unload(Voice).

:- end_tests(effects).
