:- module(test_effects, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(effects).

% Sound effect tests

test(sound_attach_bitcrush, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_bitcrush(Sound, 8, 8000, Effect),
    Effect = effect(Sound, _),
    sound_unload(Sound).

test(sound_attach_lpf, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, lpf, [cutoff=1000.0], Effect),
    Effect = effect(Sound, _),
    sound_unload(Sound).

test(sound_attach_hpf, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, hpf, [cutoff=500.0], Effect),
    Effect = effect(Sound, _),
    sound_unload(Sound).

test(sound_attach_bpf, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_bpf(Sound, 1000.0, 2, Effect),
    Effect = effect(Sound, _),
    sound_unload(Sound).

test(sound_attach_delay, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_delay(Sound, 22050, 0.5, 0.8, Effect),
    Effect = effect(Sound, _),
    sound_unload(Sound).

test(sound_attach_ping_pong_delay, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_ping_pong_delay(Sound, 44100, 22050, 0.5, 0.8, Effect),
    Effect = effect(Sound, _),
    sound_unload(Sound).

test(sound_attach_reverb, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_reverb(Sound, [decay=0.8, wet=0.3], Effect),
    Effect = effect(Sound, _),
    sound_unload(Sound).

test(sound_set_parameters, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, lpf, [cutoff=1000.0], Effect),
    effect_set_parameters(Effect, [cutoff=2000.0]),
    effects(Sound, [effect(Sound, lpf, _, Params)]),
    memberchk(cutoff=Cutoff, Params),
    abs(Cutoff - 2000.0) < 0.001,
    sound_unload(Sound).

test(sound_detach_effect, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_bitcrush(Sound, 8, 8000, Effect),
    effects(Sound, [_]),
    effect_detach(Effect),
    effects(Sound, []),
    sound_unload(Sound).

% Voice effect tests

test(voice_attach_bitcrush, [nondet, cleanup(synth_voice_uninit(Voice))]) :-
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, bitcrush, [bits=8, sample_rate=8000], Effect),
    Effect = effect(Voice, _).

test(voice_attach_lpf, [nondet, cleanup(synth_voice_uninit(Voice))]) :-
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, lpf, [cutoff=1000.0], Effect),
    Effect = effect(Voice, _).

test(voice_attach_hpf, [nondet, cleanup(synth_voice_uninit(Voice))]) :-
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, hpf, [cutoff=500.0], Effect),
    Effect = effect(Voice, _).

test(voice_attach_bpf, [nondet, cleanup(synth_voice_uninit(Voice))]) :-
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, bpf, [cutoff=1000.0, order=2], Effect),
    Effect = effect(Voice, _).

test(voice_attach_delay, [nondet, cleanup(synth_voice_uninit(Voice))]) :-
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, delay, [delay_in_frames=22050, decay=0.5, wet=0.8], Effect),
    Effect = effect(Voice, _).

test(voice_attach_ping_pong_delay, [nondet, cleanup(synth_voice_uninit(Voice))]) :-
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, ping_pong_delay, [max_delay_in_frames=44100, delay_in_frames=22050, feedback=0.5, wet=0.8], Effect),
    Effect = effect(Voice, _).

test(voice_attach_reverb, [nondet, cleanup(synth_voice_uninit(Voice))]) :-
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, reverb, [decay=0.8, wet=0.3], Effect),
    Effect = effect(Voice, _).

test(voice_set_parameters, [nondet, cleanup(synth_voice_uninit(Voice))]) :-
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, lpf, [cutoff=1000.0], Effect),
    effect_set_parameters(Effect, [cutoff=2000.0]).

test(voice_detach_effect, [nondet, cleanup(synth_voice_uninit(Voice))]) :-
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, bitcrush, [bits=8, sample_rate=8000], Effect),
    effect_detach(Effect).

% Multiple effects on voice

test(voice_multiple_effects, [nondet, cleanup(synth_voice_uninit(Voice))]) :-
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, lpf, [cutoff=2000.0], _E1),
    voice_attach_effect(Voice, reverb, [decay=0.7, wet=0.3], _E2).

% Pan effect tests

test(sound_attach_pan, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, pan, [pan=0.5], Effect),
    Effect = effect(Sound, _),
    sound_unload(Sound).

test(voice_attach_pan, [nondet, cleanup(synth_voice_uninit(Voice))]) :-
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, pan, [pan=(-0.5)], Effect),
    Effect = effect(Voice, _).

% Moog effect tests

test(sound_attach_moog, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, moog, [cutoff=1000.0, resonance=1.5], Effect),
    Effect = effect(Sound, _),
    sound_unload(Sound).

test(voice_attach_moog, [nondet, cleanup(synth_voice_uninit(Voice))]) :-
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, moog, [cutoff=500.0, resonance=2.0, drive=1.5], Effect),
    Effect = effect(Voice, _).

% effects/2 query tests for all effect types

test(effects_query_bitcrush, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, bitcrush, [bits=8, sample_rate=8000], _),
    effects(Sound, [effect(Sound, bitcrush, _, Params)]),
    memberchk(bits=8, Params),
    sound_unload(Sound).

test(effects_query_lpf, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, lpf, [cutoff=1500.0], _),
    effects(Sound, [effect(Sound, lpf, _, Params)]),
    memberchk(cutoff=Cutoff, Params),
    abs(Cutoff - 1500.0) < 0.001,
    sound_unload(Sound).

test(effects_query_hpf, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, hpf, [cutoff=500.0], _),
    effects(Sound, [effect(Sound, hpf, _, Params)]),
    memberchk(cutoff=Cutoff, Params),
    abs(Cutoff - 500.0) < 0.001,
    sound_unload(Sound).

test(effects_query_bpf, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, bpf, [cutoff=1000.0], _),
    effects(Sound, [effect(Sound, bpf, _, Params)]),
    memberchk(cutoff=Cutoff, Params),
    abs(Cutoff - 1000.0) < 0.001,
    sound_unload(Sound).

test(effects_query_delay, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, delay, [delay_in_frames=22050, decay=0.5, wet=0.8], _),
    effects(Sound, [effect(Sound, delay, _, Params)]),
    memberchk(decay=Decay, Params),
    abs(Decay - 0.5) < 0.001,
    sound_unload(Sound).

test(effects_query_ping_pong_delay, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100, delay_in_frames=22050, feedback=0.5, wet=0.8], _),
    effects(Sound, [effect(Sound, ping_pong_delay, _, Params)]),
    memberchk(feedback=Feedback, Params),
    abs(Feedback - 0.5) < 0.001,
    sound_unload(Sound).

test(effects_query_reverb, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [decay=0.8, wet=0.3], _),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(decay=Decay, Params),
    abs(Decay - 0.8) < 0.001,
    sound_unload(Sound).

test(effects_query_pan, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, pan, [pan=0.75], _),
    effects(Sound, [effect(Sound, pan, _, Params)]),
    memberchk(pan=Pan, Params),
    abs(Pan - 0.75) < 0.001,
    sound_unload(Sound).

test(effects_query_moog, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, moog, [cutoff=2000.0, resonance=1.5, drive=1.2], _),
    effects(Sound, [effect(Sound, moog, _, Params)]),
    memberchk(cutoff=Cutoff, Params),
    memberchk(resonance=Resonance, Params),
    memberchk(drive=Drive, Params),
    abs(Cutoff - 2000.0) < 0.001,
    abs(Resonance - 1.5) < 0.001,
    abs(Drive - 1.2) < 0.001,
    sound_unload(Sound).

% VCA effect tests

test(sound_attach_vca, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, vca, [gain=0.5], Effect),
    Effect = effect(Sound, _),
    sound_unload(Sound).

test(sound_attach_vca_default_gain, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, vca, [], Effect),
    Effect = effect(Sound, _),
    effects(Sound, [effect(Sound, vca, _, Params)]),
    memberchk(gain=Gain, Params),
    abs(Gain - 1.0) < 0.001,
    sound_unload(Sound).

test(voice_attach_vca, [nondet, cleanup(synth_voice_uninit(Voice))]) :-
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, vca, [gain=0.8], Effect),
    Effect = effect(Voice, _).

test(vca_set_parameters, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, vca, [gain=1.0], Effect),
    effect_set_parameters(Effect, [gain=0.25]),
    effects(Sound, [effect(Sound, vca, _, Params)]),
    memberchk(gain=Gain, Params),
    abs(Gain - 0.25) < 0.001,
    sound_unload(Sound).

test(effects_query_vca, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, vca, [gain=0.75], _),
    effects(Sound, [effect(Sound, vca, _, Params)]),
    memberchk(gain=Gain, Params),
    abs(Gain - 0.75) < 0.001,
    sound_unload(Sound).

% Compressor effect tests

test(sound_attach_compressor, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, compressor, [threshold=0.8], Effect),
    Effect = effect(Sound, _),
    sound_unload(Sound).

test(sound_attach_compressor_defaults, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, compressor, [], Effect),
    Effect = effect(Sound, _),
    effects(Sound, [effect(Sound, compressor, _, Params)]),
    memberchk(threshold=Threshold, Params),
    abs(Threshold - 0.5) < 0.001,
    memberchk(ratio=Ratio, Params),
    abs(Ratio - 4.0) < 0.01,
    memberchk(knee=Knee, Params),
    abs(Knee - 6.0) < 0.01,
    memberchk(attack_ms=Attack, Params),
    abs(Attack - 5.0) < 0.1,
    memberchk(release_ms=Release, Params),
    abs(Release - 100.0) < 1.0,
    memberchk(makeup_gain=Makeup, Params),
    abs(Makeup - 1.0) < 0.001,
    sound_unload(Sound).

test(voice_attach_compressor, [nondet, cleanup(synth_voice_uninit(Voice))]) :-
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    voice_attach_effect(Voice, compressor, [threshold=0.5, ratio=8.0, attack_ms=0.5, release_ms=50.0], Effect),
    Effect = effect(Voice, _).

test(compressor_set_parameters, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, compressor, [threshold=1.0], Effect),
    effect_set_parameters(Effect, [threshold=0.5, ratio=10.0, release_ms=200.0]),
    effects(Sound, [effect(Sound, compressor, _, Params)]),
    memberchk(threshold=Threshold, Params),
    abs(Threshold - 0.5) < 0.001,
    memberchk(ratio=Ratio, Params),
    abs(Ratio - 10.0) < 0.01,
    memberchk(release_ms=Release, Params),
    abs(Release - 200.0) < 1.0,
    sound_unload(Sound).

test(effects_query_compressor, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, compressor, [threshold=0.75, ratio=6.0, knee=3.0, attack_ms=0.2, release_ms=150.0, makeup_gain=1.5], _),
    effects(Sound, [effect(Sound, compressor, _, Params)]),
    memberchk(threshold=Threshold, Params),
    memberchk(ratio=Ratio, Params),
    memberchk(knee=Knee, Params),
    memberchk(attack_ms=Attack, Params),
    memberchk(release_ms=Release, Params),
    memberchk(makeup_gain=Makeup, Params),
    abs(Threshold - 0.75) < 0.001,
    abs(Ratio - 6.0) < 0.01,
    abs(Knee - 3.0) < 0.01,
    abs(Attack - 0.2) < 0.01,
    abs(Release - 150.0) < 1.0,
    abs(Makeup - 1.5) < 0.001,
    sound_unload(Sound).

test(compressor_as_limiter, [nondet]) :-
    % High ratio acts as limiter
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, compressor, [threshold=0.9, ratio=100.0, knee=0.0], Effect),
    Effect = effect(Sound, _),
    sound_unload(Sound).

:- end_tests(effects).
