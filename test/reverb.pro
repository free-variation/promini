:- module(test_reverb, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(reverb).

% Basic attachment tests

test(attach_reverb_defaults, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    Effect = effect(Sound, Ptr),
    ground(Ptr),
    sound_unload(Sound).

test(attach_reverb_with_params, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [decay=0.8, wet=0.5], Effect),
    Effect = effect(Sound, Ptr),
    ground(Ptr),
    sound_unload(Sound).

test(attach_reverb_generic, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [decay=0.7, damping=0.3], Effect),
    Effect = effect(Sound, Ptr),
    ground(Ptr),
    sound_unload(Sound).

% Query parameters

test(query_reverb_defaults, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], _),
    effects(Sound, [effect(Sound, reverb, Ptr, Params)]),
    ground(Ptr),
    memberchk(decay=Decay, Params),
    memberchk(damping=Damping, Params),
    memberchk(wet=Wet, Params),
    memberchk(dry=Dry, Params),
    memberchk(predelay_ms=Predelay, Params),
    memberchk(bandwidth=Bandwidth, Params),
    abs(Decay - 0.5) < 0.001,
    abs(Damping - 0.5) < 0.001,
    abs(Wet - 0.3) < 0.001,
    abs(Dry - 1.0) < 0.001,
    abs(Predelay - 20.0) < 0.001,
    abs(Bandwidth - 0.7) < 0.001,
    sound_unload(Sound).

test(query_reverb_custom, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [decay=0.85, damping=0.4, wet=0.6, dry=0.4], _),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(decay=Decay, Params),
    memberchk(damping=Damping, Params),
    memberchk(wet=Wet, Params),
    memberchk(dry=Dry, Params),
    abs(Decay - 0.85) < 0.001,
    abs(Damping - 0.4) < 0.001,
    abs(Wet - 0.6) < 0.001,
    abs(Dry - 0.4) < 0.001,
    sound_unload(Sound).

% Set parameters

test(set_reverb_decay, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [decay=0.9]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(decay=Decay, Params),
    abs(Decay - 0.9) < 0.001,
    sound_unload(Sound).

test(set_reverb_damping, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [damping=0.7]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(damping=Damping, Params),
    abs(Damping - 0.7) < 0.001,
    sound_unload(Sound).

test(set_reverb_wet_dry, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [wet=0.8, dry=0.2]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(wet=Wet, Params),
    memberchk(dry=Dry, Params),
    abs(Wet - 0.8) < 0.001,
    abs(Dry - 0.2) < 0.001,
    sound_unload(Sound).

test(set_reverb_predelay, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [predelay_ms=50.0]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(predelay_ms=Predelay, Params),
    abs(Predelay - 50.0) < 0.001,
    sound_unload(Sound).

test(set_reverb_bandwidth, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [bandwidth=0.5]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(bandwidth=Bandwidth, Params),
    abs(Bandwidth - 0.5) < 0.001,
    sound_unload(Sound).

test(set_reverb_multiple_params, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [decay=0.85, damping=0.3, wet=0.5, predelay_ms=40.0]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(decay=Decay, Params),
    memberchk(damping=Damping, Params),
    memberchk(wet=Wet, Params),
    memberchk(predelay_ms=Predelay, Params),
    abs(Decay - 0.85) < 0.001,
    abs(Damping - 0.3) < 0.001,
    abs(Wet - 0.5) < 0.001,
    abs(Predelay - 40.0) < 0.001,
    sound_unload(Sound).

% Modulation parameters

test(set_reverb_mod_rate, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [mod_rate=0.8]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(mod_rate=ModRate, Params),
    abs(ModRate - 0.8) < 0.001,
    sound_unload(Sound).

test(set_reverb_mod_depth, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [mod_depth=0.7]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(mod_depth=ModDepth, Params),
    abs(ModDepth - 0.7) < 0.001,
    sound_unload(Sound).

% Stereo width

test(set_reverb_width, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [width=1.5]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(width=Width, Params),
    abs(Width - 1.5) < 0.001,
    sound_unload(Sound).

test(set_reverb_width_mono, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [width=0.0], _),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(width=Width, Params),
    abs(Width - 0.0) < 0.001,
    sound_unload(Sound).

% Cross-feed

test(set_reverb_cross_feed, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [cross_feed=0.3]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(cross_feed=CrossFeed, Params),
    abs(CrossFeed - 0.3) < 0.001,
    sound_unload(Sound).

% Output filters (low_cut and high_cut)

test(set_reverb_low_cut, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [low_cut=120.0]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(low_cut=LowCut, Params),
    abs(LowCut - 120.0) < 0.001,
    sound_unload(Sound).

test(set_reverb_high_cut, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [high_cut=8000.0]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(high_cut=HighCut, Params),
    abs(HighCut - 8000.0) < 0.001,
    sound_unload(Sound).

test(query_reverb_filter_defaults, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], _),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(low_cut=LowCut, Params),
    memberchk(high_cut=HighCut, Params),
    abs(LowCut - 80.0) < 0.001,
    abs(HighCut - 12000.0) < 0.001,
    sound_unload(Sound).

% Shimmer parameters

test(set_reverb_shimmer1, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [shimmer1_shift=12.0, shimmer1_mix=0.3]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(shimmer1_shift=Shift, Params),
    memberchk(shimmer1_mix=Mix, Params),
    abs(Shift - 12.0) < 0.001,
    abs(Mix - 0.3) < 0.001,
    sound_unload(Sound).

test(set_reverb_shimmer2, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [shimmer2_shift=19.0, shimmer2_mix=0.2]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(shimmer2_shift=Shift, Params),
    memberchk(shimmer2_mix=Mix, Params),
    abs(Shift - 19.0) < 0.001,
    abs(Mix - 0.2) < 0.001,
    sound_unload(Sound).

test(set_reverb_shimmer_negative, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [shimmer1_shift=(-12.0), shimmer1_mix=0.4], _),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(shimmer1_shift=Shift, Params),
    abs(Shift - (-12.0)) < 0.001,
    sound_unload(Sound).

test(query_reverb_shimmer_defaults, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], _),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(shimmer1_shift=Shift1, Params),
    memberchk(shimmer1_mix=Mix1, Params),
    memberchk(shimmer2_shift=Shift2, Params),
    memberchk(shimmer2_mix=Mix2, Params),
    abs(Shift1 - 0.0) < 0.001,
    abs(Mix1 - 0.0) < 0.001,
    abs(Shift2 - 0.0) < 0.001,
    abs(Mix2 - 0.0) < 0.001,
    sound_unload(Sound).

% Room size

test(query_reverb_size_default, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], _),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(size=Size, Params),
    abs(Size - 1.0) < 0.001,
    sound_unload(Sound).

test(attach_reverb_with_size, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [size=1.5], _),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(size=Size, Params),
    abs(Size - 1.5) < 0.001,
    sound_unload(Sound).

test(set_reverb_size, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [size=0.5]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(size=Size, Params),
    abs(Size - 0.5) < 0.001,
    sound_unload(Sound).

test(set_reverb_size_large, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [size=2.0]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(size=Size, Params),
    abs(Size - 2.0) < 0.001,
    sound_unload(Sound).

% Tank highpass

test(query_reverb_hp_default, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], _),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(hp=HP, Params),
    abs(HP - 0.0) < 0.001,
    sound_unload(Sound).

test(attach_reverb_with_hp, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [hp=0.3], _),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(hp=HP, Params),
    abs(HP - 0.3) < 0.001,
    sound_unload(Sound).

test(set_reverb_hp, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [hp=0.5]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(hp=HP, Params),
    abs(HP - 0.5) < 0.001,
    sound_unload(Sound).

% Freeze mode

test(query_reverb_freeze_default, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], _),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(freeze=Freeze, Params),
    Freeze = false,
    sound_unload(Sound).

test(attach_reverb_with_freeze, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [freeze=true], _),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(freeze=Freeze, Params),
    Freeze = true,
    sound_unload(Sound).

test(set_reverb_freeze, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [freeze=true]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(freeze=Freeze, Params),
    Freeze = true,
    sound_unload(Sound).

test(set_reverb_freeze_off, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [freeze=true], Effect),
    effect_set_parameters(Effect, [freeze=false]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(freeze=Freeze, Params),
    Freeze = false,
    sound_unload(Sound).

% Shimmer in-loop

test(query_reverb_shimmer_in_loop_default, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], _),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(shimmer_in_loop=InLoop, Params),
    InLoop = false,
    sound_unload(Sound).

test(attach_reverb_with_shimmer_in_loop, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [shimmer_in_loop=true], _),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(shimmer_in_loop=InLoop, Params),
    InLoop = true,
    sound_unload(Sound).

test(set_reverb_shimmer_in_loop, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effect_set_parameters(Effect, [shimmer_in_loop=true]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(shimmer_in_loop=InLoop, Params),
    InLoop = true,
    sound_unload(Sound).

test(set_reverb_shimmer_in_loop_off, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [shimmer_in_loop=true], Effect),
    effect_set_parameters(Effect, [shimmer_in_loop=false]),
    effects(Sound, [effect(Sound, reverb, _, Params)]),
    memberchk(shimmer_in_loop=InLoop, Params),
    InLoop = false,
    sound_unload(Sound).

% Detach and clear

test(detach_reverb, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], Effect),
    effects(Sound, [_]),
    effect_detach(Effect),
    effects(Sound, []),
    sound_unload(Sound).

test(clear_reverb, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, reverb, [], _),
    effects(Sound, [_]),
    clear_effects(Sound),
    effects(Sound, []),
    sound_unload(Sound).

% Multiple effects

test(reverb_with_other_effects, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, lpf, [cutoff=2000.0], _),
    sound_attach_effect(Sound, reverb, [decay=0.7], _),
    effects(Sound, [effect(Sound, lpf, _, _), effect(Sound, reverb, _, _)]),
    sound_unload(Sound).

test(reverb_in_chain, [nondet]) :-
    sound_load('audio/counting.wav', Sound),
    sound_attach_effect(Sound, hpf, [cutoff=100.0], _),
    sound_attach_effect(Sound, reverb, [decay=0.8, wet=0.5], _),
    sound_attach_effect(Sound, lpf, [cutoff=8000.0], _),
    effects(Sound, [
        effect(Sound, hpf, _, _),
        effect(Sound, reverb, _, _),
        effect(Sound, lpf, _, _)
    ]),
    sound_unload(Sound).

:- end_tests(reverb).
