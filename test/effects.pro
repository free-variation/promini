:- module(test_effects, []).
:- use_module('src/prolog/sampler.pro').
:- use_module(library(plunit)).

:- begin_tests(effects).

test(attach_bitcrush, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_bitcrush(Sound, 8, 8000, Effect),
    Effect = effect(Sound, Ptr),
    integer(Ptr),
    sampler_sound_unload(Sound).

test(attach_effect_generic, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, bitcrush, [bits=4, sample_rate=4000], Effect),
    Effect = effect(Sound, Ptr),
    integer(Ptr),
    sampler_sound_unload(Sound).

test(attach_envelope, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_envelope(Sound, 0.1, 0.2, 0.3, 0.5, 1000.0, false, Effect),
    Effect = effect(Sound, Ptr),
    integer(Ptr),
    sampler_sound_unload(Sound).

test(attach_envelope_generic, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, envelope, [attack=0.2, decay=0.3, break=0.2, break_level=0.6, duration_ms=500.0, loop=false], Effect),
    Effect = effect(Sound, Ptr),
    integer(Ptr),
    sampler_sound_unload(Sound).

test(attach_lpf, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, lpf, [cutoff=1000.0], Effect),
    Effect = effect(Sound, Ptr),
    integer(Ptr),
    sampler_sound_unload(Sound).

test(query_effects, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_bitcrush(Sound, 8, 8000, _),
    sampler_sound_attach_envelope(Sound, 0.1, 0.2, 0.3, 0.5, 1000.0, false, _),
    sampler_sound_effects(Sound, Effects),
    \+ var(Effects),
    Effects = [
        effect(bitcrush, Ptr1, [bits=8, sample_rate=8000]),
        effect(envelope, Ptr2, [attack=Attack, decay=Decay, break=Break, break_level=BreakLevel, duration_ms=DurationMs, loop=Loop])
    ],
    integer(Ptr1),
    integer(Ptr2),
    abs(Attack - 0.1) < 0.001,
    abs(Decay - 0.2) < 0.001,
    abs(Break - 0.3) < 0.001,
    abs(BreakLevel - 0.5) < 0.001,
    abs(DurationMs - 1000.0) < 0.001,
    Loop = 0,
    sampler_sound_unload(Sound).

test(query_effects_empty, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_effects(Sound, Effects),
    Effects = [],
    sampler_sound_unload(Sound).

test(query_lpf, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, lpf, [cutoff=1500.0, order=4], _),
    sampler_sound_effects(Sound, [effect(lpf, Ptr, Params)]),
    integer(Ptr),
    memberchk(cutoff=Cutoff, Params),
    memberchk(order=Order, Params),
    abs(Cutoff - 1500.0) < 0.001,
    Order =:= 4,
    sampler_sound_unload(Sound).

test(set_bitcrush_parameters, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_bitcrush(Sound, 8, 8000, Effect),
    sampler_effect_set_parameters(Effect, [bits=4, sample_rate=4000]),
    sampler_sound_effects(Sound, [effect(bitcrush, _, [bits=4, sample_rate=4000])]),
    sampler_sound_unload(Sound).

test(set_envelope_parameters, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_envelope(Sound, 0.1, 0.2, 0.3, 0.5, 1000.0, false, Effect),
    sampler_effect_set_parameters(Effect, [attack=0.3, decay=0.4]),
    sampler_sound_effects(Sound, [effect(envelope, _, Params)]),
    memberchk(attack=Attack, Params),
    memberchk(decay=Decay, Params),
    abs(Attack - 0.3) < 0.001,
    abs(Decay - 0.4) < 0.001,
    sampler_sound_unload(Sound).

test(set_envelope_loop, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_envelope(Sound, 0.1, 0.2, 0.3, 0.5, 1000.0, false, Effect),
    sampler_effect_set_parameters(Effect, [loop=true]),
    sampler_sound_effects(Sound, [effect(envelope, _, Params)]),
    memberchk(loop=Loop, Params),
    Loop =:= 1,
    sampler_sound_unload(Sound).

test(set_envelope_invalid_proportions, [error(domain_error(_, _))]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_envelope(Sound, 0.1, 0.2, 0.3, 0.5, 1000.0, false, Effect),
    sampler_effect_set_parameters(Effect, [attack=0.5, decay=0.5, break=0.5]),
    sampler_sound_unload(Sound).

test(set_lpf_cutoff, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, lpf, [cutoff=1000.0], Effect),
    sampler_effect_set_parameters(Effect, [cutoff=2500.0]),
    sampler_sound_effects(Sound, [effect(lpf, _, Params)]),
    memberchk(cutoff=Cutoff, Params),
    abs(Cutoff - 2500.0) < 0.001,
    sampler_sound_unload(Sound).

test(set_lpf_order, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, lpf, [cutoff=1000.0, order=2], Effect),
    sampler_effect_set_parameters(Effect, [order=6]),
    sampler_sound_effects(Sound, [effect(lpf, _, Params)]),
    memberchk(order=Order, Params),
    Order =:= 6,
    sampler_sound_unload(Sound).

test(set_lpf_both_parameters, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, lpf, [cutoff=1000.0], Effect),
    sampler_effect_set_parameters(Effect, [cutoff=4000.0, order=8]),
    sampler_sound_effects(Sound, [effect(lpf, _, Params)]),
    memberchk(cutoff=Cutoff, Params),
    memberchk(order=Order, Params),
    abs(Cutoff - 4000.0) < 0.001,
    Order =:= 8,
    sampler_sound_unload(Sound).

test(attach_hpf, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, hpf, [cutoff=500.0], Effect),
    Effect = effect(Sound, Ptr),
    integer(Ptr),
    sampler_sound_unload(Sound).

test(query_hpf, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, hpf, [cutoff=800.0, order=4], _),
    sampler_sound_effects(Sound, [effect(hpf, Ptr, Params)]),
    integer(Ptr),
    memberchk(cutoff=Cutoff, Params),
    memberchk(order=Order, Params),
    abs(Cutoff - 800.0) < 0.001,
    Order =:= 4,
    sampler_sound_unload(Sound).

test(set_hpf_cutoff, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, hpf, [cutoff=500.0], Effect),
    sampler_effect_set_parameters(Effect, [cutoff=1200.0]),
    sampler_sound_effects(Sound, [effect(hpf, _, Params)]),
    memberchk(cutoff=Cutoff, Params),
    abs(Cutoff - 1200.0) < 0.001,
    sampler_sound_unload(Sound).

test(set_hpf_order, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, hpf, [cutoff=500.0, order=2], Effect),
    sampler_effect_set_parameters(Effect, [order=6]),
    sampler_sound_effects(Sound, [effect(hpf, _, Params)]),
    memberchk(order=Order, Params),
    Order =:= 6,
    sampler_sound_unload(Sound).

test(set_hpf_both_parameters, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, hpf, [cutoff=500.0], Effect),
    sampler_effect_set_parameters(Effect, [cutoff=2000.0, order=8]),
    sampler_sound_effects(Sound, [effect(hpf, _, Params)]),
    memberchk(cutoff=Cutoff, Params),
    memberchk(order=Order, Params),
    abs(Cutoff - 2000.0) < 0.001,
    Order =:= 8,
    sampler_sound_unload(Sound).

test(detach_single_effect, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_bitcrush(Sound, 8, 8000, Effect),
    sampler_sound_effects(Sound, [_]),
    sampler_effect_detach(Effect),
    sampler_sound_effects(Sound, []),
    sampler_sound_unload(Sound).

test(detach_first_of_two, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_bitcrush(Sound, 8, 8000, Effect1),
    sampler_sound_attach_envelope(Sound, 0.1, 0.2, 0.3, 0.5, 1000.0, false, _Effect2),
    sampler_sound_effects(Sound, [_, _]),
    sampler_effect_detach(Effect1),
    sampler_sound_effects(Sound, [effect(envelope, _, _)]),
    sampler_sound_unload(Sound).

test(detach_second_of_two, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_bitcrush(Sound, 8, 8000, _Effect1),
    sampler_sound_attach_envelope(Sound, 0.1, 0.2, 0.3, 0.5, 1000.0, false, Effect2),
    sampler_sound_effects(Sound, [_, _]),
    sampler_effect_detach(Effect2),
    sampler_sound_effects(Sound, [effect(bitcrush, _, _)]),
    sampler_sound_unload(Sound).

test(detach_middle_of_three, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_bitcrush(Sound, 8, 8000, _Effect1),
    sampler_sound_attach_envelope(Sound, 0.1, 0.2, 0.3, 0.5, 1000.0, false, Effect2),
    sampler_sound_attach_bitcrush(Sound, 4, 4000, _Effect3),
    sampler_sound_effects(Sound, [_, _, _]),
    sampler_effect_detach(Effect2),
    sampler_sound_effects(Sound, [effect(bitcrush, _, [bits=8, sample_rate=8000]), effect(bitcrush, _, [bits=4, sample_rate=4000])]),
    sampler_sound_unload(Sound).

test(clear_no_effects, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_clear_effects(Sound),
    sampler_sound_effects(Sound, []),
    sampler_sound_unload(Sound).

test(clear_multiple_effects, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_bitcrush(Sound, 8, 8000, _),
    sampler_sound_attach_envelope(Sound, 0.1, 0.2, 0.3, 0.5, 1000.0, false, _),
    sampler_sound_attach_bitcrush(Sound, 4, 4000, _),
    sampler_sound_effects(Sound, [_, _, _]),
    sampler_sound_clear_effects(Sound),
    sampler_sound_effects(Sound, []),
    sampler_sound_unload(Sound).

:- end_tests(effects).
