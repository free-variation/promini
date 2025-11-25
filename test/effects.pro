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

% BPF tests

test(attach_bpf, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_bpf(Sound, 1000.0, 2, Effect),
    Effect = effect(Sound, Ptr),
    integer(Ptr),
    sampler_sound_unload(Sound).

test(attach_bpf_generic, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, bpf, [cutoff=2000.0, order=4], Effect),
    Effect = effect(Sound, Ptr),
    integer(Ptr),
    sampler_sound_unload(Sound).

test(query_bpf, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, bpf, [cutoff=1500.0, order=4], _),
    sampler_sound_effects(Sound, [effect(bpf, Ptr, Params)]),
    integer(Ptr),
    memberchk(cutoff=Cutoff, Params),
    memberchk(order=Order, Params),
    abs(Cutoff - 1500.0) < 0.001,
    Order =:= 4,
    sampler_sound_unload(Sound).

test(set_bpf_cutoff, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, bpf, [cutoff=1000.0], Effect),
    sampler_effect_set_parameters(Effect, [cutoff=2500.0]),
    sampler_sound_effects(Sound, [effect(bpf, _, Params)]),
    memberchk(cutoff=Cutoff, Params),
    abs(Cutoff - 2500.0) < 0.001,
    sampler_sound_unload(Sound).

test(set_bpf_order, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, bpf, [cutoff=1000.0, order=2], Effect),
    sampler_effect_set_parameters(Effect, [order=6]),
    sampler_sound_effects(Sound, [effect(bpf, _, Params)]),
    memberchk(order=Order, Params),
    Order =:= 6,
    sampler_sound_unload(Sound).

test(set_bpf_both_parameters, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, bpf, [cutoff=1000.0], Effect),
    sampler_effect_set_parameters(Effect, [cutoff=3000.0, order=8]),
    sampler_sound_effects(Sound, [effect(bpf, _, Params)]),
    memberchk(cutoff=Cutoff, Params),
    memberchk(order=Order, Params),
    abs(Cutoff - 3000.0) < 0.001,
    Order =:= 8,
    sampler_sound_unload(Sound).

% Delay tests

test(attach_delay, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_delay(Sound, 22050, 0.5, 0.8, Effect),
    Effect = effect(Sound, Ptr),
    integer(Ptr),
    sampler_sound_unload(Sound).

test(attach_delay_generic, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, delay, [delay_in_frames=44100, decay=0.3, wet=0.7, dry=0.9], Effect),
    Effect = effect(Sound, Ptr),
    integer(Ptr),
    sampler_sound_unload(Sound).

test(query_delay, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, delay, [delay_in_frames=22050, decay=0.4, wet=0.6, dry=0.8], _),
    sampler_sound_effects(Sound, [effect(delay, Ptr, Params)]),
    integer(Ptr),
    memberchk(delay_in_frames=DelayInFrames, Params),
    memberchk(decay=Decay, Params),
    memberchk(wet=Wet, Params),
    memberchk(dry=Dry, Params),
    DelayInFrames =:= 22050,
    abs(Decay - 0.4) < 0.001,
    abs(Wet - 0.6) < 0.001,
    abs(Dry - 0.8) < 0.001,
    sampler_sound_unload(Sound).

test(set_delay_wet, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, delay, [delay_in_frames=22050], Effect),
    sampler_effect_set_parameters(Effect, [wet=0.5]),
    sampler_sound_effects(Sound, [effect(delay, _, Params)]),
    memberchk(wet=Wet, Params),
    abs(Wet - 0.5) < 0.001,
    sampler_sound_unload(Sound).

test(set_delay_dry, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, delay, [delay_in_frames=22050], Effect),
    sampler_effect_set_parameters(Effect, [dry=0.3]),
    sampler_sound_effects(Sound, [effect(delay, _, Params)]),
    memberchk(dry=Dry, Params),
    abs(Dry - 0.3) < 0.001,
    sampler_sound_unload(Sound).

test(set_delay_decay, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, delay, [delay_in_frames=22050, decay=0.0], Effect),
    sampler_effect_set_parameters(Effect, [decay=0.7]),
    sampler_sound_effects(Sound, [effect(delay, _, Params)]),
    memberchk(decay=Decay, Params),
    abs(Decay - 0.7) < 0.001,
    sampler_sound_unload(Sound).

test(set_delay_multiple_parameters, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, delay, [delay_in_frames=22050], Effect),
    sampler_effect_set_parameters(Effect, [wet=0.4, dry=0.6, decay=0.5]),
    sampler_sound_effects(Sound, [effect(delay, _, Params)]),
    memberchk(wet=Wet, Params),
    memberchk(dry=Dry, Params),
    memberchk(decay=Decay, Params),
    abs(Wet - 0.4) < 0.001,
    abs(Dry - 0.6) < 0.001,
    abs(Decay - 0.5) < 0.001,
    sampler_sound_unload(Sound).

test(set_delay_in_frames_error, [error(permission_error(modify, delay_in_frames, _))]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, delay, [delay_in_frames=22050], Effect),
    sampler_effect_set_parameters(Effect, [delay_in_frames=44100]),
    sampler_sound_unload(Sound).

% Ping-pong delay tests

test(attach_ping_pong_delay, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_ping_pong_delay(Sound, 44100, 22050, 0.5, 0.8, Effect),
    Effect = effect(Sound, Ptr),
    integer(Ptr),
    sampler_sound_unload(Sound).

test(attach_ping_pong_delay_generic, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100, delay_in_frames=22050, feedback=0.5, wet=0.7, dry=0.9], Effect),
    Effect = effect(Sound, Ptr),
    integer(Ptr),
    sampler_sound_unload(Sound).

test(query_ping_pong_delay, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100, delay_in_frames=22050, feedback=0.4, wet=0.6, dry=0.8], _),
    sampler_sound_effects(Sound, [effect(ping_pong_delay, Ptr, Params)]),
    integer(Ptr),
    memberchk(max_delay_in_frames=MaxDelayInFrames, Params),
    memberchk(delay_in_frames=DelayInFrames, Params),
    memberchk(feedback=Feedback, Params),
    memberchk(wet=Wet, Params),
    memberchk(dry=Dry, Params),
    MaxDelayInFrames =:= 44100,
    DelayInFrames =:= 22050,
    abs(Feedback - 0.4) < 0.001,
    abs(Wet - 0.6) < 0.001,
    abs(Dry - 0.8) < 0.001,
    sampler_sound_unload(Sound).

test(set_ping_pong_delay_wet, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100], Effect),
    sampler_effect_set_parameters(Effect, [wet=0.5]),
    sampler_sound_effects(Sound, [effect(ping_pong_delay, _, Params)]),
    memberchk(wet=Wet, Params),
    abs(Wet - 0.5) < 0.001,
    sampler_sound_unload(Sound).

test(set_ping_pong_delay_dry, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100], Effect),
    sampler_effect_set_parameters(Effect, [dry=0.3]),
    sampler_sound_effects(Sound, [effect(ping_pong_delay, _, Params)]),
    memberchk(dry=Dry, Params),
    abs(Dry - 0.3) < 0.001,
    sampler_sound_unload(Sound).

test(set_ping_pong_delay_feedback, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100, feedback=0.0], Effect),
    sampler_effect_set_parameters(Effect, [feedback=0.7]),
    sampler_sound_effects(Sound, [effect(ping_pong_delay, _, Params)]),
    memberchk(feedback=Feedback, Params),
    abs(Feedback - 0.7) < 0.001,
    sampler_sound_unload(Sound).

test(set_ping_pong_delay_delay_in_frames, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100, delay_in_frames=22050, smoothing_mode=0], Effect),
    sampler_effect_set_parameters(Effect, [delay_in_frames=11025]),
    sampler_sound_effects(Sound, [effect(ping_pong_delay, _, Params)]),
    memberchk(delay_in_frames=DelayInFrames, Params),
    DelayInFrames =:= 11025,
    sampler_sound_unload(Sound).

test(set_ping_pong_delay_multiple_parameters, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100, smoothing_mode=0], Effect),
    sampler_effect_set_parameters(Effect, [wet=0.4, dry=0.6, feedback=0.5, delay_in_frames=11025]),
    sampler_sound_effects(Sound, [effect(ping_pong_delay, _, Params)]),
    memberchk(wet=Wet, Params),
    memberchk(dry=Dry, Params),
    memberchk(feedback=Feedback, Params),
    memberchk(delay_in_frames=DelayInFrames, Params),
    abs(Wet - 0.4) < 0.001,
    abs(Dry - 0.6) < 0.001,
    abs(Feedback - 0.5) < 0.001,
    DelayInFrames =:= 11025,
    sampler_sound_unload(Sound).

test(set_ping_pong_delay_max_delay_error, [error(permission_error(modify, max_delay_in_frames, _))]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100], Effect),
    sampler_effect_set_parameters(Effect, [max_delay_in_frames=88200]),
    sampler_sound_unload(Sound).

test(set_ping_pong_delay_delay_out_of_range, [error(domain_error(delay_in_range, _))]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100, delay_in_frames=22050], Effect),
    sampler_effect_set_parameters(Effect, [delay_in_frames=88200]),
    sampler_sound_unload(Sound).

test(query_ping_pong_delay_smoothing_defaults, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100], _),
    sampler_sound_effects(Sound, [effect(ping_pong_delay, _, Params)]),
    memberchk(smoothing_mode=SmoothingMode, Params),
    memberchk(smoothing_speed=SmoothingSpeed, Params),
    memberchk(crossfade_length=CrossfadeLength, Params),
    SmoothingMode =:= 1,  /* default is pitch-shift mode */
    abs(SmoothingSpeed - 1.0) < 0.001,
    CrossfadeLength =:= 128,
    sampler_sound_unload(Sound).

test(attach_ping_pong_delay_with_smoothing, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100, smoothing_mode=2, smoothing_speed=0.5, crossfade_length=2048], _),
    sampler_sound_effects(Sound, [effect(ping_pong_delay, _, Params)]),
    memberchk(smoothing_mode=SmoothingMode, Params),
    memberchk(smoothing_speed=SmoothingSpeed, Params),
    memberchk(crossfade_length=CrossfadeLength, Params),
    SmoothingMode =:= 2,
    abs(SmoothingSpeed - 0.5) < 0.001,
    CrossfadeLength =:= 2048,
    sampler_sound_unload(Sound).

test(set_ping_pong_delay_smoothing_mode, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100], Effect),
    sampler_effect_set_parameters(Effect, [smoothing_mode=2]),
    sampler_sound_effects(Sound, [effect(ping_pong_delay, _, Params)]),
    memberchk(smoothing_mode=SmoothingMode, Params),
    SmoothingMode =:= 2,
    sampler_sound_unload(Sound).

test(set_ping_pong_delay_smoothing_speed, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100], Effect),
    sampler_effect_set_parameters(Effect, [smoothing_speed=0.25]),
    sampler_sound_effects(Sound, [effect(ping_pong_delay, _, Params)]),
    memberchk(smoothing_speed=SmoothingSpeed, Params),
    abs(SmoothingSpeed - 0.25) < 0.001,
    sampler_sound_unload(Sound).

test(set_ping_pong_delay_crossfade_length, [nondet]) :-
    sampler_sound_load('audio/counting.wav', Sound),
    sampler_sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100], Effect),
    sampler_effect_set_parameters(Effect, [crossfade_length=4096]),
    sampler_sound_effects(Sound, [effect(ping_pong_delay, _, Params)]),
    memberchk(crossfade_length=CrossfadeLength, Params),
    CrossfadeLength =:= 4096,
    sampler_sound_unload(Sound).

:- end_tests(effects).
