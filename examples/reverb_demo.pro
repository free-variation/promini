:- use_module('src/prolog/sampler.pro').

demo_reverb :-
    format('Loading sound...~n'),
    sampler_sound_load('audio/counting.wav', Sound),

    format('Playing original sound...~n'),
    sampler_sound_start(Sound),
    sleep(4),
    sampler_sound_stop(Sound),

    format('Attaching plate reverb (default settings)...~n'),
    sampler_sound_attach_reverb(Sound, [wet=0.5, dry=0.5], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(5),
    sampler_sound_stop(Sound),

    format('Clearing and adding hall reverb (longer decay, more predelay)...~n'),
    sampler_sound_clear_effects(Sound),
    sampler_sound_attach_reverb(Sound, [decay=0.8, predelay_ms=50.0, damping=0.3, wet=0.4], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(6),
    sampler_sound_stop(Sound),

    format('Clearing and adding ambient reverb (very long decay, high diffusion)...~n'),
    sampler_sound_clear_effects(Sound),
    sampler_sound_attach_reverb(Sound, [decay=0.95, predelay_ms=80.0, damping=0.2, wet=0.5, width=1.5], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(8),
    sampler_sound_stop(Sound),

    format('Clearing and adding shimmer reverb (octave up)...~n'),
    sampler_sound_clear_effects(Sound),
    sampler_sound_attach_reverb(Sound, [decay=0.85, shimmer1_shift=12.0, shimmer1_mix=0.3, wet=0.5, damping=0.4], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(8),
    sampler_sound_stop(Sound),

    sampler_sound_unload(Sound),
    format('Reverb demo complete.~n').

demo_reverb_dynamic :-
    format('Loading sound...~n'),
    sampler_sound_load('audio/counting.wav', Sound),

    format('Playing with dynamic reverb parameters...~n'),
    sampler_sound_attach_reverb(Sound, [decay=0.5, wet=0.3], Effect),
    sampler_sound_loop(Sound),
    sampler_sound_start(Sound),

    format('Increasing decay from 0.5 to 0.95...~n'),
    sweep_param(Effect, decay, 0.5, 0.95, 20),

    sleep(2),

    format('Increasing wet mix from 0.3 to 0.7...~n'),
    sweep_param(Effect, wet, 0.3, 0.7, 20),

    sleep(2),

    format('Adding shimmer...~n'),
    sweep_param(Effect, shimmer1_mix, 0.0, 0.4, 20),

    sleep(3),
    sampler_sound_stop(Sound),
    sampler_sound_unload(Sound),
    format('Dynamic reverb demo complete.~n').

sweep_param(Effect, Param, Start, Target, Steps) :-
    Step is (Target - Start) / Steps,
    sweep_param_(Effect, Param, Start, Target, Step).

sweep_param_(_, _, Current, Target, Step) :-
    Step > 0,
    Current >= Target, !.
sweep_param_(_, _, Current, Target, Step) :-
    Step < 0,
    Current =< Target, !.
sweep_param_(Effect, Param, Current, Target, Step) :-
    Pair =.. [=, Param, Current],
    sampler_effect_set_parameters(Effect, [Pair]),
    sleep(0.15),
    Next is Current + Step,
    sweep_param_(Effect, Param, Next, Target, Step).

demo_presets :-
    format('Loading sound...~n'),
    sampler_sound_load('audio/gong.wav', Sound),

    format('~n=== Plate Reverb ===~n'),
    sampler_sound_attach_reverb(Sound, [
        predelay_ms=10.0, decay=0.5, damping=0.5,
        bandwidth=0.9, wet=0.3
    ], _),
    sampler_sound_start(Sound),
    sleep(5),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Hall Reverb ===~n'),
    sampler_sound_attach_reverb(Sound, [
        predelay_ms=40.0, decay=0.75, damping=0.3,
        bandwidth=0.8, width=1.2, low_cut=100.0, wet=0.35
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(6),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Cathedral Reverb ===~n'),
    sampler_sound_attach_reverb(Sound, [
        predelay_ms=60.0, decay=0.9, damping=0.2,
        bandwidth=0.7, width=1.5, low_cut=120.0, wet=0.4
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(8),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Shimmer Reverb ===~n'),
    sampler_sound_attach_reverb(Sound, [
        predelay_ms=30.0, decay=0.85, damping=0.4,
        shimmer1_shift=12.0, shimmer1_mix=0.25,
        wet=0.45
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(8),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Ambient/Pad Reverb ===~n'),
    sampler_sound_attach_reverb(Sound, [
        predelay_ms=80.0, decay=0.95, damping=0.15,
        bandwidth=0.6,
        mod_depth=0.8, width=2.0, low_cut=150.0, wet=0.5
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(10),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Blackhole-style Reverb ===~n'),
    sampler_sound_attach_reverb(Sound, [
        predelay_ms=100.0, decay=0.99, damping=0.1,
        bandwidth=0.5,
        shimmer1_shift=12.0, shimmer1_mix=0.2,
        shimmer2_shift=19.0, shimmer2_mix=0.15,
        mod_depth=1.0, width=2.0, low_cut=150.0, wet=0.6
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(12),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    sampler_sound_unload(Sound),
    format('~nPresets demo complete.~n').

demo_shimmer :-
    format('Loading sound...~n'),
    sampler_sound_load('audio/counting.wav', Sound),

    format('~n=== Dry sound ===~n'),
    sampler_sound_start(Sound),
    sleep(4),
    sampler_sound_stop(Sound),

    format('~n=== Reverb only (no shimmer) ===~n'),
    sampler_sound_attach_reverb(Sound, [
        decay=0.8, wet=0.5, dry=0.5
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(5),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Subtle shimmer (+12 semitones, low mix) ===~n'),
    sampler_sound_attach_reverb(Sound, [
        decay=0.8, wet=0.5, dry=0.5,
        shimmer1_shift=12.0, shimmer1_mix=0.2
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(6),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Strong shimmer (+12 semitones, high mix) ===~n'),
    sampler_sound_attach_reverb(Sound, [
        decay=0.85, wet=0.5, dry=0.5,
        shimmer1_shift=12.0, shimmer1_mix=0.5
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(6),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Fifth up shimmer (+7 semitones) ===~n'),
    sampler_sound_attach_reverb(Sound, [
        decay=0.85, wet=0.5, dry=0.5,
        shimmer1_shift=7.0, shimmer1_mix=0.4
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(6),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Dual shimmer (octave + fifth) ===~n'),
    sampler_sound_attach_reverb(Sound, [
        decay=0.9, wet=0.5, dry=0.5,
        shimmer1_shift=12.0, shimmer1_mix=0.3,
        shimmer2_shift=19.0, shimmer2_mix=0.2
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(8),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Downward shimmer (-12 semitones) ===~n'),
    sampler_sound_attach_reverb(Sound, [
        decay=0.85, wet=0.5, dry=0.5,
        shimmer1_shift=(-12.0), shimmer1_mix=0.4
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(6),
    sampler_sound_stop(Sound),

    sampler_sound_unload(Sound),
    format('~nShimmer demo complete.~n').

demo_filtering :-
    format('Loading sound...~n'),
    sampler_sound_load('audio/counting.wav', Sound),

    format('~n=== Dry sound (reference) ===~n'),
    sampler_sound_start(Sound),
    sleep(4),
    sampler_sound_stop(Sound),

    format('~n=== Bright reverb (high bandwidth, low damping) ===~n'),
    sampler_sound_attach_reverb(Sound, [
        bandwidth=1.0, damping=0.1, decay=0.7,
        wet=0.8, dry=0.2
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(5),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Dark reverb (low bandwidth, high damping) ===~n'),
    sampler_sound_attach_reverb(Sound, [
        bandwidth=0.3, damping=0.8, decay=0.7,
        wet=0.8, dry=0.2
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(5),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Very dark (bandwidth 0.1, damping 0.95) ===~n'),
    sampler_sound_attach_reverb(Sound, [
        bandwidth=0.1, damping=0.95, decay=0.8,
        wet=0.9, dry=0.1
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(6),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Bright input, dark tail (high bandwidth, high damping) ===~n'),
    sampler_sound_attach_reverb(Sound, [
        bandwidth=0.95, damping=0.7, decay=0.85,
        wet=0.8, dry=0.2
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(6),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Dark input, bright tail (low bandwidth, low damping) ===~n'),
    sampler_sound_attach_reverb(Sound, [
        bandwidth=0.3, damping=0.15, decay=0.85,
        wet=0.8, dry=0.2
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(6),
    sampler_sound_stop(Sound),

    sampler_sound_unload(Sound),
    format('~nFiltering demo complete.~n').

demo_low_cut :-
    format('Loading sound...~n'),
    sampler_sound_load('audio/gong.wav', Sound),

    format('~n=== Very low low_cut (20 Hz) - full bass ===~n'),
    sampler_sound_attach_reverb(Sound, [
        decay=0.9, low_cut=20.0, wet=0.5, dry=0.5
    ], _),
    sampler_sound_start(Sound),
    sleep(6),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Default low_cut (80 Hz) ===~n'),
    sampler_sound_attach_reverb(Sound, [
        decay=0.9, low_cut=80.0, wet=0.5, dry=0.5
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(6),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Higher low_cut (150 Hz) - tighter bass ===~n'),
    sampler_sound_attach_reverb(Sound, [
        decay=0.9, low_cut=150.0, wet=0.5, dry=0.5
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(6),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== High low_cut (250 Hz) - thin reverb ===~n'),
    sampler_sound_attach_reverb(Sound, [
        decay=0.9, low_cut=250.0, wet=0.5, dry=0.5
    ], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(6),
    sampler_sound_stop(Sound),

    sampler_sound_unload(Sound),
    format('~nLow cut demo complete.~n').

demo_width :-
    format('~n=== Mono reverb (width=0) ===~n'),
    sampler_sound_load('audio/gong.wav', Sound1),
    sampler_sound_attach_reverb(Sound1, [
        decay=0.8, width=0.0, wet=0.5, dry=0.5
    ], _),
    sampler_sound_start(Sound1),
    sleep(5),
    sampler_sound_stop(Sound1),

    format('~n=== Normal stereo (width=1) ===~n'),
    sampler_sound_load('audio/gong.wav', Sound2),
    sampler_sound_attach_reverb(Sound2, [
        decay=0.8, width=1.0, wet=0.5, dry=0.5
    ], _),
    sampler_sound_start(Sound2),
    sleep(5),
    sampler_sound_stop(Sound2),

    format('~n=== Wide stereo (width=1.5, low_cut=120) ===~n'),
    sampler_sound_load('audio/gong.wav', Sound3),
    sampler_sound_attach_reverb(Sound3, [
        decay=0.8, width=1.5, low_cut=120.0, wet=0.5, dry=0.5
    ], _),
    sampler_sound_start(Sound3),
    sleep(5),
    sampler_sound_stop(Sound3),

    format('~n=== Extra wide stereo (width=2, low_cut=150) ===~n'),
    sampler_sound_load('audio/gong.wav', Sound4),
    sampler_sound_attach_reverb(Sound4, [
        decay=0.8, width=2.0, low_cut=150.0, wet=0.5, dry=0.5
    ], _),
    sampler_sound_start(Sound4),
    sleep(5),
    sampler_sound_stop(Sound4),

    sampler_sound_unload(Sound1),
    sampler_sound_unload(Sound2),
    sampler_sound_unload(Sound3),
    sampler_sound_unload(Sound4),
    format('~nWidth demo complete.~n').
