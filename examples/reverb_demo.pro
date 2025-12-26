:- use_module('src/prolog/promini.pro').

demo_reverb :-
    format('Loading sound...~n'),
    sound_load('audio/counting.wav', Sound),

    format('Playing original sound...~n'),
    sound_start(Sound),
    sleep(4),
    sound_stop(Sound),

    format('Attaching plate reverb (default settings)...~n'),
    sound_attach_reverb(Sound, [wet=0.5, dry=0.5], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(5),
    sound_stop(Sound),

    format('Clearing and adding hall reverb (longer decay, more predelay)...~n'),
    clear_effects(Sound),
    sound_attach_reverb(Sound, [decay=0.8, predelay_ms=50.0, damping=0.3, wet=0.4], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),

    format('Clearing and adding ambient reverb (very long decay, high diffusion)...~n'),
    clear_effects(Sound),
    sound_attach_reverb(Sound, [decay=0.95, predelay_ms=80.0, damping=0.2, wet=0.5, width=1.5], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(8),
    sound_stop(Sound),

    format('Clearing and adding shimmer reverb (octave up)...~n'),
    clear_effects(Sound),
    sound_attach_reverb(Sound, [decay=0.85, shimmer1_shift=12.0, shimmer1_mix=0.3, wet=0.5, damping=0.4], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(8),
    sound_stop(Sound),

    sound_unload(Sound),
    format('Reverb demo complete.~n').

demo_reverb_dynamic :-
    format('Loading sound...~n'),
    sound_load('audio/counting.wav', Sound),

    format('Playing with dynamic reverb parameters...~n'),
    sound_attach_reverb(Sound, [decay=0.5, wet=0.3], Effect),
    sound_loop(Sound),
    sound_start(Sound),

    format('Increasing decay from 0.5 to 0.95...~n'),
    sweep_param(Effect, decay, 0.5, 0.95, 20),

    sleep(2),

    format('Increasing wet mix from 0.3 to 0.7...~n'),
    sweep_param(Effect, wet, 0.3, 0.7, 20),

    sleep(2),

    format('Adding shimmer...~n'),
    sweep_param(Effect, shimmer1_mix, 0.0, 0.4, 20),

    sleep(3),
    sound_stop(Sound),
    sound_unload(Sound),
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
    effect_set_parameters(Effect, [Pair]),
    sleep(0.15),
    Next is Current + Step,
    sweep_param_(Effect, Param, Next, Target, Step).

demo_presets :-
    format('Loading sound...~n'),
    sound_load('audio/gong.wav', Sound),

    format('~n=== Plate Reverb ===~n'),
    sound_attach_reverb(Sound, [
        predelay_ms=10.0, decay=0.5, damping=0.5,
        bandwidth=0.9, wet=0.3
    ], _),
    sound_start(Sound),
    sleep(5),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Hall Reverb ===~n'),
    sound_attach_reverb(Sound, [
        predelay_ms=40.0, decay=0.75, damping=0.3,
        bandwidth=0.8, width=1.2, low_cut=100.0, wet=0.35
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Cathedral Reverb ===~n'),
    sound_attach_reverb(Sound, [
        predelay_ms=60.0, decay=0.9, damping=0.2,
        bandwidth=0.7, width=1.5, low_cut=120.0, wet=0.4
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(8),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Shimmer Reverb ===~n'),
    sound_attach_reverb(Sound, [
        predelay_ms=30.0, decay=0.85, damping=0.4,
        shimmer1_shift=12.0, shimmer1_mix=0.25,
        wet=0.45
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(8),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Ambient/Pad Reverb ===~n'),
    sound_attach_reverb(Sound, [
        predelay_ms=80.0, decay=0.95, damping=0.15,
        bandwidth=0.6,
        mod_depth=0.8, width=2.0, low_cut=150.0, wet=0.5
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(10),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Blackhole-style Reverb ===~n'),
    sound_attach_reverb(Sound, [
        predelay_ms=100.0, decay=0.99, damping=0.1,
        bandwidth=0.5,
        shimmer1_shift=12.0, shimmer1_mix=0.2,
        shimmer2_shift=19.0, shimmer2_mix=0.15,
        mod_depth=1.0, width=2.0, low_cut=150.0, wet=0.6
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(12),
    sound_stop(Sound),
    clear_effects(Sound),

    sound_unload(Sound),
    format('~nPresets demo complete.~n').

demo_shimmer :-
    format('Loading sound...~n'),
    sound_load('audio/counting.wav', Sound),

    format('~n=== Dry sound ===~n'),
    sound_start(Sound),
    sleep(4),
    sound_stop(Sound),

    format('~n=== Reverb only (no shimmer) ===~n'),
    sound_attach_reverb(Sound, [
        decay=0.8, wet=0.5, dry=0.5
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(5),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Subtle shimmer (+12 semitones, low mix) ===~n'),
    sound_attach_reverb(Sound, [
        decay=0.8, wet=0.5, dry=0.5,
        shimmer1_shift=12.0, shimmer1_mix=0.2
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Strong shimmer (+12 semitones, high mix) ===~n'),
    sound_attach_reverb(Sound, [
        decay=0.85, wet=0.5, dry=0.5,
        shimmer1_shift=12.0, shimmer1_mix=0.5
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Fifth up shimmer (+7 semitones) ===~n'),
    sound_attach_reverb(Sound, [
        decay=0.85, wet=0.5, dry=0.5,
        shimmer1_shift=7.0, shimmer1_mix=0.4
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Dual shimmer (octave + fifth) ===~n'),
    sound_attach_reverb(Sound, [
        decay=0.9, wet=0.5, dry=0.5,
        shimmer1_shift=12.0, shimmer1_mix=0.3,
        shimmer2_shift=19.0, shimmer2_mix=0.2
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(8),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Downward shimmer (-12 semitones) ===~n'),
    sound_attach_reverb(Sound, [
        decay=0.85, wet=0.5, dry=0.5,
        shimmer1_shift=(-12.0), shimmer1_mix=0.4
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),

    sound_unload(Sound),
    format('~nShimmer demo complete.~n').

demo_filtering :-
    format('Loading sound...~n'),
    sound_load('audio/counting.wav', Sound),

    format('~n=== Dry sound (reference) ===~n'),
    sound_start(Sound),
    sleep(4),
    sound_stop(Sound),

    format('~n=== Bright reverb (high bandwidth, low damping) ===~n'),
    sound_attach_reverb(Sound, [
        bandwidth=1.0, damping=0.1, decay=0.7,
        wet=0.8, dry=0.2
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(5),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Dark reverb (low bandwidth, high damping) ===~n'),
    sound_attach_reverb(Sound, [
        bandwidth=0.3, damping=0.8, decay=0.7,
        wet=0.8, dry=0.2
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(5),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Very dark (bandwidth 0.1, damping 0.95) ===~n'),
    sound_attach_reverb(Sound, [
        bandwidth=0.1, damping=0.95, decay=0.8,
        wet=0.9, dry=0.1
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Bright input, dark tail (high bandwidth, high damping) ===~n'),
    sound_attach_reverb(Sound, [
        bandwidth=0.95, damping=0.7, decay=0.85,
        wet=0.8, dry=0.2
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Dark input, bright tail (low bandwidth, low damping) ===~n'),
    sound_attach_reverb(Sound, [
        bandwidth=0.3, damping=0.15, decay=0.85,
        wet=0.8, dry=0.2
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),

    sound_unload(Sound),
    format('~nFiltering demo complete.~n').

demo_low_cut :-
    format('Loading sound...~n'),
    sound_load('audio/gong.wav', Sound),

    format('~n=== Very low low_cut (20 Hz) - full bass ===~n'),
    sound_attach_reverb(Sound, [
        decay=0.9, low_cut=20.0, wet=0.5, dry=0.5
    ], _),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Default low_cut (80 Hz) ===~n'),
    sound_attach_reverb(Sound, [
        decay=0.9, low_cut=80.0, wet=0.5, dry=0.5
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Higher low_cut (150 Hz) - tighter bass ===~n'),
    sound_attach_reverb(Sound, [
        decay=0.9, low_cut=150.0, wet=0.5, dry=0.5
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== High low_cut (250 Hz) - thin reverb ===~n'),
    sound_attach_reverb(Sound, [
        decay=0.9, low_cut=250.0, wet=0.5, dry=0.5
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),

    sound_unload(Sound),
    format('~nLow cut demo complete.~n').

demo_width :-
    format('~n=== Mono reverb (width=0) ===~n'),
    sound_load('audio/gong.wav', Sound1),
    sound_attach_reverb(Sound1, [
        decay=0.8, width=0.0, wet=0.5, dry=0.5
    ], _),
    sound_start(Sound1),
    sleep(5),
    sound_stop(Sound1),

    format('~n=== Normal stereo (width=1) ===~n'),
    sound_load('audio/gong.wav', Sound2),
    sound_attach_reverb(Sound2, [
        decay=0.8, width=1.0, wet=0.5, dry=0.5
    ], _),
    sound_start(Sound2),
    sleep(5),
    sound_stop(Sound2),

    format('~n=== Wide stereo (width=1.5, low_cut=120) ===~n'),
    sound_load('audio/gong.wav', Sound3),
    sound_attach_reverb(Sound3, [
        decay=0.8, width=1.5, low_cut=120.0, wet=0.5, dry=0.5
    ], _),
    sound_start(Sound3),
    sleep(5),
    sound_stop(Sound3),

    format('~n=== Extra wide stereo (width=2, low_cut=150) ===~n'),
    sound_load('audio/gong.wav', Sound4),
    sound_attach_reverb(Sound4, [
        decay=0.8, width=2.0, low_cut=150.0, wet=0.5, dry=0.5
    ], _),
    sound_start(Sound4),
    sleep(5),
    sound_stop(Sound4),

    sound_unload(Sound1),
    sound_unload(Sound2),
    sound_unload(Sound3),
    sound_unload(Sound4),
    format('~nWidth demo complete.~n').

demo_size :-
    format('Loading sound...~n'),
    sound_load('audio/gong.wav', Sound),

    format('~n=== Small room (size=0.5) ===~n'),
    sound_attach_reverb(Sound, [
        decay=0.7, size=0.5, wet=0.5, dry=0.5
    ], _),
    sound_start(Sound),
    sleep(5),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Default room (size=1.0) ===~n'),
    sound_attach_reverb(Sound, [
        decay=0.7, size=1.0, wet=0.5, dry=0.5
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(5),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Large room (size=1.5) ===~n'),
    sound_attach_reverb(Sound, [
        decay=0.7, size=1.5, wet=0.5, dry=0.5
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),
    clear_effects(Sound),

    format('~n=== Huge room (size=2.0) ===~n'),
    sound_attach_reverb(Sound, [
        decay=0.7, size=2.0, wet=0.5, dry=0.5
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(7),
    sound_stop(Sound),

    sound_unload(Sound),
    format('~nSize demo complete.~n').

demo_size_sweep :-
    format('Loading sound...~n'),
    sound_load('audio/gong.wav', Sound),

    format('Sweeping room size from 0.3 to 2.0...~n'),
    sound_attach_reverb(Sound, [decay=0.8, size=0.3, wet=0.5], Effect),
    sound_loop(Sound),
    sound_start(Sound),

    sweep_param(Effect, size, 0.3, 2.0, 40),

    sleep(2),

    format('Sweeping room size back down...~n'),
    sweep_param(Effect, size, 2.0, 0.3, 40),

    sleep(1),
    sound_stop(Sound),
    sound_unload(Sound),
    format('~nSize sweep demo complete.~n').

demo_hp :-
    format('Loading sound...~n'),
    sound_load('audio/gong.wav', Sound),
    sound_attach_reverb(Sound, [
        decay=0.85, hp=0.0, wet=1.0, dry=0.0
    ], Effect),

    format('~n=== No tank HP (hp=0.0) - full bass in tail ===~n'),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),

    format('~n=== Light tank HP (hp=0.3) - tighter bass ===~n'),
    effect_set_parameters(Effect, [hp=0.3]),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),

    format('~n=== Medium tank HP (hp=0.5) - bass decays faster ===~n'),
    effect_set_parameters(Effect, [hp=0.5]),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),

    format('~n=== Strong tank HP (hp=0.7) - thin reverb tail ===~n'),
    effect_set_parameters(Effect, [hp=0.7]),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),

    sound_unload(Sound),
    format('~nTank HP demo complete.~n').

demo_freeze :-
    format('Loading sound...~n'),
    sound_load('audio/guitar.wav', Sound),
    sound_attach_reverb(Sound, [
        decay=0.5, wet=1.0, dry=0.0
    ], Effect),

    format('~n=== Playing with reverb ===~n'),
    sound_start(Sound),
    sleep(4),

    format('~n=== Freezing reverb (infinite sustain) ===~n'),
    effect_set_parameters(Effect, [freeze=true]),
    sleep(20),

    format('~n=== Unfreezing reverb ===~n'),
    effect_set_parameters(Effect, [freeze=false]),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(4),
    sound_stop(Sound),

    sound_unload(Sound),
    format('~nFreeze demo complete.~n').

demo_darkening :-
    format('Loading sound...~n'),
    sound_load('audio/guitar.wav', Sound),
    sound_attach_reverb(Sound, [
        decay=0.95, damping=0.7, wet=1.0, dry=0.0
    ], _),

    format('~n=== Long decay with gradual darkening ===~n'),
    format('High decay (0.95) + high damping (0.7)~n'),
    format('Playing briefly, then letting tail decay...~n'),
    sound_start(Sound),
    sleep(2),
    sound_stop(Sound),
    format('~n=== Listening to reverb tail darken ===~n'),
    sleep(15),

    sound_unload(Sound),
    format('~nDarkening demo complete.~n').

demo_freeze_keyboard :-
    promini_init,
    control_init,

    format('Loading sound and freezing reverb...~n'),
    sound_load('audio/guitar.wav', Sound),
    sound_attach_reverb(Sound, [
        decay=0.7, damping=0.4, wet=1.0, dry=0.0
    ], Effect),

    /* play guitar briefly then freeze */
    sound_start(Sound),
    sleep(3),
    effect_set_parameters(Effect, [freeze=true]),
    format('~n=== Reverb frozen - play synth over the pad ===~n'),

    /* create 4 pad voices with soft envelope */
    create_pad_voice(V1, E1),
    create_pad_voice(V2, E2),
    create_pad_voice(V3, E3),
    create_pad_voice(V4, E4),

    synth_voice_start(V1),
    synth_voice_start(V2),
    synth_voice_start(V3),
    synth_voice_start(V4),

    /* summing node with light reverb for synth */
    summing_node_init(Sum),
    summing_node_connect(Sum, V1),
    summing_node_connect(Sum, V2),
    summing_node_connect(Sum, V3),
    summing_node_connect(Sum, V4),
    summing_node_attach_effect(Sum, reverb, [wet=0.25, decay=0.6, damping=0.5], _),

    /* keyboard with pentatonic scale */
    keyboard_init(K),
    keyboard_row_add_voice(K, 0, V1, [E1]),
    keyboard_row_add_voice(K, 0, V2, [E2]),
    keyboard_row_add_voice(K, 0, V3, [E3]),
    keyboard_row_add_voice(K, 0, V4, [E4]),
    keyboard_row_set(K, 0, [mode=[0.0, 2.0, 4.0, 7.0, 9.0], octave=0]),

    format('Freeze keyboard:~n'),
    format('  Frozen guitar reverb as pad~n'),
    format('  Row 0 (1-0): Synth, pentatonic~n'),
    format('  ESC to exit~n').

/* helper: create a pad voice with soft attack and longer release */
create_pad_voice(Voice, Envelope) :-
    synth_voice_init(Voice),
    /* fundamental + soft harmonics */
    synth_oscillator_add(Voice, 220.0, 0.0, O1),
    synth_oscillator_add(Voice, 440.0, 0.25, O2),
    synth_oscillator_add(Voice, 660.0, 0.5, O3),
    synth_oscillator_set_volume(O1, 0.35),
    synth_oscillator_set_volume(O2, 0.15),
    synth_oscillator_set_volume(O3, 0.05),
    /* VCA for envelope control */
    voice_attach_effect(Voice, vca, [gain=0.0], Vca),
    /* soft pad envelope: slow attack, long release */
    mod_envelope_init(0.3, 0.2, 0.4, 0.7, 0.6, 800.0, false, Envelope),
    mod_route_init(Envelope, vca, Vca, gain, absolute, 1.0, 0.0, 0.0, _).

demo_shimmer_in_loop :-
    format('Loading sound...~n'),
    sound_load('audio/guitar.wav', Sound),

    format('~n=== Post-tank shimmer (default) ===~n'),
    format('Single-pass pitch shift on wet output~n'),
    sound_attach_reverb(Sound, [
        decay=0.85, damping=0.4, wet=0.7, dry=0.3,
        shimmer1_shift=12.0, shimmer1_mix=0.4
    ], _),
    sound_start(Sound),
    sleep(2),
    sound_stop(Sound),
    format('Letting tail ring out...~n'),
    sleep(8),
    clear_effects(Sound),

    format('~n=== In-loop shimmer (Oliverb-style) ===~n'),
    format('Pitch shift in feedback path - accumulates each pass~n'),
    sound_attach_reverb(Sound, [
        decay=0.85, damping=0.4, wet=0.7, dry=0.3,
        shimmer1_shift=12.0, shimmer1_mix=0.4,
        shimmer_in_loop=true
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(2),
    sound_stop(Sound),
    format('Letting tail ring out...~n'),
    sleep(8),
    clear_effects(Sound),

    format('~n=== In-loop with fifth (+7 semitones) ===~n'),
    format('Non-octave interval creates harmonic cloud~n'),
    sound_attach_reverb(Sound, [
        decay=0.9, damping=0.5, wet=0.8, dry=0.2,
        shimmer1_shift=7.0, shimmer1_mix=0.35,
        shimmer_in_loop=true
    ], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(2),
    sound_stop(Sound),
    format('Letting tail ring out...~n'),
    sleep(10),
    clear_effects(Sound),

    sound_unload(Sound),
    format('~nIn-loop shimmer demo complete.~n').

demo_shimmer_freeze :-
    format('Loading sound...~n'),
    sound_load('audio/guitar.wav', Sound),

    format('~n=== In-loop fifth, frozen ===~n'),
    format('Cascading fifths up, then freeze~n'),
    sound_attach_reverb(Sound, [
        decay=0.9, damping=0.5, wet=1.0, dry=0.0,
        shimmer1_shift=7.0, shimmer1_mix=0.35,
        shimmer_in_loop=true
    ], Effect),
    sound_start(Sound),
    sleep(2),
    sound_stop(Sound),
    sleep(2),
    format('Freezing tail...~n'),
    effect_set_parameters(Effect, [freeze=true]),
    sleep(15),

    sound_unload(Sound),
    format('~nShimmer freeze demo complete.~n').
