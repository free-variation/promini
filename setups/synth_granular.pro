:- module(synth_granular, [setup/0, teardown/0]).
:- use_module('../src/prolog/promini.pro').

/*
 * Synth Granular Setup
 * Combines synth pads with granular processing.
 *
 * Keyboard 1 (synth): D minor pentatonic
 *   Rows 0-1: Broad pad sound (higher octaves)
 *   Rows 2-3: Bass sound (lower octaves)
 *
 * Keyboard 2 (granular): Triggers grains from synth output
 *
 * Gamepad: Controls granular parameters
 *
 * Usage:
 *   load_setup(synth_granular).
 *   unload_setup(synth_granular).
 */

setup :-
    control_init,
    control_gamepads(Gamepads),
    (   Gamepads = [gamepad(Id, Name)|_]
    ->  format('Using gamepad: ~w~n', [Name]),
        control_open(Id, GP)
    ;   format('No gamepad found~n'), fail
    ),
    setup_register(synth_granular:gamepad, GP),

    /* === SYNTH SECTION === */

    /* Pad voices for rows 0-1 (broad pad, higher harmonics) */
    create_pad_voice(PV1, PE1),
    setup_register(synth_granular:pad1, PV1),
    setup_register(synth_granular:pad_env1, PE1),
    create_pad_voice(PV2, PE2),
    setup_register(synth_granular:pad2, PV2),
    setup_register(synth_granular:pad_env2, PE2),
    create_pad_voice(PV3, PE3),
    setup_register(synth_granular:pad3, PV3),
    setup_register(synth_granular:pad_env3, PE3),
    create_pad_voice(PV4, PE4),
    setup_register(synth_granular:pad4, PV4),
    setup_register(synth_granular:pad_env4, PE4),
    create_pad_voice(PV5, PE5),
    setup_register(synth_granular:pad5, PV5),
    setup_register(synth_granular:pad_env5, PE5),
    create_pad_voice(PV6, PE6),
    setup_register(synth_granular:pad6, PV6),
    setup_register(synth_granular:pad_env6, PE6),
    create_pad_voice(PV7, PE7),
    setup_register(synth_granular:pad7, PV7),
    setup_register(synth_granular:pad_env7, PE7),
    create_pad_voice(PV8, PE8),
    setup_register(synth_granular:pad8, PV8),
    setup_register(synth_granular:pad_env8, PE8),
    maplist(synth_voice_start, [PV1, PV2, PV3, PV4, PV5, PV6, PV7, PV8]),

    /* Bass voices for rows 2-3 (fundamental-heavy) */
    create_bass_voice(BV1, BE1),
    setup_register(synth_granular:bass1, BV1),
    setup_register(synth_granular:bass_env1, BE1),
    create_bass_voice(BV2, BE2),
    setup_register(synth_granular:bass2, BV2),
    setup_register(synth_granular:bass_env2, BE2),
    create_bass_voice(BV3, BE3),
    setup_register(synth_granular:bass3, BV3),
    setup_register(synth_granular:bass_env3, BE3),
    create_bass_voice(BV4, BE4),
    setup_register(synth_granular:bass4, BV4),
    setup_register(synth_granular:bass_env4, BE4),
    create_bass_voice(BV5, BE5),
    setup_register(synth_granular:bass5, BV5),
    setup_register(synth_granular:bass_env5, BE5),
    create_bass_voice(BV6, BE6),
    setup_register(synth_granular:bass6, BV6),
    setup_register(synth_granular:bass_env6, BE6),
    create_bass_voice(BV7, BE7),
    setup_register(synth_granular:bass7, BV7),
    setup_register(synth_granular:bass_env7, BE7),
    create_bass_voice(BV8, BE8),
    setup_register(synth_granular:bass8, BV8),
    setup_register(synth_granular:bass_env8, BE8),
    maplist(synth_voice_start, [BV1, BV2, BV3, BV4, BV5, BV6, BV7, BV8]),

    /* Summing node for all synth voices */
    summing_node_init(Sum),
    setup_register(synth_granular:summing, Sum),
    maplist(summing_node_connect(Sum),
            [PV1, PV2, PV3, PV4, PV5, PV6, PV7, PV8,
             BV1, BV2, BV3, BV4, BV5, BV6, BV7, BV8]),

    /* === GRANULAR SECTION === */

    granular_init(4.0, G),
    setup_register(synth_granular:granular, G),

    /* Connect summing node to granular */
    granular_connect(G, Sum),
    granular_set(G, [density=133.3, size=150.0, position=0.5, position_spray=0.3, recording=true]),
    granular_set_mode(G, [0, 7], 1, 4),

    /* Slow LFO for position drift */
    mod_lfo_init(sine, 0.03, PosLFO),
    setup_register(synth_granular:pos_lfo, PosLFO),
    mod_route_init(PosLFO, granular, G, position, absolute, 0.4, 0.5, 0.0, PosLFORoute),
    setup_register(synth_granular:pos_lfo_route, PosLFORoute),

    granular_attach_effect(G, moog, [cutoff=20000.0, resonance=0.0], Moog),
    setup_register(synth_granular:moog, Moog),

    granular_attach_effect(G, reverb, [wet=0.3, decay=0.8, shimmer1_shift=7.0, shimmer1_mix=0.2, shimmer2_shift=19.0, shimmer2_mix=0.05, shimmer_in_loop=true], Rev),
    setup_register(synth_granular:reverb, Rev),

    granular_attach_effect(G, hpf, [cutoff=20.0, order=2], Hpf),
    setup_register(synth_granular:hpf, Hpf),

    /* === GAMEPAD CONTROLS (for granular) === */

    mod_gamepad_init(GP, left_x, LX),
    setup_register(synth_granular:lx, LX),
    mod_gamepad_init(GP, left_y, LY),
    setup_register(synth_granular:ly, LY),
    mod_gamepad_init(GP, right_x, RX),
    setup_register(synth_granular:rx, RX),
    mod_gamepad_init(GP, right_y, RY),
    setup_register(synth_granular:ry, RY),
    mod_gamepad_init(GP, left_trigger, LT),
    setup_register(synth_granular:lt, LT),
    mod_gamepad_init(GP, right_trigger, RT),
    setup_register(synth_granular:rt, RT),

    mod_gamepad_button_init(GP, lb, [0.0, 0.5, 1.0], LB),
    setup_register(synth_granular:lb, LB),
    mod_gamepad_button_init(GP, rb, [0.0, 0.3, 0.7, 1.0], RB),
    setup_register(synth_granular:rb, RB),
    mod_gamepad_button_init(GP, r3, [1.0, 0.7, 0.3, 0.0], R3Btn),
    setup_register(synth_granular:r3btn, R3Btn),
    mod_gamepad_button_init(GP, a, trigger, ABtn),
    setup_register(synth_granular:abtn, ABtn),
    mod_gamepad_button_init(GP, l3, trigger, L3Btn),
    setup_register(synth_granular:l3btn, L3Btn),
    mod_gamepad_button_init(GP, y, toggle, YBtn),
    setup_register(synth_granular:ybtn, YBtn),
    mod_gamepad_button_init(GP, b, momentary, BBtn),
    setup_register(synth_granular:bbtn, BBtn),
    mod_gamepad_button_init(GP, dpad_up, momentary, DUp),
    setup_register(synth_granular:dup, DUp),
    mod_gamepad_button_init(GP, dpad_down, momentary, DDown),
    setup_register(synth_granular:ddown, DDown),
    mod_gamepad_button_init(GP, dpad_left, momentary, DLeft),
    setup_register(synth_granular:dleft, DLeft),
    mod_gamepad_button_init(GP, dpad_right, momentary, DRight),
    setup_register(synth_granular:dright, DRight),
    mod_gamepad_button_init(GP, x, toggle, XBtn),
    setup_register(synth_granular:xbtn, XBtn),
    mod_gamepad_button_init(GP, start, momentary, PlusBtn),
    setup_register(synth_granular:plusbtn, PlusBtn),
    mod_gamepad_button_init(GP, back, momentary, MinusBtn),
    setup_register(synth_granular:minusbtn, MinusBtn),

    /* Gamepad routes to granular */
    mod_route_init(LX, granular, G, density, rate, 10.0, 0.0, 0.0, R1),
    setup_register(synth_granular:r1, R1),
    mod_route_init(LY, granular, G, size, rate, -100.0, 0.0, 0.0, R2),
    setup_register(synth_granular:r2, R2),
    mod_route_init(RX, granular, G, position, rate, 0.5, 0.0, 0.0, R3),
    setup_register(synth_granular:r3, R3),
    mod_route_init(RY, granular, G, position_spray, rate, -0.3, 0.0, 0.0, R4),
    setup_register(synth_granular:r4, R4),
    mod_route_init(LT, granular, G, deviation_up, rate, -10.0, 0.0, 0.0, R5),
    setup_register(synth_granular:r5, R5),
    mod_route_init(RT, granular, G, deviation_up, rate, 10.0, 0.0, 0.0, R6),
    setup_register(synth_granular:r6, R6),
    mod_route_init(LB, granular, G, envelope, absolute, 1.0, 0.0, 0.0, R7),
    setup_register(synth_granular:r7, R7),
    mod_route_init(RB, granular, G, reverse, absolute, 1.0, 0.0, 0.0, R8),
    setup_register(synth_granular:r8, R8),
    mod_route_init(R3Btn, granular, G, regularity, absolute, 1.0, 0.0, 0.0, R9),
    setup_register(synth_granular:r9, R9),
    mod_route_init(ABtn, granular, G, trigger, absolute, 1.0, 0.0, 0.0, R10),
    setup_register(synth_granular:r10, R10),
    mod_route_init(L3Btn, granular, G, trigger, absolute, 1.0, 0.0, 0.0, R11),
    setup_register(synth_granular:r11, R11),
    mod_route_init(BBtn, granular, G, reverse, absolute, 1.0, 0.0, 0.0, R12),
    setup_register(synth_granular:r12, R12),
    mod_route_init(XBtn, granular, G, recording, absolute, -1.0, 1.0, 0.0, R13),
    setup_register(synth_granular:r13, R13),
    mod_route_init(YBtn, reverb, Rev, freeze, absolute, 1.0, 0.0, 0.0, R14),
    setup_register(synth_granular:r14, R14),
    mod_route_init(DUp, reverb, Rev, decay, rate, 0.3, 0.0, 0.0, R15),
    setup_register(synth_granular:r15, R15),
    mod_route_init(DDown, reverb, Rev, decay, rate, -0.3, 0.0, 0.0, R16),
    setup_register(synth_granular:r16, R16),
    mod_route_init(DRight, reverb, Rev, wet, rate, 0.3, 0.0, 0.0, R17),
    setup_register(synth_granular:r17, R17),
    mod_route_init(DLeft, reverb, Rev, wet, rate, -0.3, 0.0, 0.0, R18),
    setup_register(synth_granular:r18, R18),
    mod_route_init(PlusBtn, granular, G, wet, rate, 0.3, 0.0, 0.0, R19),
    setup_register(synth_granular:r19, R19),
    mod_route_init(MinusBtn, granular, G, wet, rate, -0.3, 0.0, 0.0, R20),
    setup_register(synth_granular:r20, R20),

    /* === KEYBOARD 1: SYNTH === */

    keyboard_init(K1),
    setup_register(synth_granular:synth_keyboard, K1),

    /* D minor pentatonic: [0, 3, 5, 7, 10] relative to D */

    /* Row 0: Pad, +1 octave */
    keyboard_row_add_voice(K1, 0, PV1, [PE1]),
    keyboard_row_add_voice(K1, 0, PV2, [PE2]),
    keyboard_row_add_voice(K1, 0, PV3, [PE3]),
    keyboard_row_add_voice(K1, 0, PV4, [PE4]),
    keyboard_row_set(K1, 0, [mode=[0.0, 3.0, 5.0, 7.0, 10.0], octave=1, root=2]),

    /* Row 1: Pad, octave 0 */
    keyboard_row_add_voice(K1, 1, PV5, [PE5]),
    keyboard_row_add_voice(K1, 1, PV6, [PE6]),
    keyboard_row_add_voice(K1, 1, PV7, [PE7]),
    keyboard_row_add_voice(K1, 1, PV8, [PE8]),
    keyboard_row_set(K1, 1, [mode=[0.0, 3.0, 5.0, 7.0, 10.0], octave=0, root=2]),

    /* Row 2: Bass, -1 octave */
    keyboard_row_add_voice(K1, 2, BV1, [BE1]),
    keyboard_row_add_voice(K1, 2, BV2, [BE2]),
    keyboard_row_add_voice(K1, 2, BV3, [BE3]),
    keyboard_row_add_voice(K1, 2, BV4, [BE4]),
    keyboard_row_set(K1, 2, [mode=[0.0, 3.0, 5.0, 7.0, 10.0], octave=(-1), root=2]),

    /* Row 3: Bass, -2 octaves */
    keyboard_row_add_voice(K1, 3, BV5, [BE5]),
    keyboard_row_add_voice(K1, 3, BV6, [BE6]),
    keyboard_row_add_voice(K1, 3, BV7, [BE7]),
    keyboard_row_add_voice(K1, 3, BV8, [BE8]),
    keyboard_row_set(K1, 3, [mode=[0.0, 3.0, 5.0, 7.0, 10.0], octave=(-2), root=2]),

    /* === KEYBOARD 2: GRANULAR === */

    keyboard_init(K2),
    setup_register(synth_granular:granular_keyboard, K2),

    /* Arrow keys for filter control */
    mod_keyboard_init(up, [attack=50, release=100], FilterUp),
    setup_register(synth_granular:filter_up, FilterUp),
    mod_keyboard_init(down, [attack=50, release=100], FilterDown),
    setup_register(synth_granular:filter_down, FilterDown),
    mod_keyboard_init(left, [attack=50, release=100], HpfDown),
    setup_register(synth_granular:hpf_down, HpfDown),
    mod_keyboard_init(right, [attack=50, release=100], HpfUp),
    setup_register(synth_granular:hpf_up, HpfUp),

    mod_route_init(FilterUp, moog, Moog, cutoff, rate, 5000.0, 0.0, 0.0, FilterUpRoute),
    setup_register(synth_granular:filter_up_route, FilterUpRoute),
    mod_route_init(FilterDown, moog, Moog, cutoff, rate, -5000.0, 0.0, 0.0, FilterDownRoute),
    setup_register(synth_granular:filter_down_route, FilterDownRoute),
    mod_route_init(HpfUp, hpf, Hpf, cutoff, rate, 50.0, 0.0, 0.0, HpfUpRoute),
    setup_register(synth_granular:hpf_up_route, HpfUpRoute),
    mod_route_init(HpfDown, hpf, Hpf, cutoff, rate, -50.0, 0.0, 0.0, HpfDownRoute),
    setup_register(synth_granular:hpf_down_route, HpfDownRoute),

    /* Shift keys for row 3 octave control */
    mod_keyboard_init(left_shift, [mode=trigger], LeftShift),
    setup_register(synth_granular:left_shift, LeftShift),
    mod_keyboard_init(right_shift, [mode=trigger], RightShift),
    setup_register(synth_granular:right_shift, RightShift),
    mod_route_init(LeftShift, keyboard_row, keyboard_row(K1, 3), octave, absolute, -1.0, 0.0, 0.0, OctDownRoute),
    setup_register(synth_granular:oct_down_route, OctDownRoute),
    mod_route_init(RightShift, keyboard_row, keyboard_row(K1, 3), octave, absolute, 1.0, 0.0, 0.0, OctUpRoute),
    setup_register(synth_granular:oct_up_route, OctUpRoute),

    /* Space bar toggles position LFO pause */
    mod_keyboard_init(space, [mode=trigger], SpaceKey),
    setup_register(synth_granular:space_key, SpaceKey),
    mod_route_init(SpaceKey, lfo, PosLFO, pause, absolute, 1.0, 0.0, 0.0, PauseLFORoute),
    setup_register(synth_granular:pause_lfo_route, PauseLFORoute),

    /* Enable monitoring */
    maplist(mod_route_monitor,
            [R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15,R16,R17,R18,R19,R20],
            [true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],
            [K2,K2,K2,K2,K2,K2,K2,K2,K2,K2,K2,K2,K2,K2,K2,K2]),
    /* Paired monitoring for gamepad sticks */
    mod_route_monitor(R1, R2, true, K2),
    mod_route_monitor(R3, R4, true, K2),
    mod_route_monitor(FilterUpRoute, true, K2),
    mod_route_monitor(FilterDownRoute, true, K2),
    mod_route_monitor(HpfUpRoute, true, K2),
    mod_route_monitor(HpfDownRoute, true, K2),

    /* Connect K2 to granular for grain triggering */
    keyboard_connect(K2, 0, G),
    keyboard_connect(K2, 1, G),
    keyboard_connect(K2, 2, G),
    keyboard_connect(K2, 3, G),

    format('~nSynth Granular Setup Loaded~n'),
    format('~n  SYNTH KEYBOARD (window 1):~n'),
    format('    Rows 0-1 (1-P): Pad, D minor pentatonic~n'),
    format('    Rows 2-3 (A-/): Bass, D minor pentatonic~n'),
    format('    LEFT/RIGHT SHIFT: row 3 octave down/up~n'),
    format('~n  GRANULAR KEYBOARD (window 2):~n'),
    format('    All rows trigger grains from synth~n'),
    format('    UP/DOWN arrows: moog LPF cutoff~n'),
    format('    LEFT/RIGHT arrows: HPF cutoff~n'),
    format('    SPACE: toggle position LFO pause~n'),
    format('~n  GAMEPAD:~n'),
    format('    Left stick: density/size~n'),
    format('    Right stick: position/spray~n'),
    format('    Triggers: pitch range~n'),
    format('    D-pad: decay/wet~n'),
    format('    +/-: granular wet~n'),
    format('    Y: reverb freeze~n').

teardown :-
    setup_unload(synth_granular).

/* Pad voice: rich harmonics, slow attack for broad pad sound */
create_pad_voice(Voice, Envelope) :-
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, O1),
    synth_oscillator_add(Voice, 880.0, 0.0, O2),
    synth_oscillator_add(Voice, 1320.0, 0.0, O3),
    synth_oscillator_add(Voice, 1760.0, 0.0, O4),
    synth_oscillator_set_volume(O1, 0.35),
    synth_oscillator_set_volume(O2, 0.25),
    synth_oscillator_set_volume(O3, 0.15),
    synth_oscillator_set_volume(O4, 0.08),
    voice_attach_effect(Voice, vca, [gain=0.0], Vca),
    /* Slow attack, long decay for pad */
    mod_envelope_init(0.3, 0.4, 0.5, 0.7, 0.8, 800.0, false, Envelope),
    mod_route_init(Envelope, vca, Vca, gain, absolute, 1.0, 0.0, 0.0, _).

/* Bass voice: fundamental-heavy, punchy attack */
create_bass_voice(Voice, Envelope) :-
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, O1),
    synth_oscillator_add(Voice, 880.0, 0.0, O2),
    synth_oscillator_set_volume(O1, 0.5),
    synth_oscillator_set_volume(O2, 0.15),
    voice_attach_effect(Voice, vca, [gain=0.0], Vca),
    /* Faster attack, moderate decay for bass */
    mod_envelope_init(0.05, 0.2, 0.3, 0.5, 0.5, 600.0, false, Envelope),
    mod_route_init(Envelope, vca, Vca, gain, absolute, 1.0, 0.0, 0.0, _).
