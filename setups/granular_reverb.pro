:- module(granular_reverb, [setup/1, teardown/0]).
:- use_module('../src/prolog/promini.pro').

/*
 * Granular + Reverb Setup with Gamepad Control
 * Extracted from gamepad_granular.pro
 *
 * Usage:
 *   load_setup(granular_reverb, Source).
 *   unload_setup(granular_reverb).
 */

setup(Source) :-
    control_init,
    control_gamepads(Gamepads),
    (   Gamepads = [gamepad(Id, Name)|_]
    ->  format('Using gamepad: ~w~n', [Name]),
        control_open(Id, GP)
    ;   format('No gamepad found~n'), fail
    ),
    setup_register(granular_reverb:gamepad, GP),

    capture_start(Source, 10.0, Capture, _),
    setup_register(granular_reverb:capture, Capture),

    granular_init(4.0, G),
    setup_register(granular_reverb:granular, G),

    granular_connect(G, Capture),
    granular_set(G, [density=5.0, size=150.0, position=0.5, pan_spray=0.6, recording=true]),
    granular_set_mode(G, [0, 3, 5, 7, 10], 0, 5),

    granular_attach_effect(G, reverb, [wet=0.3, decay=0.8, shimmer1_shift=7.0, shimmer1_mix=0.2], Rev),
    setup_register(granular_reverb:reverb, Rev),

    /* Analog axes */
    mod_gamepad_init(GP, left_x, LX),
    setup_register(granular_reverb:lx, LX),
    mod_gamepad_init(GP, left_y, LY),
    setup_register(granular_reverb:ly, LY),
    mod_gamepad_init(GP, right_x, RX),
    setup_register(granular_reverb:rx, RX),
    mod_gamepad_init(GP, right_y, RY),
    setup_register(granular_reverb:ry, RY),
    mod_gamepad_init(GP, left_trigger, LT),
    setup_register(granular_reverb:lt, LT),
    mod_gamepad_init(GP, right_trigger, RT),
    setup_register(granular_reverb:rt, RT),

    /* Buttons - cycling */
    mod_gamepad_button_init(GP, lb, [0.0, 0.5, 1.0], LB),
    setup_register(granular_reverb:lb, LB),
    mod_gamepad_button_init(GP, rb, [0.0, 0.3, 0.7, 1.0], RB),
    setup_register(granular_reverb:rb, RB),
    mod_gamepad_button_init(GP, r3, [1.0, 0.7, 0.3, 0.0], R3Btn),
    setup_register(granular_reverb:r3btn, R3Btn),

    /* Buttons - trigger */
    mod_gamepad_button_init(GP, a, trigger, ABtn),
    setup_register(granular_reverb:abtn, ABtn),
    mod_gamepad_button_init(GP, l3, trigger, L3Btn),
    setup_register(granular_reverb:l3btn, L3Btn),

    /* Buttons - toggle */
    mod_gamepad_button_init(GP, y, toggle, YBtn),
    setup_register(granular_reverb:ybtn, YBtn),

    /* Buttons - momentary */
    mod_gamepad_button_init(GP, b, momentary, BBtn),
    setup_register(granular_reverb:bbtn, BBtn),
    mod_gamepad_button_init(GP, dpad_up, momentary, DUp),
    setup_register(granular_reverb:dup, DUp),
    mod_gamepad_button_init(GP, dpad_down, momentary, DDown),
    setup_register(granular_reverb:ddown, DDown),
    mod_gamepad_button_init(GP, dpad_left, momentary, DLeft),
    setup_register(granular_reverb:dleft, DLeft),
    mod_gamepad_button_init(GP, dpad_right, momentary, DRight),
    setup_register(granular_reverb:dright, DRight),
    mod_gamepad_button_init(GP, x, toggle, XBtn),
    setup_register(granular_reverb:xbtn, XBtn),
    mod_gamepad_button_init(GP, start, momentary, PlusBtn),
    setup_register(granular_reverb:plusbtn, PlusBtn),
    mod_gamepad_button_init(GP, back, momentary, MinusBtn),
    setup_register(granular_reverb:minusbtn, MinusBtn),

    /* Analog routes */
    mod_route_init(LX, granular, G, density, rate, 10.0, 0.0, 0.0, R1),
    setup_register(granular_reverb:r1, R1),
    mod_route_init(LY, granular, G, size, rate, -100.0, 0.0, 0.0, R2),
    setup_register(granular_reverb:r2, R2),
    mod_route_init(RX, granular, G, position, rate, 0.5, 0.0, 0.0, R3),
    setup_register(granular_reverb:r3, R3),
    mod_route_init(RY, granular, G, position_spray, rate, -0.3, 0.0, 0.0, R4),
    setup_register(granular_reverb:r4, R4),
    mod_route_init(LT, granular, G, deviation_up, rate, -10.0, 0.0, 0.0, R5),
    setup_register(granular_reverb:r5, R5),
    mod_route_init(RT, granular, G, deviation_up, rate, 10.0, 0.0, 0.0, R6),
    setup_register(granular_reverb:r6, R6),

    /* Button routes */
    mod_route_init(LB, granular, G, envelope, absolute, 1.0, 0.0, 0.0, R7),
    setup_register(granular_reverb:r7, R7),
    mod_route_init(RB, granular, G, reverse, absolute, 1.0, 0.0, 0.0, R8),
    setup_register(granular_reverb:r8, R8),
    mod_route_init(R3Btn, granular, G, regularity, absolute, 1.0, 0.0, 0.0, R9),
    setup_register(granular_reverb:r9, R9),
    mod_route_init(ABtn, granular, G, trigger, absolute, 1.0, 0.0, 0.0, R10),
    setup_register(granular_reverb:r10, R10),
    mod_route_init(L3Btn, granular, G, trigger, absolute, 1.0, 0.0, 0.0, R11),
    setup_register(granular_reverb:r11, R11),
    mod_route_init(BBtn, granular, G, reverse, absolute, 1.0, 0.0, 0.0, R12),
    setup_register(granular_reverb:r12, R12),
    mod_route_init(XBtn, granular, G, recording, absolute, -1.0, 1.0, 0.0, R13),
    setup_register(granular_reverb:r13, R13),
    mod_route_init(YBtn, reverb, Rev, freeze, absolute, 1.0, 0.0, 0.0, R14),
    setup_register(granular_reverb:r14, R14),

    /* D-pad: reverb wet/decay increment/decrement */
    mod_route_init(DUp, reverb, Rev, decay, rate, 0.3, 0.0, 0.0, R15),
    setup_register(granular_reverb:r15, R15),
    mod_route_init(DDown, reverb, Rev, decay, rate, -0.3, 0.0, 0.0, R16),
    setup_register(granular_reverb:r16, R16),
    mod_route_init(DRight, reverb, Rev, wet, rate, 0.3, 0.0, 0.0, R17),
    setup_register(granular_reverb:r17, R17),
    mod_route_init(DLeft, reverb, Rev, wet, rate, -0.3, 0.0, 0.0, R18),
    setup_register(granular_reverb:r18, R18),

    /* +/- buttons: granular wet */
    mod_route_init(PlusBtn, granular, G, wet, rate, 0.3, 0.0, 0.0, R19),
    setup_register(granular_reverb:r19, R19),
    mod_route_init(MinusBtn, granular, G, wet, rate, -0.3, 0.0, 0.0, R20),
    setup_register(granular_reverb:r20, R20),

    /* Keyboard for triggering grains */
    keyboard_init(K),
    setup_register(granular_reverb:keyboard, K),

    /* Enable monitoring on all routes */
    maplist(mod_route_monitor,
            [R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15,R16,R17,R18,R19,R20],
            [true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],
            [K,K,K,K,K,K,K,K,K,K,K,K,K,K,K,K,K,K,K,K]),
    keyboard_connect(K, 0, G),
    keyboard_connect(K, 1, G),
    keyboard_connect(K, 2, G),
    keyboard_connect(K, 3, G),

    format('~nGranular Reverb Setup Loaded~n'),
    format('  Left stick: density/size~n'),
    format('  Right stick: position/spray~n'),
    format('  Triggers: pitch range (LT=down, RT=up)~n'),
    format('  D-pad: up/down decay, left/right reverb wet~n'),
    format('  +/-: granular wet~n'),
    format('  A/L3: trigger grain | X: rec freeze | B: momentary reverse~n'),
    format('  Y: reverb freeze~n'),
    format('  LB: envelope (0/0.5/1) | RB: reverse (0/0.3/0.7/1)~n'),
    format('  R3: regularity (1/0.7/0.3/0)~n'),
    format('  Keyboard: QWERTY rows trigger grains~n').

teardown :-
    setup_unload(granular_reverb).
