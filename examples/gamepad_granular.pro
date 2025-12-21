:- use_module('../src/prolog/promini.pro').

/*
 * Gamepad Granular Controller
 * Maps Xbox-style controller to granular synthesis + reverb.
 */
demo(Source) :-
    promini_init('MacBook Air Speakers'),
    control_init,

    control_gamepads(Gamepads),
    (   Gamepads = [gamepad(Id, Name)|_]
    ->  format('Using gamepad: ~w~n', [Name]),
        control_open(Id, GP)
    ;   format('No gamepad found~n'), fail
    ),

    capture_start(Source, 10.0, Capture, _),
    granular_init(4.0, G),
    granular_connect(G, Capture),
    granular_set(G, [density=5.0, size=150.0, position=0.5, pan_spray=0.6, recording=true]),
    granular_attach_effect(G, reverb, [wet=0.3, decay=0.8, shimmer1_shift=7.0, shimmer1_mix=0.2], Rev),

    /* Analog axes */
    mod_gamepad_init(GP, left_x, LX),
    mod_gamepad_init(GP, left_y, LY),
    mod_gamepad_init(GP, right_x, RX),
    mod_gamepad_init(GP, right_y, RY),
    mod_gamepad_init(GP, left_trigger, LT),
    mod_gamepad_init(GP, right_trigger, RT),

    /* Buttons - cycling */
    mod_gamepad_button_init(GP, lb, [0.0, 0.5, 1.0], LB),
    mod_gamepad_button_init(GP, rb, [0.0, 0.3, 0.7, 1.0], RB),
    mod_gamepad_button_init(GP, y, [1.0, 0.7, 0.3, 0.0], YBtn),

    /* Buttons - trigger */
    mod_gamepad_button_init(GP, a, trigger, ABtn),
    mod_gamepad_button_init(GP, l3, trigger, L3Btn),
    mod_gamepad_button_init(GP, r3, trigger, R3Btn),

    /* Buttons - momentary */
    mod_gamepad_button_init(GP, b, momentary, BBtn),
    mod_gamepad_button_init(GP, dpad_up, momentary, DUp),
    mod_gamepad_button_init(GP, dpad_down, momentary, DDown),
    mod_gamepad_button_init(GP, dpad_left, momentary, DLeft),
    mod_gamepad_button_init(GP, dpad_right, momentary, DRight),

    /* Buttons - toggle */
    mod_gamepad_button_init(GP, x, toggle, XBtn),
    mod_gamepad_button_init(GP, pl, toggle, PLBtn),

    /* Analog routes */
    mod_route_init(LX, granular, G, density, rate, 10.0, 0.0, 0.0, R1),
    mod_route_init(LY, granular, G, size, rate, -100.0, 0.0, 0.0, R2),
    mod_route_init(RX, granular, G, position, rate, 0.5, 0.0, 0.0, R3),
    mod_route_init(RY, granular, G, position_spray, rate, -0.3, 0.0, 0.0, R4),
    mod_route_init(LT, granular, G, pitch, absolute, -12.0, 0.0, 0.0, R5),
    mod_route_init(RT, granular, G, pitch, absolute, 12.0, 0.0, 0.0, R6),

    /* Button routes */
    mod_route_init(LB, granular, G, envelope, absolute, 1.0, 0.0, 0.0, R7),
    mod_route_init(RB, granular, G, reverse, absolute, 1.0, 0.0, 0.0, R8),
    mod_route_init(YBtn, granular, G, regularity, absolute, 1.0, 0.0, 0.0, R9),
    mod_route_init(ABtn, granular, G, trigger, absolute, 1.0, 0.0, 0.0, R10),
    mod_route_init(L3Btn, granular, G, trigger, absolute, 1.0, 0.0, 0.0, R11),
    mod_route_init(R3Btn, granular, G, reset, absolute, 1.0, 0.0, 0.0, R12),
    mod_route_init(BBtn, granular, G, reverse, absolute, 1.0, 0.0, 0.0, R13),
    mod_route_init(XBtn, granular, G, recording, absolute, -1.0, 1.0, 0.0, R14),
    mod_route_init(PLBtn, reverb, Rev, freeze, absolute, 1.0, 0.0, 0.0, R15),

    /* D-pad: reverb wet/decay increment/decrement */
    mod_route_init(DUp, reverb, Rev, decay, rate, 0.3, 0.0, 0.0, R16),
    mod_route_init(DDown, reverb, Rev, decay, rate, -0.3, 0.0, 0.0, R17),
    mod_route_init(DRight, reverb, Rev, wet, rate, 0.3, 0.0, 0.0, R18),
    mod_route_init(DLeft, reverb, Rev, wet, rate, -0.3, 0.0, 0.0, R19),

    /* Enable monitoring on all routes */
    maplist(mod_route_monitor, [R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15,R16,R17,R18,R19],
            [true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true]),

    /* Keyboard for triggering grains */
    keyboard_init(K),
    keyboard_set_log_target(K),
    keyboard_connect(K, 0, G),
    keyboard_connect(K, 1, G),
    keyboard_connect(K, 2, G),
    keyboard_connect(K, 3, G),

    format('~nGamepad Granular Controller~n'),
    format('  Left stick: density/size~n'),
    format('  Right stick: position/spray~n'),
    format('  Triggers: pitch bend (-12/+12 semitones)~n'),
    format('  D-pad: up/down decay, left/right wet~n'),
    format('  A/L3: trigger grain | X: rec freeze | B: momentary reverse~n'),
    format('  PL: reverb freeze~n'),
    format('  LB: envelope (0/0.5/1) | RB: reverse (0/0.3/0.7/1)~n'),
    format('  Y: regularity (1/0.7/0.3/0) | R3: reset~n'),
    format('  Keyboard: QWERTY rows trigger grains~n').
