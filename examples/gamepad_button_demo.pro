:- use_module('src/prolog/promini.pro').

/*
 * Simple demo of gamepad button mod sources.
 * Controls synth oscillator with button modes.
 */
demo :-
    promini_init,
    control_init,

    control_gamepads(Gamepads),
    (   Gamepads = [gamepad(Id, Name)|_]
    ->  format('Using gamepad: ~w~n', [Name]),
        control_open(Id, GP)
    ;   format('No gamepad found~n'), fail
    ),

    /* Create a synth voice */
    synth_voice_init(V),
    synth_oscillator_add(V, 220.0, 0.0, O),
    synth_oscillator_set_volume(O, 0.3),
    voice_attach_effect(V, vca, [gain=0.5], VCA),
    synth_voice_start(V),

    /* X button: toggle - 220Hz vs 440Hz */
    mod_gamepad_button_init(GP, x, toggle, XBtn),
    mod_route_init(XBtn, oscillator, O, frequency, absolute, 220.0, 220.0, 0.0, _),

    /* LB: cycle volume levels */
    mod_gamepad_button_init(GP, lb, [0.1, 0.3, 0.5, 0.7], LB),
    mod_route_init(LB, oscillator, O, volume, absolute, 1.0, 0.0, 0.0, _),

    /* A: momentary silence via VCA */
    mod_gamepad_button_init(GP, a, momentary, ABtn),
    mod_route_init(ABtn, vca, VCA, gain, absolute, -0.5, 0.5, 0.0, _),

    format('~nGamepad Button Demo~n'),
    format('  X: toggle 220Hz / 440Hz~n'),
    format('  LB: cycle volume (0.1, 0.3, 0.5, 0.7)~n'),
    format('  A: momentary mute~n'),
    format('~nPress Enter to exit.~n'),

    read_line_to_string(user_input, _),

    synth_voice_stop(V),
    synth_voice_uninit(V),
    control_close(GP),
    control_shutdown.
