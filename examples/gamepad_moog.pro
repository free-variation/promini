% gamepad_moog.pro
% Left stick: moog cutoff (X) and resonance (Y) - rate mode
% D-pad: pan (X) and VCA level (Y) - rate mode
% Left trigger: filter opener (rate)
% Right trigger: ping-pong delay time increase (absolute)

:- use_module('src/prolog/promini.pro').

demo :-
    % Initialize
    promini_init,
    control_init,

    % Get first gamepad
    control_gamepads([gamepad(Id, Name)|_]),
    format('Using gamepad: ~w~n', [Name]),
    control_open(Id, Gamepad),

    % Load a sound and attach effects
    sound_load('audio/guitar.wav', Sound),
    sound_loop(Sound),
    sound_attach_effect(Sound, moog, [cutoff=500.0, resonance=1.0], effect(_, MoogPtr)),
    sound_attach_effect(Sound, pan, [pan=0.0], effect(_, PanPtr)),
    sound_attach_effect(Sound, vca, [gain=0.5], effect(_, VcaPtr)),
    sound_attach_effect(Sound, ping_pong_delay,
        [max_delay_in_frames=48000, delay_in_frames=12000, feedback=0.5, wet=0.3],
        effect(_, DelayPtr)),

    % Left stick mod sources
    mod_gamepad_create(Gamepad, left_x, LeftXSource),
    mod_gamepad_create(Gamepad, left_y, LeftYSource),

    % D-pad mod sources
    mod_gamepad_create(Gamepad, dpad_x, DpadXSource),
    mod_gamepad_create(Gamepad, dpad_y, DpadYSource),

    % Trigger mod sources
    mod_gamepad_create(Gamepad, left_trigger, LTSource),
    mod_gamepad_create(Gamepad, right_trigger, RTSource),

    % Left stick -> moog (rate mode)
    mod_route_create(LeftXSource, moog, MoogPtr, cutoff, rate, 500.0, 0.0, 0.0, _),
    mod_route_create(LeftYSource, moog, MoogPtr, resonance, rate, -0.5, 0.0, 0.0, _),

    % D-pad -> pan and VCA (rate mode, discrete steps)
    mod_route_create(DpadXSource, pan, PanPtr, pan, rate, 0.5, 0.0, 0.0, _),
    mod_route_create(DpadYSource, vca, VcaPtr, gain, rate, -0.5, 0.0, 0.0, _),

    % Left trigger -> filter opener (rate: adds to cutoff when pressed)
    mod_route_create(LTSource, moog, MoogPtr, cutoff, rate, 500.0, 0.0, 0.0, _),

    % Right trigger -> delay time increase (absolute: 0=12000, 1=36000 frames)
    mod_route_create(RTSource, ping_pong_delay, DelayPtr, delay, absolute, 24000.0, 12000.0, 0.0, _),

    % Start playing
    format('Left stick: filter cutoff/resonance (rate)~n'),
    format('D-pad: pan/volume~n'),
    format('Left trigger: filter opener~n'),
    format('Right trigger: delay time increase~n'),
    format('Press Enter to stop.~n'),
    sound_start(Sound),
    read_line_to_string(user_input, _),

    % Cleanup
    sound_stop(Sound),
    sound_unload(Sound),
    control_close(Gamepad),
    control_shutdown.
