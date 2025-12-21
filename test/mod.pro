:- module(test_mod, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(mod).

% LFO tests

test(create_lfo_sine, [nondet, cleanup(mod_source_uninit(L))]) :-
    mod_lfo_init(sine, 1.0, L),
    L = mod_source(_).

test(create_lfo_square, [nondet, cleanup(mod_source_uninit(L))]) :-
    mod_lfo_init(square, 2.0, L),
    L = mod_source(_).

test(create_lfo_triangle, [nondet, cleanup(mod_source_uninit(L))]) :-
    mod_lfo_init(triangle, 0.5, L),
    L = mod_source(_).

test(create_lfo_sawtooth, [nondet, cleanup(mod_source_uninit(L))]) :-
    mod_lfo_init(sawtooth, 4.0, L),
    L = mod_source(_).

test(lfo_get_frequency, [nondet, cleanup(mod_source_uninit(L))]) :-
    mod_lfo_init(sine, 5.0, L),
    mod_lfo_get_frequency(L, Freq),
    Freq =:= 5.0.

test(lfo_set_frequency, [nondet, cleanup(mod_source_uninit(L))]) :-
    mod_lfo_init(sine, 1.0, L),
    mod_lfo_set_frequency(L, 10.0),
    mod_lfo_get_frequency(L, Freq),
    Freq =:= 10.0.

test(lfo_unload, [nondet]) :-
    mod_lfo_init(sine, 1.0, L),
    mod_source_uninit(L).

% Route tests

test(create_route, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(L),
    synth_voice_uninit(V)
))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    mod_lfo_init(sine, 1.0, L),
    mod_route_init(L, oscillator, O, frequency, absolute, 100.0, 440.0, 0.0, R),
    R = mod_route(_).

test(route_unload, [nondet, cleanup((
    mod_source_uninit(L),
    synth_voice_uninit(V)
))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    mod_lfo_init(sine, 1.0, L),
    mod_route_init(L, oscillator, O, frequency, absolute, 100.0, 440.0, 0.0, R),
    mod_route_uninit(R).

test(source_unload_removes_routes, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    mod_lfo_init(sine, 1.0, L),
    mod_route_init(L, oscillator, O, frequency, absolute, 100.0, 440.0, 0.0, _R),
    mod_source_uninit(L).

test(route_monitor_enable, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(L),
    synth_voice_uninit(V)
))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    mod_lfo_init(sine, 1.0, L),
    mod_route_init(L, oscillator, O, frequency, absolute, 100.0, 440.0, 0.0, R),
    mod_route_monitor(R, true).

test(route_monitor_disable, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(L),
    synth_voice_uninit(V)
))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    mod_lfo_init(sine, 1.0, L),
    mod_route_init(L, oscillator, O, frequency, absolute, 100.0, 440.0, 0.0, R),
    mod_route_monitor(R, true),
    mod_route_monitor(R, false).

test(route_monitor_invalid_route, [error(existence_error(mod_route, _))]) :-
    mod_route_monitor(mod_route(9999), true).

% Envelope tests

test(create_envelope, [nondet, cleanup(mod_source_uninit(E))]) :-
    mod_envelope_init(0.1, 0.2, 0.3, 0.5, 0.4, 1000.0, false, E),
    E = mod_source(_).

test(create_envelope_looping, [nondet, cleanup(mod_source_uninit(E))]) :-
    mod_envelope_init(0.1, 0.1, 0.6, 0.7, 0.2, 500.0, true, E),
    E = mod_source(_).

test(envelope_trigger, [nondet, cleanup(mod_source_uninit(E))]) :-
    mod_envelope_init(0.2, 0.3, 0.3, 0.5, 0.2, 200.0, false, E),
    mod_envelope_trigger(E).

test(envelope_route_to_oscillator, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(E),
    synth_voice_uninit(V)
))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.5, O),
    mod_envelope_init(0.1, 0.2, 0.5, 0.7, 0.2, 100.0, false, E),
    mod_route_init(E, oscillator, O, frequency, absolute, 100.0, 440.0, 0.0, R),
    R = mod_route(_).

% Volume routing tests

test(route_oscillator_volume, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(L),
    synth_voice_uninit(V)
))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.5, O),
    mod_lfo_init(sine, 2.0, L),
    mod_route_init(L, oscillator, O, volume, absolute, 0.3, 0.5, 0.0, R),
    R = mod_route(_).

test(envelope_route_oscillator_volume, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(E),
    synth_voice_uninit(V)
))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.5, O),
    mod_envelope_init(0.1, 0.2, 0.5, 0.7, 0.2, 100.0, false, E),
    mod_route_init(E, oscillator, O, volume, absolute, 1.0, 0.0, 0.0, R),
    R = mod_route(_).

% Pan effect routing tests

test(route_voice_pan_effect, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(L),
    synth_voice_uninit(V)
))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.5, _O),
    voice_attach_effect(V, pan, [pan=0.0], Pan),
    mod_lfo_init(sine, 1.0, L),
    mod_route_init(L, pan, Pan, pan, absolute, 1.0, 0.0, 0.0, R),
    R = mod_route(_).

test(route_sound_pan_effect, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(L),
    sound_unload(S)
))]) :-
    sound_load('audio/guitar.wav', S),
    sound_attach_effect(S, pan, [pan=0.0], Pan),
    mod_lfo_init(sine, 1.0, L),
    mod_route_init(L, pan, Pan, pan, absolute, 1.0, 0.0, 0.0, R),
    R = mod_route(_).

test(pan_effect_set_parameters, [nondet, cleanup(synth_voice_uninit(V))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.5, _O),
    voice_attach_effect(V, pan, [pan=0.0], Effect),
    effect_set_parameters(Effect, [pan=0.5]).

% Moog cutoff routing tests

test(route_sound_moog_cutoff, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(L),
    sound_unload(S)
))]) :-
    sound_load('audio/guitar.wav', S),
    sound_attach_effect(S, moog, [cutoff=1000.0], Moog),
    mod_lfo_init(sine, 1.0, L),
    mod_route_init(L, moog, Moog, cutoff, absolute, 500.0, 1000.0, 0.0, R),
    R = mod_route(_).

test(route_voice_moog_cutoff, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(L),
    synth_voice_uninit(V)
))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.5, _O),
    voice_attach_effect(V, moog, [cutoff=2000.0], Moog),
    mod_lfo_init(sine, 1.0, L),
    mod_route_init(L, moog, Moog, cutoff, absolute, 1000.0, 1500.0, 0.0, R),
    R = mod_route(_).

% VCA gain routing tests

test(route_sound_vca_gain, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(L),
    sound_unload(S)
))]) :-
    sound_load('audio/guitar.wav', S),
    sound_attach_effect(S, vca, [gain=1.0], Vca),
    mod_lfo_init(sine, 4.0, L),
    mod_route_init(L, vca, Vca, gain, absolute, 0.4, 0.6, 0.0, R),
    R = mod_route(_).

test(route_voice_vca_gain, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(L),
    synth_voice_uninit(V)
))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.5, _O),
    voice_attach_effect(V, vca, [gain=1.0], Vca),
    mod_lfo_init(sine, 4.0, L),
    mod_route_init(L, vca, Vca, gain, absolute, 0.5, 0.5, 0.0, R),
    R = mod_route(_).

test(envelope_route_vca_gain, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(E),
    sound_unload(S)
))]) :-
    sound_load('audio/guitar.wav', S),
    sound_attach_effect(S, vca, [gain=0.0], Vca),
    mod_envelope_init(0.1, 0.2, 0.5, 0.7, 0.2, 100.0, false, E),
    mod_route_init(E, vca, Vca, gain, absolute, 1.0, 0.0, 0.0, R),
    R = mod_route(_).

% D-pad gamepad mod source tests

test(mod_gamepad_dpad_x, [nondet, setup(control_init), cleanup((
    mod_route_uninit(R),
    mod_source_uninit(S),
    synth_voice_uninit(V),
    control_close(G),
    control_shutdown
))]) :-
    control_gamepads([gamepad(Id, _)|_]),
    control_open(Id, G),
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.5, O),
    mod_gamepad_init(G, dpad_x, S),
    mod_route_init(S, oscillator, O, frequency, rate, 100.0, 0.0, 0.0, R),
    R = mod_route(_).

test(mod_gamepad_dpad_y, [nondet, setup(control_init), cleanup((
    mod_route_uninit(R),
    mod_source_uninit(S),
    synth_voice_uninit(V),
    control_close(G),
    control_shutdown
))]) :-
    control_gamepads([gamepad(Id, _)|_]),
    control_open(Id, G),
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.5, O),
    mod_gamepad_init(G, dpad_y, S),
    mod_route_init(S, oscillator, O, frequency, rate, 100.0, 0.0, 0.0, R),
    R = mod_route(_).

% Gamepad button mod source tests

test(mod_gamepad_button_momentary, [nondet, setup(control_init), cleanup((
    mod_source_uninit(S),
    control_close(G),
    control_shutdown
))]) :-
    control_gamepads([gamepad(Id, _)|_]),
    control_open(Id, G),
    mod_gamepad_button_init(G, a, momentary, S),
    S = mod_source(_).

test(mod_gamepad_button_trigger, [nondet, setup(control_init), cleanup((
    mod_source_uninit(S),
    control_close(G),
    control_shutdown
))]) :-
    control_gamepads([gamepad(Id, _)|_]),
    control_open(Id, G),
    mod_gamepad_button_init(G, b, trigger, S),
    S = mod_source(_).

test(mod_gamepad_button_toggle, [nondet, setup(control_init), cleanup((
    mod_source_uninit(S),
    control_close(G),
    control_shutdown
))]) :-
    control_gamepads([gamepad(Id, _)|_]),
    control_open(Id, G),
    mod_gamepad_button_init(G, x, toggle, S),
    S = mod_source(_).

test(mod_gamepad_button_cycling, [nondet, setup(control_init), cleanup((
    mod_source_uninit(S),
    control_close(G),
    control_shutdown
))]) :-
    control_gamepads([gamepad(Id, _)|_]),
    control_open(Id, G),
    mod_gamepad_button_init(G, lb, [0.0, 0.5, 1.0], S),
    S = mod_source(_).

test(mod_gamepad_button_route, [nondet, setup(control_init), cleanup((
    mod_route_uninit(R),
    mod_source_uninit(S),
    synth_voice_uninit(V),
    control_close(G),
    control_shutdown
))]) :-
    control_gamepads([gamepad(Id, _)|_]),
    control_open(Id, G),
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.5, O),
    mod_gamepad_button_init(G, rb, [0.0, 0.3, 0.7, 1.0], S),
    mod_route_init(S, oscillator, O, volume, absolute, 1.0, 0.0, 0.0, R),
    R = mod_route(_).

% Ping-pong delay routing tests

test(route_sound_ping_pong_delay, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(L),
    sound_unload(S)
))]) :-
    sound_load('audio/guitar.wav', S),
    sound_attach_effect(S, ping_pong_delay,
        [max_delay_in_frames=48000, delay_in_frames=12000, feedback=0.5, wet=0.3],
        Delay),
    mod_lfo_init(sine, 0.5, L),
    mod_route_init(L, ping_pong_delay, Delay, delay, absolute, 24000.0, 24000.0, 0.0, R),
    R = mod_route(_).

test(route_voice_ping_pong_delay, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(L),
    synth_voice_uninit(V)
))]) :-
    synth_voice_init(V),
    synth_oscillator_add(V, 440.0, 0.5, _O),
    voice_attach_effect(V, ping_pong_delay,
        [max_delay_in_frames=48000, delay_in_frames=12000, feedback=0.5, wet=0.3],
        Delay),
    mod_lfo_init(sine, 0.5, L),
    mod_route_init(L, ping_pong_delay, Delay, delay, absolute, 24000.0, 24000.0, 0.0, R),
    R = mod_route(_).

test(envelope_route_ping_pong_delay, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(E),
    sound_unload(S)
))]) :-
    sound_load('audio/guitar.wav', S),
    sound_attach_effect(S, ping_pong_delay,
        [max_delay_in_frames=48000, delay_in_frames=12000, feedback=0.5, wet=0.3],
        Delay),
    mod_envelope_init(0.1, 0.2, 0.5, 0.7, 0.2, 500.0, false, E),
    mod_route_init(E, ping_pong_delay, Delay, delay, absolute, 24000.0, 12000.0, 0.0, R),
    R = mod_route(_).

% Granular routing tests

test(route_granular_density, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(L),
    granular_uninit(G)
))]) :-
    granular_init(2.0, G),
    mod_lfo_init(sine, 1.0, L),
    mod_route_init(L, granular, G, density, absolute, 10.0, 5.0, 0.0, R),
    R = mod_route(_).

test(route_granular_pitch, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(L),
    granular_uninit(G)
))]) :-
    granular_init(2.0, G),
    mod_lfo_init(sine, 0.5, L),
    mod_route_init(L, granular, G, pitch, absolute, 12.0, 0.0, 0.0, R),
    R = mod_route(_).

test(keyboard_route_granular_density, [nondet, setup(control_init), cleanup((
    mod_route_uninit(R),
    mod_source_uninit(K),
    granular_uninit(G),
    control_shutdown
))]) :-
    granular_init(2.0, G),
    mod_keyboard_init(space, [attack=100, release=200], K),
    mod_route_init(K, granular, G, density, absolute, 15.0, 0.0, 0.0, R),
    R = mod_route(_).

test(keyboard_route_granular_pitch, [nondet, setup(control_init), cleanup((
    mod_route_uninit(R),
    mod_source_uninit(K),
    granular_uninit(G),
    control_shutdown
))]) :-
    granular_init(2.0, G),
    mod_keyboard_init(up, [attack=50, release=300], K),
    mod_route_init(K, granular, G, pitch, absolute, 12.0, 0.0, 0.0, R),
    R = mod_route(_).

% Noise source tests

test(create_noise_white, [nondet, cleanup(mod_source_uninit(N))]) :-
    mod_noise_init(white, N),
    N = mod_source(_).

test(create_noise_pink, [nondet, cleanup(mod_source_uninit(N))]) :-
    mod_noise_init(pink, N),
    N = mod_source(_).

test(create_noise_brownian, [nondet, cleanup(mod_source_uninit(N))]) :-
    mod_noise_init(brownian, N),
    N = mod_source(_).

test(noise_route_to_granular_position, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(N),
    granular_uninit(G)
))]) :-
    granular_init(2.0, G),
    mod_noise_init(white, N),
    mod_route_init(N, granular, G, position, absolute, 0.5, 0.5, 0.0, R),
    R = mod_route(_).

% Sample & hold tests

test(noise_with_sh, [nondet, cleanup(mod_source_uninit(N))]) :-
    mod_noise_init(white, N),
    mod_source_set_sh(N, 1000.0).

test(lfo_with_sh, [nondet, cleanup(mod_source_uninit(L))]) :-
    mod_lfo_init(sine, 10.0, L),
    mod_source_set_sh(L, 500.0).

test(sh_disable, [nondet, cleanup(mod_source_uninit(N))]) :-
    mod_noise_init(pink, N),
    mod_source_set_sh(N, 1000.0),
    mod_source_set_sh(N, 0.0).

test(noise_sh_route_granular_position, [nondet, cleanup((
    mod_route_uninit(R),
    mod_source_uninit(N),
    granular_uninit(G)
))]) :-
    granular_init(2.0, G),
    mod_noise_init(white, N),
    mod_source_set_sh(N, 1000.0),
    mod_route_init(N, granular, G, position, absolute, 0.5, 0.5, 0.0, R),
    R = mod_route(_).

:- end_tests(mod).
