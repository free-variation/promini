:- module(test_mod, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(mod).

% LFO tests

test(create_lfo_sine, [nondet, cleanup(mod_source_unload(L))]) :-
    mod_lfo_create(sine, 1.0, L),
    L = mod_source(_).

test(create_lfo_square, [nondet, cleanup(mod_source_unload(L))]) :-
    mod_lfo_create(square, 2.0, L),
    L = mod_source(_).

test(create_lfo_triangle, [nondet, cleanup(mod_source_unload(L))]) :-
    mod_lfo_create(triangle, 0.5, L),
    L = mod_source(_).

test(create_lfo_sawtooth, [nondet, cleanup(mod_source_unload(L))]) :-
    mod_lfo_create(sawtooth, 4.0, L),
    L = mod_source(_).

test(lfo_get_frequency, [nondet, cleanup(mod_source_unload(L))]) :-
    mod_lfo_create(sine, 5.0, L),
    mod_lfo_get_frequency(L, Freq),
    Freq =:= 5.0.

test(lfo_set_frequency, [nondet, cleanup(mod_source_unload(L))]) :-
    mod_lfo_create(sine, 1.0, L),
    mod_lfo_set_frequency(L, 10.0),
    mod_lfo_get_frequency(L, Freq),
    Freq =:= 10.0.

test(lfo_unload, [nondet]) :-
    mod_lfo_create(sine, 1.0, L),
    mod_source_unload(L).

% Route tests

test(create_route, [nondet, cleanup((
    mod_route_unload(R),
    mod_source_unload(L),
    synth_voice_unload(V)
))]) :-
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    mod_lfo_create(sine, 1.0, L),
    mod_route_create(L, oscillator, O, frequency, absolute, 100.0, 440.0, 0.0, R),
    R = mod_route(_).

test(route_unload, [nondet, cleanup((
    mod_source_unload(L),
    synth_voice_unload(V)
))]) :-
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    mod_lfo_create(sine, 1.0, L),
    mod_route_create(L, oscillator, O, frequency, absolute, 100.0, 440.0, 0.0, R),
    mod_route_unload(R).

test(source_unload_removes_routes, [nondet, cleanup(synth_voice_unload(V))]) :-
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.0, O),
    mod_lfo_create(sine, 1.0, L),
    mod_route_create(L, oscillator, O, frequency, absolute, 100.0, 440.0, 0.0, _R),
    mod_source_unload(L).

% Envelope tests

test(create_envelope, [nondet, cleanup(mod_source_unload(E))]) :-
    mod_envelope_create(0.1, 0.2, 0.3, 0.5, 0.4, 1000.0, false, E),
    E = mod_source(_).

test(create_envelope_looping, [nondet, cleanup(mod_source_unload(E))]) :-
    mod_envelope_create(0.1, 0.1, 0.6, 0.7, 0.2, 500.0, true, E),
    E = mod_source(_).

test(envelope_trigger, [nondet, cleanup(mod_source_unload(E))]) :-
    mod_envelope_create(0.2, 0.3, 0.3, 0.5, 0.2, 200.0, false, E),
    mod_envelope_trigger(E).

test(envelope_route_to_oscillator, [nondet, cleanup((
    mod_route_unload(R),
    mod_source_unload(E),
    synth_voice_unload(V)
))]) :-
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.5, O),
    mod_envelope_create(0.1, 0.2, 0.5, 0.7, 0.2, 100.0, false, E),
    mod_route_create(E, oscillator, O, frequency, absolute, 100.0, 440.0, 0.0, R),
    R = mod_route(_).

% Volume routing tests

test(route_oscillator_volume, [nondet, cleanup((
    mod_route_unload(R),
    mod_source_unload(L),
    synth_voice_unload(V)
))]) :-
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.5, O),
    mod_lfo_create(sine, 2.0, L),
    mod_route_create(L, oscillator, O, volume, absolute, 0.3, 0.5, 0.0, R),
    R = mod_route(_).

test(envelope_route_oscillator_volume, [nondet, cleanup((
    mod_route_unload(R),
    mod_source_unload(E),
    synth_voice_unload(V)
))]) :-
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.5, O),
    mod_envelope_create(0.1, 0.2, 0.5, 0.7, 0.2, 100.0, false, E),
    mod_route_create(E, oscillator, O, volume, absolute, 1.0, 0.0, 0.0, R),
    R = mod_route(_).

% Pan effect routing tests

test(route_voice_pan_effect, [nondet, cleanup((
    mod_route_unload(R),
    mod_source_unload(L),
    synth_voice_unload(V)
))]) :-
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.5, _O),
    voice_attach_effect(V, pan, [pan=0.0], effect(_Source, PanPtr)),
    mod_lfo_create(sine, 1.0, L),
    mod_route_create(L, pan, PanPtr, pan, absolute, 1.0, 0.0, 0.0, R),
    R = mod_route(_).

test(route_sound_pan_effect, [nondet, cleanup((
    mod_route_unload(R),
    mod_source_unload(L),
    sound_unload(S)
))]) :-
    sound_load('audio/guitar.wav', S),
    sound_attach_effect(S, pan, [pan=0.0], effect(_Source, PanPtr)),
    mod_lfo_create(sine, 1.0, L),
    mod_route_create(L, pan, PanPtr, pan, absolute, 1.0, 0.0, 0.0, R),
    R = mod_route(_).

test(pan_effect_set_parameters, [nondet, cleanup(synth_voice_unload(V))]) :-
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.5, _O),
    voice_attach_effect(V, pan, [pan=0.0], Effect),
    effect_set_parameters(Effect, [pan=0.5]).

% Moog cutoff routing tests

test(route_sound_moog_cutoff, [nondet, cleanup((
    mod_route_unload(R),
    mod_source_unload(L),
    sound_unload(S)
))]) :-
    sound_load('audio/guitar.wav', S),
    sound_attach_effect(S, moog, [cutoff=1000.0], effect(_Source, MoogPtr)),
    mod_lfo_create(sine, 1.0, L),
    mod_route_create(L, moog, MoogPtr, cutoff, absolute, 500.0, 1000.0, 0.0, R),
    R = mod_route(_).

test(route_voice_moog_cutoff, [nondet, cleanup((
    mod_route_unload(R),
    mod_source_unload(L),
    synth_voice_unload(V)
))]) :-
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.5, _O),
    voice_attach_effect(V, moog, [cutoff=2000.0], effect(_Source, MoogPtr)),
    mod_lfo_create(sine, 1.0, L),
    mod_route_create(L, moog, MoogPtr, cutoff, absolute, 1000.0, 1500.0, 0.0, R),
    R = mod_route(_).

% VCA gain routing tests

test(route_sound_vca_gain, [nondet, cleanup((
    mod_route_unload(R),
    mod_source_unload(L),
    sound_unload(S)
))]) :-
    sound_load('audio/guitar.wav', S),
    sound_attach_effect(S, vca, [gain=1.0], effect(_Source, VcaPtr)),
    mod_lfo_create(sine, 4.0, L),
    mod_route_create(L, vca, VcaPtr, gain, absolute, 0.4, 0.6, 0.0, R),
    R = mod_route(_).

test(route_voice_vca_gain, [nondet, cleanup((
    mod_route_unload(R),
    mod_source_unload(L),
    synth_voice_unload(V)
))]) :-
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.5, _O),
    voice_attach_effect(V, vca, [gain=1.0], effect(_Source, VcaPtr)),
    mod_lfo_create(sine, 4.0, L),
    mod_route_create(L, vca, VcaPtr, gain, absolute, 0.5, 0.5, 0.0, R),
    R = mod_route(_).

test(envelope_route_vca_gain, [nondet, cleanup((
    mod_route_unload(R),
    mod_source_unload(E),
    sound_unload(S)
))]) :-
    sound_load('audio/guitar.wav', S),
    sound_attach_effect(S, vca, [gain=0.0], effect(_Source, VcaPtr)),
    mod_envelope_create(0.1, 0.2, 0.5, 0.7, 0.2, 100.0, false, E),
    mod_route_create(E, vca, VcaPtr, gain, absolute, 1.0, 0.0, 0.0, R),
    R = mod_route(_).

% D-pad gamepad mod source tests

test(mod_gamepad_dpad_x, [nondet, setup(control_init), cleanup((
    mod_route_unload(R),
    mod_source_unload(S),
    synth_voice_unload(V),
    control_close(G),
    control_shutdown
))]) :-
    control_gamepads([gamepad(Id, _)|_]),
    control_open(Id, G),
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.5, O),
    mod_gamepad_create(G, dpad_x, S),
    mod_route_create(S, oscillator, O, frequency, rate, 100.0, 0.0, 0.0, R),
    R = mod_route(_).

test(mod_gamepad_dpad_y, [nondet, setup(control_init), cleanup((
    mod_route_unload(R),
    mod_source_unload(S),
    synth_voice_unload(V),
    control_close(G),
    control_shutdown
))]) :-
    control_gamepads([gamepad(Id, _)|_]),
    control_open(Id, G),
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.5, O),
    mod_gamepad_create(G, dpad_y, S),
    mod_route_create(S, oscillator, O, frequency, rate, 100.0, 0.0, 0.0, R),
    R = mod_route(_).

% Ping-pong delay routing tests

test(route_sound_ping_pong_delay, [nondet, cleanup((
    mod_route_unload(R),
    mod_source_unload(L),
    sound_unload(S)
))]) :-
    sound_load('audio/guitar.wav', S),
    sound_attach_effect(S, ping_pong_delay,
        [max_delay_in_frames=48000, delay_in_frames=12000, feedback=0.5, wet=0.3],
        effect(_Source, DelayPtr)),
    mod_lfo_create(sine, 0.5, L),
    mod_route_create(L, ping_pong_delay, DelayPtr, delay, absolute, 24000.0, 24000.0, 0.0, R),
    R = mod_route(_).

test(route_voice_ping_pong_delay, [nondet, cleanup((
    mod_route_unload(R),
    mod_source_unload(L),
    synth_voice_unload(V)
))]) :-
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.5, _O),
    voice_attach_effect(V, ping_pong_delay,
        [max_delay_in_frames=48000, delay_in_frames=12000, feedback=0.5, wet=0.3],
        effect(_Source, DelayPtr)),
    mod_lfo_create(sine, 0.5, L),
    mod_route_create(L, ping_pong_delay, DelayPtr, delay, absolute, 24000.0, 24000.0, 0.0, R),
    R = mod_route(_).

test(envelope_route_ping_pong_delay, [nondet, cleanup((
    mod_route_unload(R),
    mod_source_unload(E),
    sound_unload(S)
))]) :-
    sound_load('audio/guitar.wav', S),
    sound_attach_effect(S, ping_pong_delay,
        [max_delay_in_frames=48000, delay_in_frames=12000, feedback=0.5, wet=0.3],
        effect(_Source, DelayPtr)),
    mod_envelope_create(0.1, 0.2, 0.5, 0.7, 0.2, 500.0, false, E),
    mod_route_create(E, ping_pong_delay, DelayPtr, delay, absolute, 24000.0, 12000.0, 0.0, R),
    R = mod_route(_).

:- end_tests(mod).
