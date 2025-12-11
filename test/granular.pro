:- module(test_granular, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(granular).

% Creation tests

test(granular_create, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    G = granular(_).

test(granular_create_fails_zero_buffer, [error(domain_error(positive_number, _))]) :-
    granular_create(0.0, _).

test(granular_create_fails_negative_buffer, [error(domain_error(positive_number, _))]) :-
    granular_create(-1.0, _).

% Parameter getter tests

test(granular_get_defaults, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_get(G, Params),
    memberchk(density=0.0, Params),
    memberchk(position=0.5, Params),
    memberchk(position_spray=0.0, Params),
    memberchk(size=100.0, Params),
    memberchk(size_spray=0.0, Params),
    memberchk(pitch=0.0, Params),
    memberchk(envelope=0.5, Params),
    memberchk(pan=0.0, Params),
    memberchk(pan_spray=0.0, Params),
    memberchk(recording=false, Params),
    memberchk(normalize=true, Params).

% Parameter setter tests

test(granular_set_density, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set(G, [density=10.0]),
    granular_get(G, Params),
    memberchk(density=D, Params),
    abs(D - 10.0) < 0.001.

test(granular_set_position, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set(G, [position=0.75]),
    granular_get(G, Params),
    memberchk(position=P, Params),
    abs(P - 0.75) < 0.001.

test(granular_set_position_clamped, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set(G, [position=1.5]),
    granular_get(G, Params),
    memberchk(position=P, Params),
    abs(P - 1.0) < 0.001.

test(granular_set_size, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set(G, [size=200.0]),
    granular_get(G, Params),
    memberchk(size=S, Params),
    abs(S - 200.0) < 0.001.

test(granular_set_pitch, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set(G, [pitch=7.0]),
    granular_get(G, Params),
    memberchk(pitch=P, Params),
    abs(P - 7.0) < 0.001.

test(granular_set_envelope, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set(G, [envelope=0.0]),
    granular_get(G, Params),
    memberchk(envelope=E, Params),
    abs(E - 0.0) < 0.001.

test(granular_set_envelope_clamped, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set(G, [envelope=2.0]),
    granular_get(G, Params),
    memberchk(envelope=E, Params),
    abs(E - 1.0) < 0.001.

test(granular_set_pan, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set(G, [pan=(-0.5)]),
    granular_get(G, Params),
    memberchk(pan=P, Params),
    abs(P - (-0.5)) < 0.001.

test(granular_set_pan_clamped, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set(G, [pan=(-2.0)]),
    granular_get(G, Params),
    memberchk(pan=P, Params),
    abs(P - (-1.0)) < 0.001.

test(granular_set_regularity, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set(G, [regularity=0.5]),
    granular_get(G, Params),
    memberchk(regularity=R, Params),
    abs(R - 0.5) < 0.001.

test(granular_set_reverse_probability, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set(G, [reverse_probability=0.3]),
    granular_get(G, Params),
    memberchk(reverse_probability=R, Params),
    abs(R - 0.3) < 0.001.

test(granular_set_recording, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set(G, [recording=true]),
    granular_get(G, Params),
    memberchk(recording=true, Params).

test(granular_set_normalize_false, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set(G, [normalize=false]),
    granular_get(G, Params),
    memberchk(normalize=false, Params).

test(granular_set_normalize_true, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set(G, [normalize=false]),
    granular_set(G, [normalize=true]),
    granular_get(G, Params),
    memberchk(normalize=true, Params).

test(granular_set_multiple, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set(G, [density=5.0, position=0.25, size=50.0, pitch=(-12.0)]),
    granular_get(G, Params),
    memberchk(density=D, Params), abs(D - 5.0) < 0.001,
    memberchk(position=P, Params), abs(P - 0.25) < 0.001,
    memberchk(size=S, Params), abs(S - 50.0) < 0.001,
    memberchk(pitch=Pitch, Params), abs(Pitch - (-12.0)) < 0.001.

test(granular_set_spray_params, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set(G, [position_spray=0.1, size_spray=20.0, pan_spray=0.5]),
    granular_get(G, Params),
    memberchk(position_spray=PS, Params), abs(PS - 0.1) < 0.001,
    memberchk(size_spray=SS, Params), abs(SS - 20.0) < 0.001,
    memberchk(pan_spray=PanS, Params), abs(PanS - 0.5) < 0.001.

% Trigger tests

test(granular_trigger, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_trigger(G).

test(granular_trigger_invalid_handle, [error(existence_error(granular_delay, _))]) :-
    granular_trigger(granular(999)).

% Destroy tests

test(granular_destroy, [nondet]) :-
    granular_create(2.0, G),
    granular_destroy(G).

test(granular_destroy_invalid_handle, [error(existence_error(granular_delay, _))]) :-
    granular_destroy(granular(999)).

% Connect tests with sound source

test(granular_connect_sound, [nondet, cleanup((granular_destroy(G), sound_unload(S)))]) :-
    granular_create(2.0, G),
    sound_load('audio/counting.wav', S),
    granular_connect(G, S).

% Connect tests with voice source

test(granular_connect_voice, [nondet, cleanup((granular_destroy(G), synth_voice_unload(V)))]) :-
    granular_create(2.0, G),
    synth_voice_create(V),
    synth_oscillator_add(V, 440.0, 0.0, _),
    granular_connect(G, V).

% Connect tests with capture source

test(granular_connect_capture, [nondet]) :-
    promini_devices(Devices),
    member(device(Name, capture, _), Devices),
    !,
    capture_start(Name, 1.0, C, _),
    granular_create(2.0, G),
    granular_connect(G, C),
    granular_destroy(G),
    capture_stop(C).

test(granular_connect_invalid_source, [error(existence_error(source, _)), cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_connect(G, sound(999)).

% Mode tests

test(granular_set_mode_major, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set_mode(G, [0.0, 2.0, 4.0, 5.0, 7.0, 9.0, 11.0], 0, 6).

test(granular_set_mode_pentatonic, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set_mode(G, [0.0, 3.0, 5.0, 7.0, 10.0], 0, 10).

test(granular_set_mode_disable, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set_mode(G, [0.0, 2.0, 4.0], 0, 2),
    granular_set_mode(G, [], 0, 0).

test(granular_set_mode_with_deviation_down, [nondet, cleanup(granular_destroy(G))]) :-
    granular_create(2.0, G),
    granular_set_mode(G, [0.0, 4.0, 7.0], 2, 4).

test(granular_set_mode_invalid_handle, [error(existence_error(granular_delay, _))]) :-
    granular_set_mode(granular(999), [0.0, 2.0, 4.0], 0, 2).

:- end_tests(granular).
