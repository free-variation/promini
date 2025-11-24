:- use_module('src/prolog/sampler.pro').

demo_chord :-
    format('Creating three voices for A minor triad...~n'),
    sampler_synth_voice_create(Voice1),
    sampler_synth_voice_create(Voice2),
    sampler_synth_voice_create(Voice3),

    format('Setting frequencies (A4, C5, E5)...~n'),
    sampler_synth_voice_set_frequency(Voice1, 440.0),    % A4
    sampler_synth_voice_set_frequency(Voice2, 523.25),   % C5
    sampler_synth_voice_set_frequency(Voice3, 659.25),   % E5

    format('Starting voices...~n'),
    sampler_synth_voice_start(Voice1),
    sampler_synth_voice_start(Voice2),
    sampler_synth_voice_start(Voice3),

    format('Playing A minor chord for 10 seconds...~n'),
    sleep(10),

    format('Stopping voices...~n'),
    sampler_synth_voice_stop(Voice1),
    sampler_synth_voice_stop(Voice2),
    sampler_synth_voice_stop(Voice3),

    sampler_synth_voice_unload(Voice1),
    sampler_synth_voice_unload(Voice2),
    sampler_synth_voice_unload(Voice3),

    format('Demo complete.~n').

demo_harmonics :-
    format('Creating additive tone from harmonic partials...~n'),
    format('Fundamental: 220Hz (A3)~n~n'),

    Fundamental = 220.0,

    format('Creating 8 harmonic partials...~n'),
    sampler_synth_voice_create(V1),
    sampler_synth_voice_create(V2),
    sampler_synth_voice_create(V3),
    sampler_synth_voice_create(V4),
    sampler_synth_voice_create(V5),
    sampler_synth_voice_create(V6),
    sampler_synth_voice_create(V7),
    sampler_synth_voice_create(V8),

    format('Setting harmonic frequencies (1f, 2f, 3f, 4f, 5f, 6f, 7f, 8f)...~n'),
    sampler_synth_voice_set_frequency(V1, Fundamental),           % 220Hz
    F2 is Fundamental * 2, sampler_synth_voice_set_frequency(V2, F2),  % 440Hz
    F3 is Fundamental * 3, sampler_synth_voice_set_frequency(V3, F3),  % 660Hz
    F4 is Fundamental * 4, sampler_synth_voice_set_frequency(V4, F4),  % 880Hz
    F5 is Fundamental * 5, sampler_synth_voice_set_frequency(V5, F5),  % 1100Hz
    F6 is Fundamental * 6, sampler_synth_voice_set_frequency(V6, F6),  % 1320Hz
    F7 is Fundamental * 7, sampler_synth_voice_set_frequency(V7, F7),  % 1540Hz
    F8 is Fundamental * 8, sampler_synth_voice_set_frequency(V8, F8),  % 1760Hz

    format('Starting all partials (sawtooth-like spectrum)...~n'),
    sampler_synth_voice_start(V1),
    sampler_synth_voice_start(V2),
    sampler_synth_voice_start(V3),
    sampler_synth_voice_start(V4),
    sampler_synth_voice_start(V5),
    sampler_synth_voice_start(V6),
    sampler_synth_voice_start(V7),
    sampler_synth_voice_start(V8),

    format('Playing for 5 seconds...~n'),
    sleep(5),

    format('Stopping all voices...~n'),
    sampler_synth_voice_stop(V1),
    sampler_synth_voice_stop(V2),
    sampler_synth_voice_stop(V3),
    sampler_synth_voice_stop(V4),
    sampler_synth_voice_stop(V5),
    sampler_synth_voice_stop(V6),
    sampler_synth_voice_stop(V7),
    sampler_synth_voice_stop(V8),

    sampler_synth_voice_unload(V1),
    sampler_synth_voice_unload(V2),
    sampler_synth_voice_unload(V3),
    sampler_synth_voice_unload(V4),
    sampler_synth_voice_unload(V5),
    sampler_synth_voice_unload(V6),
    sampler_synth_voice_unload(V7),
    sampler_synth_voice_unload(V8),

    format('Demo complete.~n').

demo_odd_harmonics :-
    format('Creating tone with only odd harmonics (square-like)...~n'),
    format('Fundamental: 220Hz (A3)~n~n'),

    Fundamental = 220.0,

    format('Creating 4 odd harmonic partials...~n'),
    sampler_synth_voice_create(V1),
    sampler_synth_voice_create(V3),
    sampler_synth_voice_create(V5),
    sampler_synth_voice_create(V7),

    format('Setting odd harmonic frequencies (1f, 3f, 5f, 7f)...~n'),
    sampler_synth_voice_set_frequency(V1, Fundamental),           % 220Hz
    F3 is Fundamental * 3, sampler_synth_voice_set_frequency(V3, F3),  % 660Hz
    F5 is Fundamental * 5, sampler_synth_voice_set_frequency(V5, F5),  % 1100Hz
    F7 is Fundamental * 7, sampler_synth_voice_set_frequency(V7, F7),  % 1540Hz

    format('Starting all partials...~n'),
    sampler_synth_voice_start(V1),
    sampler_synth_voice_start(V3),
    sampler_synth_voice_start(V5),
    sampler_synth_voice_start(V7),

    format('Playing for 5 seconds...~n'),
    sleep(5),

    format('Stopping all voices...~n'),
    sampler_synth_voice_stop(V1),
    sampler_synth_voice_stop(V3),
    sampler_synth_voice_stop(V5),
    sampler_synth_voice_stop(V7),

    sampler_synth_voice_unload(V1),
    sampler_synth_voice_unload(V3),
    sampler_synth_voice_unload(V5),
    sampler_synth_voice_unload(V7),

    format('Demo complete.~n').

demo_stereo_spread :-
    format('Creating harmonic partials with moving stereo positions...~n'),
    format('Fundamental: 55Hz (A1)~n~n'),

    Fundamental = 55.0,

    format('Creating 8 harmonic partials...~n'),
    sampler_synth_voice_create(V1),
    sampler_synth_voice_create(V2),
    sampler_synth_voice_create(V3),
    sampler_synth_voice_create(V4),
    sampler_synth_voice_create(V5),
    sampler_synth_voice_create(V6),
    sampler_synth_voice_create(V7),
    sampler_synth_voice_create(V8),

    format('Setting harmonic frequencies...~n'),
    sampler_synth_voice_set_frequency(V1, Fundamental),
    F2 is Fundamental * 2, sampler_synth_voice_set_frequency(V2, F2),
    F3 is Fundamental * 3, sampler_synth_voice_set_frequency(V3, F3),
    F4 is Fundamental * 4, sampler_synth_voice_set_frequency(V4, F4),
    F5 is Fundamental * 5, sampler_synth_voice_set_frequency(V5, F5),
    F6 is Fundamental * 6, sampler_synth_voice_set_frequency(V6, F6),
    F7 is Fundamental * 7, sampler_synth_voice_set_frequency(V7, F7),
    F8 is Fundamental * 8, sampler_synth_voice_set_frequency(V8, F8),

    format('Starting all partials...~n'),
    sampler_synth_voice_start(V1),
    sampler_synth_voice_start(V2),
    sampler_synth_voice_start(V3),
    sampler_synth_voice_start(V4),
    sampler_synth_voice_start(V5),
    sampler_synth_voice_start(V6),
    sampler_synth_voice_start(V7),
    sampler_synth_voice_start(V8),

    format('Moving sounds across stereo field in opposite directions...~n'),
    animate_pan([V1,V2,V3,V4], [V5,V6,V7,V8], 0.0, 100),

    format('Stopping all voices...~n'),
    sampler_synth_voice_stop(V1),
    sampler_synth_voice_stop(V2),
    sampler_synth_voice_stop(V3),
    sampler_synth_voice_stop(V4),
    sampler_synth_voice_stop(V5),
    sampler_synth_voice_stop(V6),
    sampler_synth_voice_stop(V7),
    sampler_synth_voice_stop(V8),

    sampler_synth_voice_unload(V1),
    sampler_synth_voice_unload(V2),
    sampler_synth_voice_unload(V3),
    sampler_synth_voice_unload(V4),
    sampler_synth_voice_unload(V5),
    sampler_synth_voice_unload(V6),
    sampler_synth_voice_unload(V7),
    sampler_synth_voice_unload(V8),

    format('Demo complete.~n').

animate_pan(_, _, _, 0) :- !.
animate_pan(GroupA, GroupB, Time, Steps) :-
    Steps > 0,
    Pan is sin(Time) * 0.8,
    PanOpp is -Pan,
    maplist(set_voice_pan(Pan), GroupA),
    maplist(set_voice_pan(PanOpp), GroupB),
    sleep(0.05),
    NextTime is Time + 0.1,
    NextSteps is Steps - 1,
    animate_pan(GroupA, GroupB, NextTime, NextSteps).

set_voice_pan(Pan, Voice) :-
    sampler_synth_voice_set_pan(Voice, Pan).
