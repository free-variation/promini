:- use_module('../src/prolog/sampler.pro').

/*
 * Capture Demo - Live microphone granulation
 *
 * Demonstrates:
 * - Starting capture from microphone
 * - Querying buffer state
 * - Extracting audio snapshots
 * - Creating granular textures from live input
 */

demo_capture_basic :-
    format('~n=== Live Capture Demo ===~n~n'),

    % Find first capture device (microphone)
    sampler_devices(Devices),
    (member(device(DeviceName, capture, _), Devices) ->
        format('Using capture device: ~w~n', [DeviceName])
    ;
        format('ERROR: No capture device found~n'),
        fail
    ),

    % Start 10-second capture buffer
    format('Starting capture with 10-second buffer...~n'),
    sampler_capture_start(DeviceName, 10.0, Capture, BufferFrames),
    sampler_capture_get_info(Capture, capture_info(_, _, SampleRate)),
    format('Buffer: ~w frames at ~w Hz~n', [BufferFrames, SampleRate]),

    % Record for 2 seconds
    format('~nRecording for 2 seconds... SPEAK INTO MICROPHONE~n'),
    sleep(2.0),

    % Extract three 100ms grains from different positions
    format('~nExtracting grains from captured audio...~n'),
    GrainLength is SampleRate // 10,  % 100ms

    Offset1 is -(SampleRate),           % 1 second ago
    Offset2 is -(SampleRate * 3 // 2),  % 1.5 seconds ago
    Offset3 is -(SampleRate // 2),      % 0.5 seconds ago

    sampler_capture_extract(Capture, Offset1, GrainLength, Data1),
    sampler_capture_extract(Capture, Offset2, GrainLength, Data2),
    sampler_capture_extract(Capture, Offset3, GrainLength, Data3),

    format('Extracted 3 grains of ~w frames each~n', [GrainLength]),

    % Create sounds from grains with different pitches
    format('~nCreating sounds with pitch variations...~n'),
    sampler_sound_create(Data1, Sound1),
    sampler_sound_create(Data2, Sound2),
    sampler_sound_create(Data3, Sound3),

    sampler_sound_set_pitch(Sound1, -7.0),   % Lower
    sampler_sound_set_pitch(Sound2, 0.0),    % Original
    sampler_sound_set_pitch(Sound3, 7.0),    % Higher

    % Play grains in sequence
    format('Playing grains: low pitch -> original -> high pitch~n~n'),
    sampler_sound_start(Sound1),
    sleep(0.15),
    sampler_sound_start(Sound2),
    sleep(0.15),
    sampler_sound_start(Sound3),
    sleep(0.15),

    % Wait for playback
    sleep(0.5),

    % Cleanup
    format('Cleaning up...~n'),
    sampler_sound_unload(Sound1),
    sampler_sound_unload(Sound2),
    sampler_sound_unload(Sound3),
    sampler_data_unload(Data1),
    sampler_data_unload(Data2),
    sampler_data_unload(Data3),
    sampler_capture_stop(Capture),

    format('~nDemo complete!~n~n').


demo_capture_granular :-
    format('~n=== Live Granular Synthesis Demo ===~n~n'),

    % Find microphone
    sampler_devices(Devices),
    (member(device(DeviceName, capture, _), Devices) ->
        format('Using: ~w~n', [DeviceName])
    ;
        format('ERROR: No capture device~n'),
        fail
    ),

    % Start capture
    format('Starting 10-second capture buffer...~n'),
    sampler_capture_start(DeviceName, 10.0, Capture, _),
    sampler_capture_get_info(Capture, capture_info(_, _, SampleRate)),

    % Record for 3 seconds
    format('Recording for 3 seconds... MAKE SOME NOISE~n'),
    sleep(3.0),

    % Spawn grain cloud
    format('~nSpawning granular cloud (20 grains)...~n'),
    GrainDuration is 500,  % 50ms grains
    GrainLength is (SampleRate * GrainDuration) // 1000,

    spawn_grain_cloud(Capture, SampleRate, GrainLength, 20, Sounds),

    format('Grain cloud playing... (~w overlapping grains)~n', [20]),
    sleep(3.0),

    % Cleanup
    format('~nCleaning up...~n'),
    cleanup_sounds(Sounds),
    sampler_capture_stop(Capture),

    format('Demo complete!~n~n').


% Spawn overlapping grains with random parameters
spawn_grain_cloud(Capture, SampleRate, GrainLength, Count, Sounds) :-
    spawn_grains(Capture, SampleRate, GrainLength, Count, [], Sounds).

spawn_grains(_, _, _, 0, Acc, Acc) :- !.
spawn_grains(Capture, SampleRate, GrainLength, N, Acc, Sounds) :-
    N > 0,

    % Random position: 0.5 to 2.5 seconds ago
    random_between(5, 25, TenthsAgo),
    Offset is -(SampleRate * TenthsAgo) // 10,

    % Extract grain
    sampler_capture_extract(Capture, Offset, GrainLength, Data),
    sampler_sound_create(Data, Sound),

    % Random pitch: -12 to +12 semitones
    random_between(-12, 12, Pitch),
    PitchFloat is Pitch * 1.0,
    sampler_sound_set_pitch(Sound, PitchFloat),

    % Random pan
    random_between(-10, 10, PanInt),
    Pan is PanInt / 10.0,
    sampler_sound_set_pan(Sound, Pan),

    % Envelope: 10ms attack, 20ms decay, 20ms sustain, 50% level
    sampler_sound_attach_envelope(Sound, 0.1, 0.2, 0.2, 0.5, 50.0, false, _),

    % Start with slight delay
    random_between(0, 100, DelayMs),
    Delay is DelayMs / 1000.0,
    sleep(Delay),
    sampler_sound_start(Sound),

    N1 is N - 1,
    spawn_grains(Capture, SampleRate, GrainLength, N1, [[Data, Sound]|Acc], Sounds).


cleanup_sounds([]).
cleanup_sounds([[Data, Sound]|Rest]) :-
    sampler_sound_unload(Sound),
    sampler_data_unload(Data),
    cleanup_sounds(Rest).


% Run basic demo by default
main :-
    sampler_init,
    demo_capture_basic.

% Or run granular version
main_granular :-
    sampler_init,
    demo_capture_granular.
