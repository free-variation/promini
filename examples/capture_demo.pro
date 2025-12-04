:- use_module('../src/prolog/promini.pro').

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
    promini_devices(Devices),
    (member(device(DeviceName, capture, _), Devices) ->
        format('Using capture device: ~w~n', [DeviceName])
    ;
        format('ERROR: No capture device found~n'),
        fail
    ),

    % Start 10-second capture buffer
    format('Starting capture with 10-second buffer...~n'),
    capture_start(DeviceName, 10.0, Capture, BufferFrames),
    capture_get_info(Capture, capture_info(_, _, SampleRate)),
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

    capture_extract(Capture, Offset1, GrainLength, Data1),
    capture_extract(Capture, Offset2, GrainLength, Data2),
    capture_extract(Capture, Offset3, GrainLength, Data3),

    format('Extracted 3 grains of ~w frames each~n', [GrainLength]),

    % Create sounds from grains with different pitches
    format('~nCreating sounds with pitch variations...~n'),
    sound_create(Data1, Sound1),
    sound_create(Data2, Sound2),
    sound_create(Data3, Sound3),

    sound_set_pitch(Sound1, -7.0),   % Lower
    sound_set_pitch(Sound2, 0.0),    % Original
    sound_set_pitch(Sound3, 7.0),    % Higher

    % Play grains in sequence
    format('Playing grains: low pitch -> original -> high pitch~n~n'),
    sound_start(Sound1),
    sleep(0.15),
    sound_start(Sound2),
    sleep(0.15),
    sound_start(Sound3),
    sleep(0.15),

    % Wait for playback
    sleep(0.5),

    % Cleanup
    format('Cleaning up...~n'),
    sound_unload(Sound1),
    sound_unload(Sound2),
    sound_unload(Sound3),
    audio_unload(Data1),
    audio_unload(Data2),
    audio_unload(Data3),
    capture_stop(Capture),

    format('~nDemo complete!~n~n').


demo_capture_granular :-
    format('~n=== Live Granular Synthesis Demo ===~n~n'),

    % Find microphone
    promini_devices(Devices),
    (member(device(DeviceName, capture, _), Devices) ->
        format('Using: ~w~n', [DeviceName])
    ;
        format('ERROR: No capture device~n'),
        fail
    ),

    % Start capture
    format('Starting 10-second capture buffer...~n'),
    capture_start(DeviceName, 10.0, Capture, _),
    capture_get_info(Capture, capture_info(_, _, SampleRate)),

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
    capture_stop(Capture),

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
    capture_extract(Capture, Offset, GrainLength, Data),
    sound_create(Data, Sound),

    % Random pitch: -12 to +12 semitones
    random_between(-12, 12, Pitch),
    PitchFloat is Pitch * 1.0,
    sound_set_pitch(Sound, PitchFloat),

    % Random pan via pan effect
    random_between(-10, 10, PanInt),
    Pan is PanInt / 10.0,
    sound_attach_effect(Sound, pan, [pan=Pan], _),

    % Random volume via VCA
    random_between(3, 10, VolInt),
    Vol is VolInt / 10.0,
    sound_attach_effect(Sound, vca, [gain=Vol], _),

    % Start with slight delay
    random_between(0, 100, DelayMs),
    Delay is DelayMs / 1000.0,
    sleep(Delay),
    sound_start(Sound),

    N1 is N - 1,
    spawn_grains(Capture, SampleRate, GrainLength, N1, [[Data, Sound]|Acc], Sounds).


cleanup_sounds([]).
cleanup_sounds([[Data, Sound]|Rest]) :-
    sound_unload(Sound),
    audio_unload(Data),
    cleanup_sounds(Rest).


% Run basic demo by default
main :-
    promini_init,
    demo_capture_basic.

% Or run granular version
main_granular :-
    promini_init,
    demo_capture_granular.
