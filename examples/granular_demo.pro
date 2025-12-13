:- use_module('src/prolog/promini.pro').

/*
 * Granular Delay Demos
 *
 * demo_granular_file          - Granulate guitar.wav with various settings
 * demo_granular_live_reverb   - Live mic input with wide stereo and reverb
 * demo_granular_live          - Granulate live microphone input
 * demo_granular_texture       - Create evolving granular textures
 * demo_granular_freeze        - Freeze and granulate a moment in time
 * demo_granular_normalization - Compare normalization vs compression
 * demo_granular_trigger       - Manual grain triggering
 * demo_granular_partial_buffer - Demonstrate click-free partial buffer granulation
 * demo_dminor_gong            - Melodic D minor piece with ping-pong and shimmer
 * demo_granular_mode          - Pitch quantization to musical scales
 */

demo_granular_file :-
    format('~n=== Granular Delay Demo (File) ===~n~n'),

    format('Loading guitar.wav...~n'),
    sound_load('audio/guitar.wav', Sound),
    sound_loop(Sound),

    format('Creating granular delay (4 second buffer)...~n'),
    granular_init(4.0, G),
    granular_connect(G, Sound),
    granular_set(G, [recording=true, normalize=true]),

    format('Starting source playback...~n'),
    sound_start(Sound),
    sleep(4.0),  % let buffer fill

    format('~n--- Sparse grains (2/sec, 200ms) ---~n'),
    granular_set(G, [
        density=2.0,
        size=200.0,
        position=0.5,
        envelope=0.5
    ]),
    sleep(10.0),

    format('~n--- Dense cloud (20/sec, 50ms) ---~n'),
    granular_set(G, [
        density=20.0,
        size=50.0,
        size_spray=20.0,
        position_spray=0.3
    ]),
    sleep(10.0),

    format('~n--- Pitch shifted up (+7 semitones) ---~n'),
    granular_set(G, [
        density=10.0,
        size=100.0,
        pitch=7.0
    ]),
    sleep(10.0),

    format('~n--- Pitch shifted down (-12 semitones) ---~n'),
    granular_set(G, [pitch=(-12.0)]),
    sleep(10.0),

    format('~n--- Wide stereo spread ---~n'),
    granular_set(G, [
        pitch=0.0,
        pan_spray=1.0
    ]),
    sleep(10.0),

    format('~n--- Reverse grains (50%% probability) ---~n'),
    granular_set(G, [
        reverse_probability=0.5,
        density=8.0,
        size=150.0
    ]),
    sleep(10.0),

    format('~n--- Irregular timing (low regularity) ---~n'),
    granular_set(G, [
        regularity=0.2,
        reverse_probability=0.3
    ]),
    sleep(10.0),

    format('~nCleaning up...~n'),
    sound_stop(Sound),
    granular_uninit(G),
    sound_unload(Sound),
    format('Granular file demo complete.~n~n').


/*
 * demo_granular_live_reverb
 * Record from microphone continuously while granulating
 * with wide stereo spread and reverb.
 */
demo_granular_live_reverb :-
    format('~n=== Live Granular with Reverb ===~n~n'),

    % Find capture device
    promini_devices(Devices),
    (   member(device(DeviceName, capture, _), Devices)
    ->  format('Using capture device: ~w~n', [DeviceName])
    ;   format('ERROR: No capture device found~n'), fail
    ),

    format('Starting capture (6 second buffer)...~n'),
    capture_start(DeviceName, 6.0, Capture, BufferFrames),
    format('Buffer: ~w frames~n', [BufferFrames]),

    format('Creating granular delay...~n'),
    granular_init(6.0, G),
    granular_connect(G, Capture),
    granular_set(G, [recording=true, normalize=true, density=0.0]),

    % Add reverb with shimmer
    granular_attach_effect(G, reverb, [
        wet=0.4,
        decay=0.85,
        damping=0.3,
        width=1.5,
        shimmer1_shift=12.0,
        shimmer1_mix=0.15
    ], _),

    format('~n*** SPEAK OR MAKE SOUNDS INTO MICROPHONE ***~n'),
    format('Filling buffer...~n~n'),
    sleep(6.0),

    format('Starting granulation (Ctrl-C to stop)...~n~n'),
    granular_set(G, [
        density=12.0,
        size=150.0,
        size_spray=50.0,
        position=0.4,
        position_spray=0.3,
        envelope=0.5,
        pan_spray=1.0,
        reverse_probability=0.2
    ]),

    % Aeolian mode (natural minor): W-H-W-W-H-W-W
    % Range: 2 semitones down, 12 semitones up
    granular_set_mode(G, [0.0, 2.0, 3.0, 5.0, 7.0, 8.0, 10.0], 2, 12),

    % Run until interrupted
    catch(
        live_granular_loop(G),
        _,
        true
    ),

    format('~nCleaning up...~n'),
    granular_uninit(G),
    capture_stop(Capture),
    format('Live reverb demo complete.~n~n').

live_granular_loop(G) :-
    sleep(30.0),
    live_granular_loop(G).


demo_granular_live :-
    format('~n=== Granular Delay Demo (Live Input) ===~n~n'),

    % Find capture device
    promini_devices(Devices),
    (   member(device(DeviceName, capture, _), Devices)
    ->  format('Using capture device: ~w~n', [DeviceName])
    ;   format('ERROR: No capture device found~n'), fail
    ),

    format('Starting capture (4 second buffer)...~n'),
    capture_start(DeviceName, 4.0, Capture, BufferFrames),
    format('Buffer: ~w frames~n', [BufferFrames]),

    format('Creating granular delay...~n'),
    granular_init(4.0, G),
    granular_connect(G, Capture),
    granular_set(G, [recording=true, normalize=true]),

    format('~n*** SPEAK OR MAKE SOUNDS INTO MICROPHONE ***~n~n'),
    sleep(4.0),  % let buffer fill

    format('--- Real-time granulation (low latency) ---~n'),
    granular_set(G, [
        density=15.0,
        size=80.0,
        position=0.05,  % read very recent audio
        position_spray=0.02,
        envelope=0.5,
        pan_spray=0.5
    ]),
    sleep(5.0),

    format('~n--- Delayed granulation (0.5s behind) ---~n'),
    granular_set(G, [
        position=0.125,  % ~0.5s into 4s buffer
        position_spray=0.05
    ]),
    sleep(5.0),

    format('~n--- Pitch cloud (+/- random via spray) ---~n'),
    granular_set(G, [
        density=12.0,
        size=60.0,
        position=0.2,
        pan_spray=0.8
    ]),
    sleep(5.0),

    format('~n--- Glitchy texture (small grains, irregular) ---~n'),
    granular_set(G, [
        density=30.0,
        size=30.0,
        size_spray=15.0,
        regularity=0.1,
        reverse_probability=0.4
    ]),
    sleep(5.0),

    format('~nCleaning up...~n'),
    granular_uninit(G),
    capture_stop(Capture),
    format('Granular live demo complete.~n~n').


demo_granular_texture :-
    format('~n=== Granular Texture Demo ===~n~n'),

    format('Loading gong.wav for sustained texture...~n'),
    sound_load('audio/gong.wav', Sound),

    granular_init(6.0, G),
    granular_connect(G, Sound),
    granular_set(G, [recording=true, normalize=true]),

    format('Playing source once to fill buffer...~n'),
    sound_start(Sound),
    sleep(3.0),
    sound_stop(Sound),

    format('Stopping source, granulating buffer only...~n'),
    granular_set(G, [recording=false]),  % stop recording, freeze buffer

    format('~n--- Shimmering pad texture ---~n'),
    granular_set(G, [
        density=25.0,
        size=120.0,
        size_spray=40.0,
        position=0.3,
        position_spray=0.4,
        pitch=0.0,
        envelope=0.5,
        pan_spray=0.9,
        regularity=0.6
    ]),
    sleep(6.0),

    format('~n--- Slowly scanning through buffer ---~n'),
    scan_position(G, 0.1, 0.9, 40),

    format('~n--- Octave up texture ---~n'),
    granular_set(G, [
        pitch=12.0,
        density=20.0,
        size=80.0,
        position=0.25,
        position_spray=0.2
    ]),
    sleep(5.0),

    format('~n--- Sub-bass drone ---~n'),
    granular_set(G, [
        pitch=(-24.0),
        density=8.0,
        size=300.0,
        size_spray=50.0,
        pan_spray=0.3
    ]),
    sleep(5.0),

    format('~nCleaning up...~n'),
    granular_uninit(G),
    sound_unload(Sound),
    format('Granular texture demo complete.~n~n').


demo_granular_freeze :-
    format('~n=== Granular Freeze Demo ===~n~n'),

    format('Loading counting.wav...~n'),
    sound_load('audio/counting.wav', Sound),

    granular_init(2.0, G),
    granular_connect(G, Sound),
    granular_set(G, [recording=true, density=0.0, normalize=true]),

    format('Playing source...~n'),
    sound_start(Sound),
    sleep(1.5),

    format('~n*** FREEZING AUDIO ***~n'),
    granular_set(G, [recording=false]),  % freeze buffer
    sound_stop(Sound),

    format('~n--- Granulating frozen moment ---~n'),
    granular_set(G, [
        density=15.0,
        size=100.0,
        position=0.5,
        position_spray=0.1,
        envelope=0.5
    ]),
    sleep(4.0),

    format('~n--- Stretching time (long grains) ---~n'),
    granular_set(G, [
        density=6.0,
        size=400.0,
        size_spray=100.0,
        position_spray=0.05
    ]),
    sleep(5.0),

    format('~n--- Pitch cascade ---~n'),
    pitch_cascade(G),

    format('~nCleaning up...~n'),
    granular_uninit(G),
    sound_unload(Sound),
    format('Granular freeze demo complete.~n~n').


% Helper: gradually scan position through buffer
scan_position(G, Start, End, Steps) :-
    Step is (End - Start) / Steps,
    scan_position_(G, Start, End, Step).

scan_position_(_, Pos, End, Step) :-
    Step > 0, Pos >= End, !.
scan_position_(_, Pos, End, Step) :-
    Step < 0, Pos =< End, !.
scan_position_(G, Pos, End, Step) :-
    granular_set(G, [position=Pos]),
    sleep(0.15),
    NextPos is Pos + Step,
    scan_position_(G, NextPos, End, Step).


% Helper: cascade through pitches
pitch_cascade(G) :-
    format('Pitch: 0 -> +12 -> -12 -> 0~n'),
    sweep_pitch(G, 0.0, 12.0, 15),
    sweep_pitch(G, 12.0, -12.0, 30),
    sweep_pitch(G, -12.0, 0.0, 15).

sweep_pitch(G, Start, End, Steps) :-
    Step is (End - Start) / Steps,
    sweep_pitch_(G, Start, End, Step).

sweep_pitch_(_, Pitch, End, Step) :-
    Step > 0, Pitch >= End, !.
sweep_pitch_(_, Pitch, End, Step) :-
    Step < 0, Pitch =< End, !.
sweep_pitch_(G, Pitch, End, Step) :-
    granular_set(G, [pitch=Pitch]),
    sleep(0.12),
    NextPitch is Pitch + Step,
    sweep_pitch_(G, NextPitch, End, Step).


/*
 * demo_granular_normalization
 * Compare built-in normalization vs compressor for managing grain overlap levels.
 */
demo_granular_normalization :-
    format('~n=== Granular Normalization vs Compression Demo ===~n~n'),

    format('Loading guitar.wav...~n'),
    sound_load('audio/guitar.wav', Sound),
    sound_loop(Sound),

    granular_init(4.0, G),
    granular_connect(G, Sound),
    granular_set(G, [recording=true]),

    format('Starting source playback to fill buffer...~n'),
    sound_start(Sound),
    sleep(4.0),
    sound_stop(Sound),
    granular_set(G, [recording=false]),
    format('Buffer filled, source stopped.~n'),

    format('~n--- Baseline: low density (5/sec), normalization ON ---~n'),
    granular_set(G, [
        normalize=true,
        density=5.0,
        size=100.0,
        position=0.5,
        position_spray=0.3,
        size_spray=20.0,
        envelope=0.5,
        pan_spray=0.5
    ]),
    sleep(4.0),

    format('~n--- Increasing density: 5 -> 15 -> 40 -> 80 ---~n'),
    format('With normalization, level stays consistent.~n'),
    format('density=5~n'),
    sleep(3.0),
    granular_set(G, [density=15.0]),
    format('density=15~n'),
    sleep(3.0),
    granular_set(G, [density=40.0]),
    format('density=40~n'),
    sleep(3.0),
    granular_set(G, [density=80.0]),
    format('density=80~n'),
    sleep(3.0),

    format('~n--- Same density sweep, normalization OFF ---~n'),
    format('WARNING: Gets progressively louder!~n'),
    granular_set(G, [normalize=false, density=5.0]),
    format('density=5~n'),
    sleep(3.0),
    granular_set(G, [density=15.0]),
    format('density=15~n'),
    sleep(3.0),
    granular_set(G, [density=40.0]),
    format('density=40~n'),
    sleep(3.0),
    granular_set(G, [density=80.0]),
    format('density=80 (loud!)~n'),
    sleep(3.0),

    format('~n--- Compressor instead of normalization ---~n'),
    format('Compressor reacts to peaks, adds pumping character.~n'),
    granular_set(G, [density=20.0]),
    granular_attach_effect(G, compressor, [
        threshold=0.3,
        ratio=8.0,
        attack=5.0,
        release=100.0,
        knee=6.0
    ], _),
    sleep(6.0),

    format('~n--- Both normalization AND compressor ---~n'),
    format('Normalization handles overlap, compressor adds glue.~n'),
    granular_set(G, [normalize=true]),
    sleep(6.0),

    format('~n--- Envelope shape comparison ---~n'),
    format('Square envelope (0.0) - harsh edges~n'),
    granular_set(G, [density=0.0]),
    sleep(0.3),
    granular_set(G, [envelope=0.0, density=15.0]),
    sleep(3.0),
    format('Hann envelope (0.5) - smooth, natural~n'),
    granular_set(G, [density=0.0]),
    sleep(0.3),
    granular_set(G, [envelope=0.5, density=15.0]),
    sleep(3.0),
    format('Sawtooth envelope (1.0) - percussive attack~n'),
    granular_set(G, [density=0.0]),
    sleep(0.3),
    granular_set(G, [envelope=1.0, density=15.0]),
    sleep(3.0),

    format('~nCleaning up...~n'),
    granular_uninit(G),
    sound_unload(Sound),
    format('Normalization demo complete.~n~n').


% Manual trigger demo
demo_granular_trigger :-
    format('~n=== Manual Grain Trigger Demo ===~n~n'),

    sound_load('audio/gong.wav', Sound),
    granular_init(3.0, G),
    granular_connect(G, Sound),
    granular_set(G, [
        recording=true,
        normalize=true,
        density=0.0,  % no automatic triggering
        size=200.0,
        envelope=0.5,
        pan_spray=0.8
    ]),

    format('Recording gong...~n'),
    sound_start(Sound),
    sleep(2.0),
    sound_stop(Sound),
    granular_set(G, [recording=false]),

    format('~n--- Triggering grains manually ---~n'),
    format('Single grains:~n'),
    trigger_with_delay(G, 0.3, 5),

    format('~nBurst of grains:~n'),
    forall(between(1, 20, _), granular_trigger(G)),
    sleep(1.0),

    format('~nRhythmic pattern:~n'),
    rhythmic_triggers(G, 16),

    format('~nCleaning up...~n'),
    granular_uninit(G),
    sound_unload(Sound),
    format('Manual trigger demo complete.~n~n').


trigger_with_delay(_, _, 0) :- !.
trigger_with_delay(G, Delay, N) :-
    N > 0,
    granular_trigger(G),
    sleep(Delay),
    N1 is N - 1,
    trigger_with_delay(G, Delay, N1).


rhythmic_triggers(_, 0) :- !.
rhythmic_triggers(G, N) :-
    N > 0,
    granular_trigger(G),
    (   0 is N mod 4
    ->  sleep(0.3)  % longer pause every 4th
    ;   sleep(0.15)
    ),
    N1 is N - 1,
    rhythmic_triggers(G, N1).


/*
 * demo_granular_partial_buffer
 * Demonstrate granulation with partially filled buffer - no clicks.
 */
demo_granular_partial_buffer :-
    format('~n=== Granular Partial Buffer Demo ===~n~n'),

    format('Loading guitar.wav...~n'),
    sound_load('audio/guitar.wav', Sound),

    granular_init(8.0, G),
    granular_connect(G, Sound),
    granular_set(G, [recording=true, normalize=true, density=0.0]),
    granular_attach_effect(G, reverb, [wet=0.3, decay=0.8, shimmer1_shift=12.0, shimmer1_mix=0.15], _),

    format('Recording 1 second into 8 second buffer...~n'),
    sound_start(Sound),
    sleep(1.0),
    sound_stop(Sound),
    granular_set(G, [recording=false]),

    format('Playing grains from partial buffer (12.5%% filled)...~n~n'),
    granular_set(G, [
        density=10.0,
        size=100.0,
        position=0.05,
        position_spray=0.05,
        envelope=0.5,
        pan_spray=0.5
    ]),
    sleep(4.0),

    format('Recording 2 more seconds...~n'),
    granular_set(G, [recording=true]),
    sound_start(Sound),
    sleep(2.0),
    sound_stop(Sound),
    granular_set(G, [recording=false]),

    format('Playing grains from larger buffer (37.5%% filled)...~n~n'),
    granular_set(G, [
        position=0.15,
        position_spray=0.1
    ]),
    sleep(4.0),

    format('~nCleaning up...~n'),
    granular_uninit(G),
    sound_unload(Sound),
    format('Partial buffer demo complete.~n~n').


/*
 * demo_dminor_gong
 * Melodic granular piece in D minor using gong.wav
 * with ping-pong delay and shimmer reverb.
 */
demo_dminor_gong :-
    format('~n=== D Minor Gong Meditation ===~n~n'),

    sound_load('audio/gong.wav', Sound),
    granular_init(3.0, G),
    granular_connect(G, Sound),
    granular_set(G, [recording=true, normalize=true, density=0.0]),

    format('Recording gong...~n'),
    sound_start(Sound),
    sleep(3.0),
    sound_stop(Sound),
    granular_set(G, [recording=false]),

    % Add ping-pong delay for stereo width
    granular_attach_effect(G, ping_pong_delay, [
        max_delay_in_frames=48000,
        delay_in_frames=19200,
        feedback=0.45,
        wet=0.35,
        dry=0.7
    ], _),

    % Add shimmer reverb
    granular_attach_effect(G, reverb, [
        wet=0.3,
        decay=0.8,
        damping=0.3,
        width=1.5,
        shimmer1_shift=12.0,
        shimmer1_mix=0.15
    ], _),

    format('~n--- D minor arpeggios ---~n'),
    granular_set_mode(G, [0.0, 2.0, 3.0, 5.0, 7.0, 8.0, 10.0], 0, 7),
    granular_set(G, [
        density=6.0,
        size=180.0,
        position=0.3,
        position_spray=0.25,
        envelope=0.5,
        pan_spray=0.8,
        pitch=0.0
    ]),
    sleep(8.0),

    format('~n--- Rising intensity ---~n'),
    granular_set(G, [
        density=12.0,
        size=120.0,
        position_spray=0.35,
        pitch=7.0
    ]),
    sleep(8.0),

    format('~n--- Sparse low register ---~n'),
    granular_set(G, [
        density=4.0,
        size=250.0,
        pitch=(-5.0),
        pan_spray=0.5
    ]),
    sleep(8.0),

    format('~n--- Dense shimmer cloud ---~n'),
    granular_set(G, [
        density=20.0,
        size=80.0,
        pitch=12.0,
        position=0.4,
        position_spray=0.3,
        pan_spray=1.0
    ]),
    sleep(8.0),

    format('~n--- Fade to stillness ---~n'),
    fade_density(G, 20.0, 2.0, 20),
    sleep(4.0),

    format('~nCleaning up...~n'),
    granular_uninit(G),
    sound_unload(Sound),
    format('D Minor meditation complete.~n~n').

fade_density(_, _, _, 0) :- !.
fade_density(G, Current, Target, Steps) :-
    Steps > 0,
    granular_set(G, [density=Current]),
    sleep(0.3),
    Step is (Target - Current) / Steps,
    Next is Current + Step,
    S1 is Steps - 1,
    fade_density(G, Next, Target, S1).


/*
 * demo_granular_mode
 * Demonstrate pitch quantization to musical scales.
 */
demo_granular_mode :-
    format('~n=== Granular Mode (Scale) Demo ===~n~n'),

    format('Loading gong.wav...~n'),
    sound_load('audio/gong.wav', Sound),

    granular_init(4.0, G),
    granular_connect(G, Sound),
    granular_set(G, [recording=true, normalize=true, density=0.0]),

    % Add reverb with shimmer
    granular_attach_effect(G, reverb, [
        wet=0.25,
        decay=0.7,
        damping=0.4,
        width=1.2,
        shimmer1_shift=12.0,
        shimmer1_mix=0.1
    ], _),

    format('Recording source...~n'),
    sound_start(Sound),
    sleep(2.0),
    sound_stop(Sound),
    granular_set(G, [recording=false]),

    format('~n--- No mode (continuous pitch) ---~n'),
    granular_set(G, [
        density=8.0,
        size=150.0,
        position=0.3,
        position_spray=0.2,
        envelope=0.5,
        pan_spray=0.6
    ]),
    sleep(4.0),

    format('~n--- Major scale, root octave only ---~n'),
    granular_set_mode(G, [0.0, 2.0, 4.0, 5.0, 7.0, 9.0, 11.0], 0, 6),
    sleep(5.0),

    format('~n--- Major scale, two octaves ---~n'),
    granular_set_mode(G, [0.0, 2.0, 4.0, 5.0, 7.0, 9.0, 11.0], 0, 13),
    sleep(5.0),

    format('~n--- Minor pentatonic ---~n'),
    granular_set_mode(G, [0.0, 3.0, 5.0, 7.0, 10.0], 0, 9),
    sleep(5.0),

    format('~n--- Major triad (chord tones only) ---~n'),
    granular_set_mode(G, [0.0, 4.0, 7.0], 0, 8),
    granular_set(G, [density=12.0, size=100.0]),
    sleep(5.0),

    format('~n--- Whole tone scale ---~n'),
    granular_set_mode(G, [0.0, 2.0, 4.0, 6.0, 8.0, 10.0], 0, 11),
    sleep(5.0),

    format('~n--- Minor scale with octave below (deviation_down) ---~n'),
    granular_set_mode(G, [0.0, 2.0, 3.0, 5.0, 7.0, 8.0, 10.0], 7, 7),
    granular_set(G, [pitch=12.0]),  % shift tonic up so we hear the lower octave
    sleep(5.0),

    format('~n--- Chromatic (all semitones) ---~n'),
    granular_set_mode(G, [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0], 0, 23),
    granular_set(G, [pitch=0.0]),
    sleep(5.0),

    format('~n--- Disable mode (back to continuous) ---~n'),
    granular_set_mode(G, [], 0, 0),
    sleep(3.0),

    format('~nCleaning up...~n'),
    granular_uninit(G),
    sound_unload(Sound),
    format('Mode demo complete.~n~n').
