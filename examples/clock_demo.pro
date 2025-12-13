/*
 * clock_demo.pro - Clock system demos
 *
 * Basic:
 *   demo_clock_basic         - Clock running and beat position
 *
 * Clock Routes:
 *   demo_lfo_sync            - LFO frequency synced to tempo
 *   demo_lfo_pulse           - LFO phase reset on beat
 *   demo_envelope_pulse      - Envelope triggered on beat
 *   demo_delay_sync          - Delay time synced to tempo
 *   demo_granular_pulse      - Grains triggered on beat
 *   demo_polyrhythm          - Multiple routes at different divisions
 *   demo_tempo_change        - BPM changes with synced effects
 *   demo_full_composition    - All clock features combined
 */

:- use_module('src/prolog/promini.pro').


/*
 * demo_clock_basic
 * Demonstrates the global clock running and beat position advancing.
 */
demo_clock_basic :-
    format('~n=== Basic Clock Demo ===~n~n'),

    format('Setting clock to 120 BPM...~n'),
    clock_set_bpm(120),

    format('Starting clock...~n'),
    clock_start,

    format('Watching beat position for 5 seconds:~n'),
    show_beats(10),

    format('~nChanging to 60 BPM...~n'),
    clock_set_bpm(60),
    show_beats(5),

    format('~nChanging to 180 BPM...~n'),
    clock_set_bpm(180),
    show_beats(5),

    format('~nStopping clock...~n'),
    clock_stop,

    format('Basic clock demo complete.~n~n').

show_beats(0) :- !.
show_beats(N) :-
    N > 0,
    clock_get_beat_position(Pos),
    format('  Beat position: ~2f~n', [Pos]),
    sleep(0.5),
    N1 is N - 1,
    show_beats(N1).


/*
 * demo_lfo_sync
 * Demonstrates LFO frequency locked to tempo divisions.
 * The LFO frequency is automatically updated when BPM changes.
 */
demo_lfo_sync :-
    format('~n=== LFO Sync Demo ===~n~n'),

    format('Creating synth voice with filter...~n'),
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 220.0, 0.6, _),
    synth_noise_add(Voice, white, 0.15, _),
    voice_attach_effect(Voice, moog, [cutoff=800.0, resonance=0.7], effect(_, FilterPtr)),

    format('Creating LFO for filter modulation...~n'),
    mod_lfo_init(sine, 1.0, LFO),
    mod_route_init(LFO, moog, FilterPtr, cutoff, absolute, 400.0, 800.0, 0.0, _Route),

    format('Setting clock to 120 BPM...~n'),
    clock_set_bpm(120),

    format('Syncing LFO to quarter notes (division=24)...~n'),
    clock_route_init(lfo, LFO, sync, 24, ClockRoute),

    synth_voice_start(Voice),
    clock_start,

    format('~nLFO synced to quarter notes at 120 BPM (2 Hz)...~n'),
    sleep(4.0),

    format('Changing to 8th notes (division=12)...~n'),
    clock_route_uninit(ClockRoute),
    clock_route_init(lfo, LFO, sync, 12, ClockRoute2),
    sleep(4.0),

    format('Changing to 16th notes (division=6)...~n'),
    clock_route_uninit(ClockRoute2),
    clock_route_init(lfo, LFO, sync, 6, ClockRoute3),
    sleep(4.0),

    format('~nChanging BPM to 90 (LFO follows automatically)...~n'),
    clock_set_bpm(90),
    sleep(4.0),

    format('Changing BPM to 150...~n'),
    clock_set_bpm(150),
    sleep(4.0),

    format('~nCleaning up...~n'),
    clock_stop,
    clock_route_uninit(ClockRoute3),
    mod_source_uninit(LFO),
    synth_voice_uninit(Voice),
    format('LFO sync demo complete.~n~n').


/*
 * demo_lfo_pulse
 * Demonstrates LFO phase reset (hard sync) on clock pulses.
 * Creates rhythmic "restart" of the LFO waveform.
 */
demo_lfo_pulse :-
    format('~n=== LFO Pulse (Hard Sync) Demo ===~n~n'),

    format('Creating synth voice...~n'),
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 110.0, 0.5, Osc),
    synth_oscillator_add(Voice, 220.0, 0.3, _),

    format('Creating triangle LFO (0.7 Hz, not synced to tempo)...~n'),
    mod_lfo_init(triangle, 0.7, LFO),
    mod_route_init(LFO, oscillator, Osc, volume, absolute, 0.4, 0.3, 0.0, _Route),

    clock_set_bpm(120),

    format('~n--- LFO running freely (no sync) ---~n'),
    synth_voice_start(Voice),
    sleep(4.0),

    format('~n--- LFO reset on every beat (division=24) ---~n'),
    format('Notice the rhythmic "restart" of the volume sweep.~n'),
    clock_route_init(lfo, LFO, pulse, 24, ClockRoute),
    clock_start,
    sleep(6.0),

    format('~n--- LFO reset on every half note (division=48) ---~n'),
    clock_route_uninit(ClockRoute),
    clock_route_init(lfo, LFO, pulse, 48, ClockRoute2),
    sleep(6.0),

    format('~n--- LFO reset on every 8th note (division=12) ---~n'),
    clock_route_uninit(ClockRoute2),
    clock_route_init(lfo, LFO, pulse, 12, ClockRoute3),
    sleep(6.0),

    format('~nCleaning up...~n'),
    clock_stop,
    clock_route_uninit(ClockRoute3),
    mod_source_uninit(LFO),
    synth_voice_uninit(Voice),
    format('LFO pulse demo complete.~n~n').


/*
 * demo_envelope_pulse
 * Demonstrates envelope triggering on clock pulses.
 * Creates rhythmic amplitude patterns.
 */
demo_envelope_pulse :-
    format('~n=== Envelope Pulse Demo ===~n~n'),

    format('Creating synth voice with VCA...~n'),
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 1.0, _),
    synth_oscillator_add(Voice, 880.0, 0.5, _),
    voice_attach_effect(Voice, vca, [gain=0.0], effect(_, VCAPtr)),
    voice_attach_effect(Voice, reverb, [wet=0.3, decay=0.6], _),

    format('Creating ADBR envelope (fast attack, medium decay)...~n'),
    % mod_envelope_init(Attack, Decay, Break, BreakLevel, Release, Duration, Loop, Env)
    mod_envelope_init(0.05, 0.2, 0.35, 0.4, 0.4, 400.0, false, Env),
    mod_route_init(Env, vca, VCAPtr, gain, absolute, 0.8, 0.0, 0.0, _Route),

    clock_set_bpm(120),

    format('~n--- Envelope triggered on quarter notes ---~n'),
    clock_route_init(envelope, Env, pulse, 24, ClockRoute),
    synth_voice_start(Voice),
    clock_start,
    sleep(4.0),

    format('~n--- Envelope triggered on 8th notes ---~n'),
    clock_route_uninit(ClockRoute),
    clock_route_init(envelope, Env, pulse, 12, ClockRoute2),
    sleep(4.0),

    format('~n--- Envelope triggered on 16th notes ---~n'),
    clock_route_uninit(ClockRoute2),
    clock_route_init(envelope, Env, pulse, 6, ClockRoute3),
    sleep(4.0),

    format('~n--- Changing to faster tempo (160 BPM) ---~n'),
    clock_set_bpm(160),
    sleep(4.0),

    format('~n--- Triplet feel (division=8, 8th note triplets) ---~n'),
    clock_route_uninit(ClockRoute3),
    clock_route_init(envelope, Env, pulse, 8, ClockRoute4),
    sleep(4.0),

    format('~nCleaning up...~n'),
    clock_stop,
    clock_route_uninit(ClockRoute4),
    mod_source_uninit(Env),
    synth_voice_uninit(Voice),
    format('Envelope pulse demo complete.~n~n').


/*
 * demo_delay_sync
 * Demonstrates delay time locked to tempo.
 * Delay time automatically updates when BPM changes.
 */
demo_delay_sync :-
    format('~n=== Delay Sync Demo ===~n~n'),

    format('Loading sound...~n'),
    sound_load('audio/counting.wav', Sound),

    format('Adding ping-pong delay...~n'),
    sound_attach_effect(Sound, ping_pong_delay, [
        max_delay_in_frames=96000,
        delay_in_frames=22050,
        feedback=0.5,
        wet=0.6
    ], Delay),

    clock_set_bpm(120),

    format('~n--- Delay synced to quarter notes (120 BPM = 0.5s) ---~n'),
    clock_route_init(ping_pong_delay, Delay, sync, 24, ClockRoute),
    clock_start,
    sound_loop(Sound),
    sound_start(Sound),
    sleep(6.0),

    format('~n--- Changing BPM to 90 (delay becomes 0.667s) ---~n'),
    clock_set_bpm(90),
    sleep(6.0),

    format('~n--- Changing BPM to 150 (delay becomes 0.4s) ---~n'),
    clock_set_bpm(150),
    sleep(6.0),

    format('~n--- Syncing to 8th notes instead (half the time) ---~n'),
    clock_route_uninit(ClockRoute),
    clock_route_init(ping_pong_delay, Delay, sync, 12, ClockRoute2),
    sleep(6.0),

    format('~n--- Dotted 8th notes (division=18) ---~n'),
    clock_route_uninit(ClockRoute2),
    clock_route_init(ping_pong_delay, Delay, sync, 18, ClockRoute3),
    sleep(6.0),

    format('~nCleaning up...~n'),
    clock_stop,
    sound_stop(Sound),
    clock_route_uninit(ClockRoute3),
    sound_unload(Sound),
    format('Delay sync demo complete.~n~n').


/*
 * demo_granular_pulse
 * Demonstrates grain triggering synced to clock.
 * Creates rhythmic granular textures.
 */
demo_granular_pulse :-
    format('~n=== Granular Pulse Demo ===~n~n'),

    format('Loading and recording source...~n'),
    sound_load('audio/gong.wav', Sound),

    granular_init(4.0, G),
    granular_connect(G, Sound),
    granular_set(G, [
        recording=true,
        normalize=true,
        density=0.0,  % No automatic triggering
        size=150.0,
        envelope=0.5,
        pan_spray=0.6,
        position=0.3,
        position_spray=0.2
    ]),

    granular_attach_effect(G, reverb, [wet=0.25, decay=0.7], _),

    format('Recording gong...~n'),
    sound_start(Sound),
    sleep(3.0),
    sound_stop(Sound),
    granular_set(G, [recording=false]),

    clock_set_bpm(120),

    format('~n--- Grains triggered on quarter notes ---~n'),
    clock_route_init(granular, G, pulse, 24, ClockRoute),
    clock_start,
    sleep(6.0),

    format('~n--- Grains triggered on 8th notes ---~n'),
    clock_route_uninit(ClockRoute),
    clock_route_init(granular, G, pulse, 12, ClockRoute2),
    sleep(6.0),

    format('~n--- Grains triggered on 16th notes ---~n'),
    clock_route_uninit(ClockRoute2),
    clock_route_init(granular, G, pulse, 6, ClockRoute3),
    sleep(6.0),

    format('~n--- Adding pitch variation ---~n'),
    granular_set_mode(G, [0.0, 3.0, 7.0, 10.0], 0, 12),
    sleep(6.0),

    format('~n--- Faster tempo (160 BPM), triplets ---~n'),
    clock_set_bpm(160),
    clock_route_uninit(ClockRoute3),
    clock_route_init(granular, G, pulse, 8, ClockRoute4),
    sleep(6.0),

    format('~nCleaning up...~n'),
    clock_stop,
    clock_route_uninit(ClockRoute4),
    granular_uninit(G),
    sound_unload(Sound),
    format('Granular pulse demo complete.~n~n').


/*
 * demo_polyrhythm
 * Demonstrates multiple clock routes at different divisions
 * creating polyrhythmic textures.
 */
demo_polyrhythm :-
    format('~n=== Polyrhythm Demo ===~n~n'),

    format('Setting up three voices at different divisions...~n'),

    % Voice 1: Low drone, triggered on whole notes
    synth_voice_init(Voice1),
    synth_oscillator_add(Voice1, 55.0, 1.0, _),
    voice_attach_effect(Voice1, vca, [gain=0.0], effect(_, VCA1)),
    mod_envelope_init(0.1, 0.2, 0.4, 0.5, 0.3, 2000.0, false, Env1),
    mod_route_init(Env1, vca, VCA1, gain, absolute, 0.6, 0.0, 0.0, _),

    % Voice 2: Mid tone, triggered on quarter notes
    synth_voice_init(Voice2),
    synth_oscillator_add(Voice2, 220.0, 1.0, _),
    voice_attach_effect(Voice2, vca, [gain=0.0], effect(_, VCA2)),
    voice_attach_effect(Voice2, lpf, [cutoff=1500.0], _),
    mod_envelope_init(0.05, 0.15, 0.4, 0.3, 0.4, 400.0, false, Env2),
    mod_route_init(Env2, vca, VCA2, gain, absolute, 0.4, 0.0, 0.0, _),

    % Voice 3: High ping, triggered on dotted 8ths (3 against 4)
    synth_voice_init(Voice3),
    synth_oscillator_add(Voice3, 880.0, 1.0, _),
    voice_attach_effect(Voice3, vca, [gain=0.0], effect(_, VCA3)),
    mod_envelope_init(0.02, 0.1, 0.4, 0.0, 0.48, 200.0, false, Env3),
    mod_route_init(Env3, vca, VCA3, gain, absolute, 0.25, 0.0, 0.0, _),

    % Master reverb via summing node
    summing_node_init(Sum),
    summing_node_connect(Sum, Voice1),
    summing_node_connect(Sum, Voice2),
    summing_node_connect(Sum, Voice3),
    summing_node_attach_effect(Sum, reverb, [wet=0.3, decay=0.75], _),

    clock_set_bpm(100),

    format('~n--- Creating polyrhythmic pattern ---~n'),
    format('Voice 1 (low): whole notes (division=96)~n'),
    format('Voice 2 (mid): quarter notes (division=24)~n'),
    format('Voice 3 (high): dotted 8ths (division=18) - 3 against 4~n~n'),

    clock_route_init(envelope, Env1, pulse, 96, Route1),
    clock_route_init(envelope, Env2, pulse, 24, Route2),
    clock_route_init(envelope, Env3, pulse, 18, Route3),

    synth_voice_start(Voice1),
    synth_voice_start(Voice2),
    synth_voice_start(Voice3),
    clock_start,

    sleep(12.0),

    format('~n--- Changing high voice to triplets (8th note triplets) ---~n'),
    clock_route_uninit(Route3),
    clock_route_init(envelope, Env3, pulse, 8, Route3b),
    sleep(8.0),

    format('~n--- Speeding up to 130 BPM ---~n'),
    clock_set_bpm(130),
    sleep(8.0),

    format('~nCleaning up...~n'),
    clock_stop,
    clock_route_uninit(Route1),
    clock_route_uninit(Route2),
    clock_route_uninit(Route3b),
    mod_source_uninit(Env1),
    mod_source_uninit(Env2),
    mod_source_uninit(Env3),
    synth_voice_uninit(Voice1),
    synth_voice_uninit(Voice2),
    synth_voice_uninit(Voice3),
    summing_node_uninit(Sum),
    format('Polyrhythm demo complete.~n~n').


/*
 * demo_tempo_change
 * Demonstrates how synced elements respond to tempo changes.
 */
demo_tempo_change :-
    format('~n=== Tempo Change Demo ===~n~n'),

    format('Setting up synced LFO, envelope, and delay...~n'),

    % Synth with filter
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 110.0, 1.0, _),
    synth_oscillator_add(Voice, 111.0, 0.6, _),  % Slight detune
    voice_attach_effect(Voice, moog, [cutoff=600.0, resonance=0.6], effect(_, FilterPtr)),
    voice_attach_effect(Voice, vca, [gain=0.0], effect(_, VCAPtr)),
    voice_attach_effect(Voice, ping_pong_delay, [
        max_delay_in_frames=96000,
        delay_in_frames=22050,
        feedback=0.4,
        wet=0.4
    ], Delay),
    voice_attach_effect(Voice, reverb, [wet=0.2, decay=0.6], _),

    % LFO for filter (synced to 8th notes)
    mod_lfo_init(triangle, 1.0, LFO),
    mod_route_init(LFO, moog, FilterPtr, cutoff, absolute, 1000.0, 600.0, 0.0, _),

    % Envelope for VCA (triggered on quarter notes)
    mod_envelope_init(0.05, 0.2, 0.35, 0.4, 0.4, 600.0, false, Env),
    mod_route_init(Env, vca, VCAPtr, gain, absolute, 0.7, 0.0, 0.0, _),

    % Clock routes
    clock_route_init(lfo, LFO, sync, 12, LFORoute),        % 8th notes
    clock_route_init(envelope, Env, pulse, 24, EnvRoute), % Quarter notes
    clock_route_init(ping_pong_delay, Delay, sync, 24, DelayRoute),  % Quarter note delay

    synth_voice_start(Voice),

    format('~n--- Starting at 80 BPM (slow) ---~n'),
    clock_set_bpm(80),
    clock_start,
    sleep(6.0),

    format('~n--- Accelerating to 100 BPM ---~n'),
    clock_set_bpm(100),
    sleep(6.0),

    format('~n--- Accelerating to 120 BPM ---~n'),
    clock_set_bpm(120),
    sleep(6.0),

    format('~n--- Accelerating to 140 BPM ---~n'),
    clock_set_bpm(140),
    sleep(6.0),

    format('~n--- Accelerating to 160 BPM ---~n'),
    clock_set_bpm(160),
    sleep(6.0),

    format('~n--- Slowing back down to 90 BPM ---~n'),
    clock_set_bpm(90),
    sleep(6.0),

    format('~nCleaning up...~n'),
    clock_stop,
    clock_route_uninit(LFORoute),
    clock_route_uninit(EnvRoute),
    clock_route_uninit(DelayRoute),
    mod_source_uninit(LFO),
    mod_source_uninit(Env),
    synth_voice_uninit(Voice),
    format('Tempo change demo complete.~n~n').


/*
 * demo_full_composition
 * A musical piece using all clock routing features together.
 */
demo_full_composition :-
    format('~n=== Full Composition Demo ===~n~n'),
    format('A generative piece using synced LFOs, envelopes, delay, and granular.~n~n'),

    % Load source for granular
    sound_load('audio/gong.wav', GongSound),

    % --- Granular layer ---
    granular_init(4.0, Gran),
    granular_connect(Gran, GongSound),
    granular_set(Gran, [
        recording=true,
        normalize=true,
        density=0.0,
        size=200.0,
        envelope=0.5,
        pan_spray=0.7,
        position=0.4,
        position_spray=0.25
    ]),
    granular_attach_effect(Gran, reverb, [
        wet=0.35,
        decay=0.8,
        shimmer1_shift=12.0,
        shimmer1_mix=0.1
    ], _),

    % Record gong
    format('Recording gong for granular...~n'),
    sound_start(GongSound),
    sleep(3.0),
    sound_stop(GongSound),
    granular_set(Gran, [recording=false]),

    % Set scale for granular
    granular_set_mode(Gran, [0.0, 2.0, 3.0, 5.0, 7.0, 8.0, 10.0], 5, 12),

    % --- Bass synth ---
    synth_voice_init(Bass),
    synth_oscillator_add(Bass, 55.0, 1.0, _),
    synth_oscillator_add(Bass, 110.0, 0.5, _),
    voice_attach_effect(Bass, lpf, [cutoff=400.0, order=2], _),
    voice_attach_effect(Bass, vca, [gain=0.0], effect(_, BassVCAPtr)),

    mod_envelope_init(0.1, 0.2, 0.3, 0.4, 0.4, 1500.0, false, BassEnv),
    mod_route_init(BassEnv, vca, BassVCAPtr, gain, absolute, 0.5, 0.0, 0.0, _),

    % --- Pad synth with filter modulation ---
    synth_voice_init(Pad),
    synth_oscillator_add(Pad, 220.0, 0.4, _),
    synth_oscillator_add(Pad, 330.0, 0.3, _),
    synth_oscillator_add(Pad, 440.0, 0.2, _),
    voice_attach_effect(Pad, moog, [cutoff=800.0, resonance=0.5], effect(_, PadFilterPtr)),
    voice_attach_effect(Pad, vca, [gain=0.3], _),
    voice_attach_effect(Pad, ping_pong_delay, [
        max_delay_in_frames=96000,
        delay_in_frames=22050,
        feedback=0.35,
        wet=0.3
    ], PadDelay),

    mod_lfo_init(sine, 1.0, PadLFO),
    mod_route_init(PadLFO, moog, PadFilterPtr, cutoff, absolute, 800.0, 600.0, 0.0, _),

    % --- High pluck synth ---
    synth_voice_init(Pluck),
    synth_oscillator_add(Pluck, 880.0, 1.0, _),
    voice_attach_effect(Pluck, hpf, [cutoff=500.0], _),
    voice_attach_effect(Pluck, vca, [gain=0.0], effect(_, PluckVCAPtr)),

    mod_envelope_init(0.02, 0.15, 0.4, 0.0, 0.43, 250.0, false, PluckEnv),
    mod_route_init(PluckEnv, vca, PluckVCAPtr, gain, absolute, 0.2, 0.0, 0.0, _),

    % --- Master bus ---
    summing_node_init(Master),
    summing_node_connect(Master, Bass),
    summing_node_connect(Master, Pad),
    summing_node_connect(Master, Pluck),
    summing_node_attach_effect(Master, compressor, [
        threshold=0.5,
        ratio=3.0,
        attack_ms=10.0,
        release_ms=100.0
    ], _),

    % --- Clock routes ---
    clock_set_bpm(95),

    % Granular: 8th notes
    clock_route_init(granular, Gran, pulse, 12, GranRoute),
    % Bass: half notes
    clock_route_init(envelope, BassEnv, pulse, 48, BassRoute),
    % Pad LFO: synced to whole notes
    clock_route_init(lfo, PadLFO, sync, 96, PadLFORoute),
    % Pad delay: quarter notes
    clock_route_init(ping_pong_delay, PadDelay, sync, 24, PadDelayRoute),
    % Pluck: dotted 8ths (3 against 4)
    clock_route_init(envelope, PluckEnv, pulse, 18, PluckRoute),

    % --- Performance ---
    format('~n--- Section 1: Sparse introduction ---~n'),
    synth_voice_start(Bass),
    synth_voice_start(Pad),
    clock_start,
    sleep(8.0),

    format('~n--- Section 2: Adding granular texture ---~n'),
    sleep(8.0),

    format('~n--- Section 3: Adding pluck arpeggios ---~n'),
    synth_voice_start(Pluck),
    sleep(8.0),

    format('~n--- Section 4: Increasing tempo to 110 ---~n'),
    clock_set_bpm(110),
    sleep(8.0),

    format('~n--- Section 5: Dense granular (16th notes) ---~n'),
    clock_route_uninit(GranRoute),
    clock_route_init(granular, Gran, pulse, 6, GranRoute2),
    sleep(8.0),

    format('~n--- Section 6: Breakdown (slower, sparser) ---~n'),
    clock_set_bpm(80),
    clock_route_uninit(GranRoute2),
    clock_route_init(granular, Gran, pulse, 24, GranRoute3),
    sleep(10.0),

    format('~n--- Fade out ---~n'),
    granular_set(Gran, [density=0.0]),
    sleep(4.0),

    format('~nCleaning up...~n'),
    clock_stop,

    % Cleanup routes
    clock_route_uninit(GranRoute3),
    clock_route_uninit(BassRoute),
    clock_route_uninit(PadLFORoute),
    clock_route_uninit(PadDelayRoute),
    clock_route_uninit(PluckRoute),

    % Cleanup mod sources
    mod_source_uninit(BassEnv),
    mod_source_uninit(PadLFO),
    mod_source_uninit(PluckEnv),

    % Cleanup audio
    synth_voice_uninit(Bass),
    synth_voice_uninit(Pad),
    synth_voice_uninit(Pluck),
    summing_node_uninit(Master),
    granular_uninit(Gran),
    sound_unload(GongSound),

    format('Full composition demo complete.~n~n').


/*
 * Division reference (at 24 PPQN):
 *   6  = 16th note
 *   8  = 8th note triplet
 *  12  = 8th note
 *  16  = quarter note triplet
 *  18  = dotted 8th note
 *  24  = quarter note
 *  32  = dotted quarter note
 *  36  = half note triplet
 *  48  = half note
 *  72  = dotted half note
 *  96  = whole note
 */
