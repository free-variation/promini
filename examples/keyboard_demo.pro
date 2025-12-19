:- use_module('src/prolog/promini.pro').

:- dynamic demo_voice/1, demo_envelope/1, demo_granular/1, demo_sound/1, demo_summing/1.

demo_cleanup :-
    forall(demo_voice(V), ignore(synth_voice_uninit(V))),
    forall(demo_envelope(E), ignore(mod_source_uninit(E))),
    forall(demo_granular(G), ignore(granular_uninit(G))),
    forall(demo_summing(S), ignore(summing_node_uninit(S))),
    forall(demo_sound(Snd), ignore(sound_unload(Snd))),
    retractall(demo_voice(_)),
    retractall(demo_envelope(_)),
    retractall(demo_granular(_)),
    retractall(demo_summing(_)),
    retractall(demo_sound(_)).

demo_keyboard :-
    promini_init,
    control_init,
    sound_load('audio/gong.wav', Sound),
    granular_init(4.0, G),
    granular_connect(G, Sound),
    keyboard_init(K),
    keyboard_connect(K, 0, G),
    keyboard_connect(K, 1, G),
    keyboard_connect(K, 2, G),
    keyboard_connect(K, 3, G),
    format('Keyboard window opened. Press ESC to close.~n').

demo_keyboard_granular :-
    promini_init,
    control_init,

    sound_load('audio/gong.wav', Sound),
    granular_init(4.0, G),
    granular_connect(G, Sound),
    granular_set(G, [density=0.0, size=150.0, envelope=0.5, pitch=12.0]),

    granular_attach_effect(G, reverb, [wet=0.3, decay=0.8], _),

    keyboard_init(K),
    keyboard_connect(K, 0, G),
    keyboard_connect(K, 1, G),
    keyboard_connect(K, 2, G),
    keyboard_connect(K, 3, G),
    format('Keyboard connected. Play keys to trigger grains. ESC to exit.~n').

demo_keyboard_split :-
    promini_init,
    control_init,

    sound_load('audio/gong.wav', Gong),
    sound_load('audio/guitar.wav', Guitar),

    granular_init(4.0, G1),
    granular_connect(G1, Gong),
    granular_set(G1, [density=0.0, size=150.0, envelope=0.5, pitch=12.0, pan=(-0.5)]),

    granular_init(4.0, G2),
    granular_connect(G2, Guitar),
    granular_set(G2, [density=0.0, size=100.0, envelope=0.5, pitch=12.0, pan=0.5]),

    summing_node_init(Sum),
    summing_node_connect(Sum, G1),
    summing_node_connect(Sum, G2),
    summing_node_attach_effect(Sum, reverb, [wet=0.3, decay=0.8], _),
    summing_node_attach_effect(Sum, ping_pong_delay, [
        max_delay_in_frames=48000,
        delay_in_frames=12000,
        feedback=0.4,
        wet=0.3
    ], _),

    keyboard_init(K),
    keyboard_connect(K, 0, G1),
    keyboard_connect(K, 1, G1),
    keyboard_connect(K, 2, G2),
    keyboard_connect(K, 3, G2),
    format('Split keyboard: rows 0-1 = gong, rows 2-3 = guitar. ESC to exit.~n').

demo_keyboard_mod :-
    promini_init,
    control_init,

    sound_load('audio/gong.wav', Sound),
    granular_init(4.0, G),
    granular_connect(G, Sound),
    granular_set(G, [density=0.0, size=150.0, envelope=0.5, pitch=12.0]),

    granular_attach_effect(G, reverb, [wet=0.3, decay=0.8], _),

    keyboard_init(K),
    keyboard_connect(K, 0, G),
    keyboard_connect(K, 1, G),
    keyboard_connect(K, 2, G),
    keyboard_connect(K, 3, G),

    /* spacebar increases density while held */
    mod_keyboard_init(space, [attack=300, release=500], ModSpace),
    mod_route_init(ModSpace, granular, G, density, absolute, 15.0, 0.0, 0.0, _),

    /* up/down arrows bend pitch */
    mod_keyboard_init(up, [attack=100, release=300], ModUp),
    mod_keyboard_init(down, [attack=100, release=300], ModDown),
    mod_route_init(ModUp, granular, G, pitch, rate, 24.0, 0.0, 0.0, _),
    mod_route_init(ModDown, granular, G, pitch, rate, -24.0, 0.0, 0.0, _),

    format('Keyboard with mod sources:~n'),
    format('  - Play letter/number keys to trigger grains~n'),
    format('  - Hold SPACE for continuous grains~n'),
    format('  - UP arrow bends pitch up~n'),
    format('  - DOWN arrow bends pitch down~n'),
    format('  - ESC to exit~n').

demo_keyboard_scales :-
    promini_init,
    control_init,

    sound_load('audio/gong.wav', Sound),
    granular_init(4.0, G),
    granular_connect(G, Sound),
    granular_set(G, [density=0.0, size=150.0, envelope=0.5, pitch=0.0]),

    granular_attach_effect(G, reverb, [wet=0.4, decay=0.9], _),

    keyboard_init(K),
    keyboard_connect(K, 0, G),
    keyboard_connect(K, 1, G),
    keyboard_connect(K, 2, G),
    keyboard_connect(K, 3, G),

    /* row 0: major pentatonic, high octave */
    keyboard_row_set(K, 0, [mode=[0.0, 2.0, 4.0, 7.0, 9.0], octave=2]),

    /* row 1: blues scale */
    keyboard_row_set(K, 1, [mode=[0.0, 3.0, 5.0, 6.0, 7.0, 10.0], octave=1]),

    /* row 2: whole tone scale */
    keyboard_row_set(K, 2, [mode=[0.0, 2.0, 4.0, 6.0, 8.0, 10.0], octave=0]),

    /* row 3: quarter tone chromatic */
    keyboard_row_set(K, 3, [mode=[0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5], octave=(-1)]),

    format('Keyboard with custom scales:~n'),
    format('  Row 0 (1-0): Major pentatonic, +2 octaves~n'),
    format('  Row 1 (Q-P): Blues scale, +1 octave~n'),
    format('  Row 2 (A-;): Whole tone scale~n'),
    format('  Row 3 (Z-/): Quarter tone chromatic, -1 octave~n'),
    format('  ESC to exit~n').

demo_keyboard_multi :-
    promini_init,
    control_init,

    sound_load('audio/gong.wav', Gong),
    sound_load('audio/guitar.wav', Guitar),

    granular_init(4.0, G1),
    granular_connect(G1, Gong),
    granular_set(G1, [density=0.0, size=150.0, envelope=0.5, pitch=0.0]),
    granular_attach_effect(G1, reverb, [wet=0.3, decay=0.8], _),

    granular_init(4.0, G2),
    granular_connect(G2, Guitar),
    granular_set(G2, [density=0.0, size=100.0, envelope=0.3, pitch=0.0]),
    granular_attach_effect(G2, reverb, [wet=0.4, decay=0.6], _),

    /* first keyboard controls gong */
    keyboard_init(K1),
    keyboard_connect(K1, 0, G1),
    keyboard_connect(K1, 1, G1),
    keyboard_connect(K1, 2, G1),
    keyboard_connect(K1, 3, G1),
    keyboard_row_set(K1, 0, [mode=[0.0, 2.0, 4.0, 7.0, 9.0], octave=2]),
    keyboard_row_set(K1, 1, [mode=[0.0, 2.0, 4.0, 7.0, 9.0], octave=1]),
    keyboard_row_set(K1, 2, [mode=[0.0, 2.0, 4.0, 7.0, 9.0], octave=0]),
    keyboard_row_set(K1, 3, [mode=[0.0, 2.0, 4.0, 7.0, 9.0], octave=(-1)]),

    /* second keyboard controls guitar */
    keyboard_init(K2),
    keyboard_connect(K2, 0, G2),
    keyboard_connect(K2, 1, G2),
    keyboard_connect(K2, 2, G2),
    keyboard_connect(K2, 3, G2),
    keyboard_row_set(K2, 0, [mode=[0.0, 3.0, 5.0, 6.0, 7.0, 10.0], octave=1]),
    keyboard_row_set(K2, 1, [mode=[0.0, 3.0, 5.0, 6.0, 7.0, 10.0], octave=0]),
    keyboard_row_set(K2, 2, [mode=[0.0, 3.0, 5.0, 6.0, 7.0, 10.0], octave=(-1)]),
    keyboard_row_set(K2, 3, [mode=[0.0, 3.0, 5.0, 6.0, 7.0, 10.0], octave=(-2)]),

    format('Two keyboards:~n'),
    format('  Keyboard 1: Gong, pentatonic scale~n'),
    format('  Keyboard 2: Guitar, blues scale~n'),
    format('  ESC closes focused keyboard~n').

demo_keyboard_synth :-
    demo_cleanup,
    promini_init,
    control_init,

    /* create 4 voices with harmonics */
    create_synth_voice(V1, E1),
    create_synth_voice(V2, E2),
    create_synth_voice(V3, E3),
    create_synth_voice(V4, E4),

    /* track for cleanup */
    assertz(demo_voice(V1)),
    assertz(demo_voice(V2)),
    assertz(demo_voice(V3)),
    assertz(demo_voice(V4)),
    assertz(demo_envelope(E1)),
    assertz(demo_envelope(E2)),
    assertz(demo_envelope(E3)),
    assertz(demo_envelope(E4)),

    /* start voices (envelopes control sound) */
    synth_voice_start(V1),
    synth_voice_start(V2),
    synth_voice_start(V3),
    synth_voice_start(V4),

    /* create keyboard and add voices to row 0 */
    keyboard_init(K),
    keyboard_row_add_voice(K, 0, V1, [E1]),
    keyboard_row_add_voice(K, 0, V2, [E2]),
    keyboard_row_add_voice(K, 0, V3, [E3]),
    keyboard_row_add_voice(K, 0, V4, [E4]),

    /* pentatonic scale */
    keyboard_row_set(K, 0, [mode=[0.0, 2.0, 4.0, 7.0, 9.0], octave=0]),

    format('Polyphonic synth keyboard (4 voices):~n'),
    format('  Row 0 (1-0): Pentatonic scale~n'),
    format('  ESC to exit~n').

demo_keyboard_hybrid :-
    demo_cleanup,
    promini_init,
    control_init,

    /* load audio for granular rows */
    sound_load('audio/gong.wav', Gong),
    sound_load('audio/guitar.wav', Guitar),
    assertz(demo_sound(Gong)),
    assertz(demo_sound(Guitar)),

    /* granular for row 0 */
    granular_init(4.0, G1),
    granular_connect(G1, Gong),
    granular_set(G1, [density=0.0, size=150.0, envelope=0.5, pitch=0.0]),
    assertz(demo_granular(G1)),

    /* granular for row 1 */
    granular_init(4.0, G2),
    granular_connect(G2, Guitar),
    granular_set(G2, [density=0.0, size=100.0, envelope=0.3, pitch=0.0]),
    assertz(demo_granular(G2)),

    /* synth voices for row 2 */
    create_synth_voice(V1, E1),
    create_synth_voice(V2, E2),
    create_synth_voice(V3, E3),
    create_synth_voice(V4, E4),
    synth_voice_start(V1),
    synth_voice_start(V2),
    synth_voice_start(V3),
    synth_voice_start(V4),

    /* synth voices for row 3 */
    create_synth_voice(V5, E5),
    create_synth_voice(V6, E6),
    create_synth_voice(V7, E7),
    create_synth_voice(V8, E8),
    synth_voice_start(V5),
    synth_voice_start(V6),
    synth_voice_start(V7),
    synth_voice_start(V8),

    /* track voices and envelopes for cleanup */
    assertz(demo_voice(V1)), assertz(demo_voice(V2)),
    assertz(demo_voice(V3)), assertz(demo_voice(V4)),
    assertz(demo_voice(V5)), assertz(demo_voice(V6)),
    assertz(demo_voice(V7)), assertz(demo_voice(V8)),
    assertz(demo_envelope(E1)), assertz(demo_envelope(E2)),
    assertz(demo_envelope(E3)), assertz(demo_envelope(E4)),
    assertz(demo_envelope(E5)), assertz(demo_envelope(E6)),
    assertz(demo_envelope(E7)), assertz(demo_envelope(E8)),

    /* summing node with reverb - connect all sources */
    summing_node_init(Sum),
    summing_node_connect(Sum, G1),
    summing_node_connect(Sum, G2),
    summing_node_connect(Sum, V1),
    summing_node_connect(Sum, V2),
    summing_node_connect(Sum, V3),
    summing_node_connect(Sum, V4),
    summing_node_connect(Sum, V5),
    summing_node_connect(Sum, V6),
    summing_node_connect(Sum, V7),
    summing_node_connect(Sum, V8),
    summing_node_attach_effect(Sum, reverb, [wet=0.3, decay=0.8], _),
    assertz(demo_summing(Sum)),

    /* S&H noise on gong position - random jumps every ~1 sec */
    mod_noise_init(white, PosNoise),
    mod_source_set_sh(PosNoise, 1000.0),
    mod_route_init(PosNoise, granular, G1, position, absolute, 0.5, 0.5, 0.0, _),

    /* keyboard setup */
    keyboard_init(K),

    /* row 0: granular gong, pentatonic +2 octaves */
    keyboard_connect(K, 0, G1),
    keyboard_row_set(K, 0, [mode=[0.0, 2.0, 4.0, 7.0, 9.0], octave=2]),

    /* row 1: granular guitar, pentatonic +1 octave */
    keyboard_connect(K, 1, G2),
    keyboard_row_set(K, 1, [mode=[0.0, 2.0, 4.0, 7.0, 9.0], octave=1]),

    /* row 2: synth, pentatonic, octave 0 */
    keyboard_row_add_voice(K, 2, V1, [E1]),
    keyboard_row_add_voice(K, 2, V2, [E2]),
    keyboard_row_add_voice(K, 2, V3, [E3]),
    keyboard_row_add_voice(K, 2, V4, [E4]),
    keyboard_row_set(K, 2, [mode=[0.0, 2.0, 4.0, 7.0, 9.0], octave=0]),

    /* row 3: synth, pentatonic, octave -1 */
    keyboard_row_add_voice(K, 3, V5, [E5]),
    keyboard_row_add_voice(K, 3, V6, [E6]),
    keyboard_row_add_voice(K, 3, V7, [E7]),
    keyboard_row_add_voice(K, 3, V8, [E8]),
    keyboard_row_set(K, 3, [mode=[0.0, 2.0, 4.0, 7.0, 9.0], octave=(-1)]),

    format('Hybrid keyboard:~n'),
    format('  Row 0 (1-0): Granular gong, pentatonic +2 octaves~n'),
    format('  Row 1 (Q-P): Granular guitar, pentatonic +1 octave~n'),
    format('  Row 2 (A-;): Synth, pentatonic~n'),
    format('  Row 3 (Z-/): Synth, pentatonic -1 octave~n'),
    format('  ESC to exit~n').

/* helper: create a voice with 3 harmonics and an ADSR envelope */
create_synth_voice(Voice, Envelope) :-
    synth_voice_init(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, O1),
    synth_oscillator_add(Voice, 880.0, 0.0, O2),
    synth_oscillator_add(Voice, 1320.0, 0.0, O3),
    synth_oscillator_set_volume(O1, 0.4),
    synth_oscillator_set_volume(O2, 0.2),
    synth_oscillator_set_volume(O3, 0.1),
    /* attach VCA to control volume */
    voice_attach_effect(Voice, vca, [gain=0.0], Vca),
    /* ADSR: attack=0.1, decay=0.2, break=0.3, breaklevel=0.6, release=0.4, duration=500ms, no loop */
    mod_envelope_init(0.1, 0.2, 0.3, 0.6, 0.4, 500.0, false, Envelope),
    /* route envelope to VCA gain */
    mod_route_init(Envelope, vca, Vca, gain, absolute, 1.0, 0.0, 0.0, _).
