:- module(keyboard_pad, [setup/0, teardown/0]).
:- use_module('../src/prolog/promini.pro').

/*
 * Keyboard Pad Setup
 * All 4 rows: Synth voices (4 each, 16 total)
 * Eb minor pentatonic [1, 4, 6, 8, 11]
 * Row 0 (+1 oct) -> Row 3 (-2 oct, bass)
 *
 * Usage:
 *   load_setup(keyboard_pad).
 *   unload_setup(keyboard_pad).
 */

setup :-
    control_init,

    /* synth voices for row 0 (highest) */
    create_synth_voice(V1, E1),
    setup_register(keyboard_pad:voice1, V1),
    setup_register(keyboard_pad:env1, E1),
    create_synth_voice(V2, E2),
    setup_register(keyboard_pad:voice2, V2),
    setup_register(keyboard_pad:env2, E2),
    create_synth_voice(V3, E3),
    setup_register(keyboard_pad:voice3, V3),
    setup_register(keyboard_pad:env3, E3),
    create_synth_voice(V4, E4),
    setup_register(keyboard_pad:voice4, V4),
    setup_register(keyboard_pad:env4, E4),
    synth_voice_start(V1),
    synth_voice_start(V2),
    synth_voice_start(V3),
    synth_voice_start(V4),

    /* synth voices for row 1 */
    create_synth_voice(V5, E5),
    setup_register(keyboard_pad:voice5, V5),
    setup_register(keyboard_pad:env5, E5),
    create_synth_voice(V6, E6),
    setup_register(keyboard_pad:voice6, V6),
    setup_register(keyboard_pad:env6, E6),
    create_synth_voice(V7, E7),
    setup_register(keyboard_pad:voice7, V7),
    setup_register(keyboard_pad:env7, E7),
    create_synth_voice(V8, E8),
    setup_register(keyboard_pad:voice8, V8),
    setup_register(keyboard_pad:env8, E8),
    synth_voice_start(V5),
    synth_voice_start(V6),
    synth_voice_start(V7),
    synth_voice_start(V8),

    /* synth voices for row 2 */
    create_synth_voice(V9, E9),
    setup_register(keyboard_pad:voice9, V9),
    setup_register(keyboard_pad:env9, E9),
    create_synth_voice(V10, E10),
    setup_register(keyboard_pad:voice10, V10),
    setup_register(keyboard_pad:env10, E10),
    create_synth_voice(V11, E11),
    setup_register(keyboard_pad:voice11, V11),
    setup_register(keyboard_pad:env11, E11),
    create_synth_voice(V12, E12),
    setup_register(keyboard_pad:voice12, V12),
    setup_register(keyboard_pad:env12, E12),
    synth_voice_start(V9),
    synth_voice_start(V10),
    synth_voice_start(V11),
    synth_voice_start(V12),

    /* synth voices for row 3 (bass) */
    create_synth_voice(V13, E13),
    setup_register(keyboard_pad:voice13, V13),
    setup_register(keyboard_pad:env13, E13),
    create_synth_voice(V14, E14),
    setup_register(keyboard_pad:voice14, V14),
    setup_register(keyboard_pad:env14, E14),
    create_synth_voice(V15, E15),
    setup_register(keyboard_pad:voice15, V15),
    setup_register(keyboard_pad:env15, E15),
    create_synth_voice(V16, E16),
    setup_register(keyboard_pad:voice16, V16),
    setup_register(keyboard_pad:env16, E16),
    synth_voice_start(V13),
    synth_voice_start(V14),
    synth_voice_start(V15),
    synth_voice_start(V16),

    /* summing node with VCA (master volume) and reverb */
    summing_node_init(Sum),
    setup_register(keyboard_pad:summing, Sum),
    summing_node_connect(Sum, V1),
    summing_node_connect(Sum, V2),
    summing_node_connect(Sum, V3),
    summing_node_connect(Sum, V4),
    summing_node_connect(Sum, V5),
    summing_node_connect(Sum, V6),
    summing_node_connect(Sum, V7),
    summing_node_connect(Sum, V8),
    summing_node_connect(Sum, V9),
    summing_node_connect(Sum, V10),
    summing_node_connect(Sum, V11),
    summing_node_connect(Sum, V12),
    summing_node_connect(Sum, V13),
    summing_node_connect(Sum, V14),
    summing_node_connect(Sum, V15),
    summing_node_connect(Sum, V16),
    summing_node_attach_effect(Sum, vca, [gain=0.8], MasterVca),
    setup_register(keyboard_pad:master_vca, MasterVca),
    summing_node_attach_effect(Sum, ping_pong_delay, [max_delay_in_frames=22050, delay_in_frames=22050, feedback=0.4, wet=0.3, dry=0.7], PingPong),
    summing_node_attach_effect(Sum, reverb, [wet=0.1, decay=0.5, shimmer1_shift=7.0, shimmer1_mix=0.2], Rev),
    setup_register(keyboard_pad:ping_pong, PingPong),

    /* up/down arrows control master volume */
    mod_keyboard_init(up, [attack=50, release=100], ModUp),
    setup_register(keyboard_pad:mod_up, ModUp),
    mod_keyboard_init(down, [attack=50, release=100], ModDown),
    setup_register(keyboard_pad:mod_down, ModDown),
    mod_route_init(ModUp, vca, MasterVca, gain, rate, 0.5, 0.0, 0.0, VolUpRoute),
    setup_register(keyboard_pad:vol_up_route, VolUpRoute),
    mod_route_init(ModDown, vca, MasterVca, gain, rate, -0.5, 0.0, 0.0, VolDownRoute),
    setup_register(keyboard_pad:vol_down_route, VolDownRoute),

    /* keyboard setup */
    keyboard_init(K),
    setup_register(keyboard_pad:keyboard, K),
    mod_route_monitor(VolUpRoute, true, K),
    mod_route_monitor(VolDownRoute, true, K),

    /* Eb minor pentatonic: [1, 4, 6, 8, 11] */

    /* row 0: +1 octave (highest) */
    keyboard_row_add_voice(K, 0, V1, [E1]),
    keyboard_row_add_voice(K, 0, V2, [E2]),
    keyboard_row_add_voice(K, 0, V3, [E3]),
    keyboard_row_add_voice(K, 0, V4, [E4]),
    keyboard_row_set(K, 0, [mode=[1.0, 4.0, 6.0, 8.0, 11.0], octave=1]),

    /* row 1: octave 0 */
    keyboard_row_add_voice(K, 1, V5, [E5]),
    keyboard_row_add_voice(K, 1, V6, [E6]),
    keyboard_row_add_voice(K, 1, V7, [E7]),
    keyboard_row_add_voice(K, 1, V8, [E8]),
    keyboard_row_set(K, 1, [mode=[1.0, 4.0, 6.0, 8.0, 11.0], octave=0]),

    /* row 2: -1 octave */
    keyboard_row_add_voice(K, 2, V9, [E9]),
    keyboard_row_add_voice(K, 2, V10, [E10]),
    keyboard_row_add_voice(K, 2, V11, [E11]),
    keyboard_row_add_voice(K, 2, V12, [E12]),
    keyboard_row_set(K, 2, [mode=[1.0, 4.0, 6.0, 8.0, 11.0], octave=(-1)]),

    /* row 3: -2 octaves (bass) */
    keyboard_row_add_voice(K, 3, V13, [E13]),
    keyboard_row_add_voice(K, 3, V14, [E14]),
    keyboard_row_add_voice(K, 3, V15, [E15]),
    keyboard_row_add_voice(K, 3, V16, [E16]),
    keyboard_row_set(K, 3, [mode=[1.0, 4.0, 6.0, 8.0, 11.0], octave=(-2)]),

    format('~nKeyboard Pad Setup Loaded~n'),
    format('  Row 0 (1-0): Synth, Eb minor pentatonic +1 octave~n'),
    format('  Row 1 (Q-P): Synth, Eb minor pentatonic~n'),
    format('  Row 2 (A-;): Synth, Eb minor pentatonic -1 octave~n'),
    format('  Row 3 (Z-/): Synth, Eb minor pentatonic -2 octaves (bass)~n'),
    format('  UP/DOWN arrows: master volume~n'),
    format('  ESC to exit~n').

teardown :-
    setup_unload(keyboard_pad).

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
