:- use_module('src/prolog/promini.pro').

demo_keyboard :-
    control_init,
    keyboard_init(_K),
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

    /* row 0: major pentatonic, high octave */
    keyboard_row_set(K, 0, [mode=[0.0, 2.0, 4.0, 7.0, 9.0], octave=2]),

    /* row 1: blues scale */
    keyboard_row_set(K, 1, [mode=[0.0, 3.0, 5.0, 6.0, 7.0, 10.0], octave=1]),

    /* row 2: whole tone scale */
    keyboard_row_set(K, 2, [mode=[0.0, 2.0, 4.0, 6.0, 8.0, 10.0], octave=0]),

    /* row 3: quarter tone chromatic */
    keyboard_row_set(K, 3, [mode=[0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5], octave=(-1)]),

    keyboard_connect(K, 0, G),
    keyboard_connect(K, 1, G),
    keyboard_connect(K, 2, G),
    keyboard_connect(K, 3, G),

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
    keyboard_row_set(K1, 0, [mode=[0.0, 2.0, 4.0, 7.0, 9.0], octave=2]),
    keyboard_row_set(K1, 1, [mode=[0.0, 2.0, 4.0, 7.0, 9.0], octave=1]),
    keyboard_row_set(K1, 2, [mode=[0.0, 2.0, 4.0, 7.0, 9.0], octave=0]),
    keyboard_row_set(K1, 3, [mode=[0.0, 2.0, 4.0, 7.0, 9.0], octave=(-1)]),
    keyboard_connect(K1, 0, G1),
    keyboard_connect(K1, 1, G1),
    keyboard_connect(K1, 2, G1),
    keyboard_connect(K1, 3, G1),

    /* second keyboard controls guitar */
    keyboard_init(K2),
    keyboard_row_set(K2, 0, [mode=[0.0, 3.0, 5.0, 6.0, 7.0, 10.0], octave=1]),
    keyboard_row_set(K2, 1, [mode=[0.0, 3.0, 5.0, 6.0, 7.0, 10.0], octave=0]),
    keyboard_row_set(K2, 2, [mode=[0.0, 3.0, 5.0, 6.0, 7.0, 10.0], octave=(-1)]),
    keyboard_row_set(K2, 3, [mode=[0.0, 3.0, 5.0, 6.0, 7.0, 10.0], octave=(-2)]),
    keyboard_connect(K2, 0, G2),
    keyboard_connect(K2, 1, G2),
    keyboard_connect(K2, 2, G2),
    keyboard_connect(K2, 3, G2),

    format('Two keyboards:~n'),
    format('  Keyboard 1: Gong, pentatonic scale~n'),
    format('  Keyboard 2: Guitar, blues scale~n'),
    format('  ESC closes focused keyboard~n').
