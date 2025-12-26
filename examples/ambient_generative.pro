/*
 * ambient_generative.pro
 * Ambient generative music using the granular_reverb setup
 *
 * Loads granular_reverb and adds slow autonomous modulation.
 * Gamepad and keyboard remain active for live performance.
 *
 * Usage:
 *   ['examples/ambient_generative.pro'].
 *   ambient_start('MacBook Pro Microphone').
 *   ambient_stop.
 */

:- use_module('../src/prolog/promini.pro').

:- dynamic ambient_mod/1.

/*
 * ambient_start/1
 * Load granular_reverb setup and add generative modulation.
 */
ambient_start(Source) :-
    format('~n=== Ambient Generative ===~n~n'),

    /* load the granular_reverb setup */
    load_setup(granular_reverb, Source),

    /* get handles for modulation targets */
    setup_get(granular_reverb:granular, G),
    setup_get(granular_reverb:moog, Moog),
    setup_get(granular_reverb:reverb, Rev),

    /* set ambient-friendly initial parameters */
    granular_set(G, [
        density=15.0,
        size=250.0,
        position_spray=0.4,
        pan_spray=0.8,
        regularity=0.3
    ]),
    effect_set_parameters(Rev, [
        decay=0.92,
        shimmer1_mix=0.2
    ]),
    effect_set_parameters(Moog, [cutoff=3000.0]),

    format('Adding generative modulation...~n'),

    /* === SLOW LFOs === */

    /* position drift: 45 second cycle */
    mod_lfo_init(sine, 0.022, PosLFO),
    assertz(ambient_mod(PosLFO)),
    mod_route_init(PosLFO, granular, G, position, absolute, 0.4, 0.5, 0.0, _),

    /* density breathing: 30 second cycle */
    mod_lfo_init(triangle, 0.033, DensLFO),
    assertz(ambient_mod(DensLFO)),
    mod_route_init(DensLFO, granular, G, density, absolute, 6.0, 15.0, 0.0, _),

    /* grain size: 60 second cycle */
    mod_lfo_init(sine, 0.017, SizeLFO),
    assertz(ambient_mod(SizeLFO)),
    mod_route_init(SizeLFO, granular, G, size, absolute, 100.0, 200.0, 0.0, _),

    /* filter sweep: 90 second cycle */
    mod_lfo_init(sine, 0.011, FilterLFO),
    assertz(ambient_mod(FilterLFO)),
    mod_route_init(FilterLFO, moog, Moog, cutoff, absolute, 2000.0, 2500.0, 0.0, _),

    /* === S&H NOISE === */

    /* position jumps: every 8 seconds */
    mod_noise_init(white, PosNoise),
    assertz(ambient_mod(PosNoise)),
    mod_source_set_sh(PosNoise, 8000.0),
    mod_route_init(PosNoise, granular, G, position, absolute, 0.15, 0.0, 0.0, _),

/*     /* pitch shifts: every 12 seconds */
    mod_noise_init(white, PitchNoise),
    assertz(ambient_mod(PitchNoise)),
    mod_source_set_sh(PitchNoise, 12000.0),
    mod_route_init(PitchNoise, granular, G, pitch, absolute, 7.0, 0.0, 0.0, _), */

    format('~nAmbient generation active.~n'),
    format('Use gamepad/keyboard for live control.~n'),
    format('Call ambient_stop to end.~n').

/*
 * ambient_stop/0
 * Clean up modulation and unload setup.
 */
ambient_stop :-
    format('Stopping...~n'),
    forall(ambient_mod(M), mod_source_uninit(M)),
    retractall(ambient_mod(_)),
    unload_setup(granular_reverb),
    format('Stopped.~n').
