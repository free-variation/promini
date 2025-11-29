/*
 * mod_demo.pro - Modulation system demo
 * Demonstrates LFO modulating oscillator frequency (vibrato effect)
 */

:- use_module('../src/prolog/sampler.pro').

demo :-
    writeln('Creating voice with oscillator at 440 Hz...'),
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 440.0, 0.0, O),

    writeln('Creating sine LFO at 5 Hz...'),
    sampler_mod_lfo_create(sine, 5.0, L),

    writeln('Routing LFO to oscillator frequency (depth=50 Hz, offset=440 Hz)...'),
    sampler_mod_route_create(L, oscillator, O, frequency, 50.0, 440.0, 0.0, R),

    writeln('Playing for 3 seconds with vibrato...'),
    sampler_synth_voice_start(V),
    sleep(3),

    writeln('Changing LFO to 1 Hz for slower vibrato...'),
    sampler_mod_lfo_set_frequency(L, 1.0),
    sleep(3),

    writeln('Increasing depth to 100 Hz...'),
    sampler_mod_route_unload(R),
    sampler_mod_route_create(L, oscillator, O, frequency, 100.0, 440.0, 0.0, R2),
    sleep(3),

    writeln('Stopping...'),
    sampler_synth_voice_stop(V),

    writeln('Cleaning up...'),
    sampler_mod_route_unload(R2),
    sampler_mod_source_unload(L),
    sampler_synth_voice_unload(V),

    writeln('Done.').
