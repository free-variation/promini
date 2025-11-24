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
