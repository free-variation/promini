:- use_module('src/prolog/sampler.pro').

% Test 1: Stop without fade - should click
test_no_fade :-
    format('Creating voice with oscillator...~n'),
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 220.0, 0.0, _),

    format('Starting...~n'),
    sampler_synth_voice_start(V),
    sleep(1),

    format('Stopping WITHOUT fade...~n'),
    sampler_synth_voice_stop(V),

    sleep(0.5),
    sampler_synth_voice_unload(V),
    format('Done.~n').

% Test 2: Stop with fade - should not click
test_with_fade :-
    format('Creating voice with oscillator...~n'),
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 220.0, 0.0, _),

    format('Starting...~n'),
    sampler_synth_voice_start(V),
    sleep(1),

    format('Fading then stopping...~n'),
    sampler_synth_voice_fade(V, 0.0, 15),
    sleep(0.02),
    sampler_synth_voice_stop(V),

    sleep(0.5),
    sampler_synth_voice_unload(V),
    format('Done.~n').

% Test 3: Just start - isolate if click is on start
test_start_only :-
    format('Creating voice with oscillator...~n'),
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 220.0, 0.0, _),

    format('Starting...~n'),
    sampler_synth_voice_start(V),
    sleep(2),

    format('NOT stopping - just unloading...~n'),
    sampler_synth_voice_unload(V),
    format('Done.~n').
