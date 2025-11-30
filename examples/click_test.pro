:- use_module('src/prolog/promini.pro').

% Test 1: Stop without fade - should click
test_no_fade :-
    format('Creating voice with oscillator...~n'),
    synth_voice_create(V),
    synth_oscillator_add(V, 220.0, 0.0, _),

    format('Starting...~n'),
    synth_voice_start(V),
    sleep(1),

    format('Stopping WITHOUT fade...~n'),
    synth_voice_stop(V),

    sleep(0.5),
    synth_voice_unload(V),
    format('Done.~n').

% Test 2: Stop with fade - should not click
test_with_fade :-
    format('Creating voice with oscillator...~n'),
    synth_voice_create(V),
    synth_oscillator_add(V, 220.0, 0.0, _),

    format('Starting...~n'),
    synth_voice_start(V),
    sleep(1),

    format('Fading then stopping...~n'),
    synth_voice_fade(V, 0.0, 15),
    sleep(0.02),
    synth_voice_stop(V),

    sleep(0.5),
    synth_voice_unload(V),
    format('Done.~n').

% Test 3: Just start - isolate if click is on start
test_start_only :-
    format('Creating voice with oscillator...~n'),
    synth_voice_create(V),
    synth_oscillator_add(V, 220.0, 0.0, _),

    format('Starting...~n'),
    synth_voice_start(V),
    sleep(2),

    format('NOT stopping - just unloading...~n'),
    synth_voice_unload(V),
    format('Done.~n').
