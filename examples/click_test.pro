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

    format('Attaching VCA for fade control...~n'),
    voice_attach_effect(V, vca, [gain=1.0], VcaEffect),

    format('Starting...~n'),
    synth_voice_start(V),
    sleep(1),

    format('Fading out via VCA then stopping...~n'),
    fade_vca(VcaEffect, 1.0, 0.0, 15),
    synth_voice_stop(V),

    sleep(0.5),
    synth_voice_unload(V),
    format('Done.~n').

% Helper to fade VCA gain over steps
fade_vca(_, _, _, 0) :- !.
fade_vca(VcaEffect, Current, Target, Steps) :-
    Steps > 0,
    Step is (Target - Current) / Steps,
    Next is Current + Step,
    effect_set_parameters(VcaEffect, [gain=Next]),
    sleep(0.02),
    RemainingSteps is Steps - 1,
    fade_vca(VcaEffect, Next, Target, RemainingSteps).

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
