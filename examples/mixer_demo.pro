:- use_module('src/prolog/promini.pro').

/*
 * VCA demo - basic gain control with per-sample interpolation
 */
demo_vca :-
    format('~n=== VCA Demo ===~n~n'),

    sound_load('audio/counting.wav', Sound),

    format('Playing original sound...~n'),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Attaching VCA at gain 0.5 (half volume)...~n'),
    sound_attach_effect(Sound, vca, [gain=0.5], VCA),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Setting VCA gain to 0.25 (quarter volume)...~n'),
    effect_set_parameters(VCA, [gain=0.25]),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Setting VCA gain to 1.0 (full volume)...~n'),
    effect_set_parameters(VCA, [gain=1.0]),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    sound_unload(Sound),
    format('VCA demo complete.~n~n').

/*
 * VCA fade demo - smooth gain changes via rapid parameter updates
 */
demo_vca_fade :-
    format('~n=== VCA Fade Demo ===~n~n'),

    sound_load('audio/guitar.wav', Sound),
    sound_loop(Sound),
    sound_attach_effect(Sound, vca, [gain=1.0], VCA),

    format('Playing with fade out over 3 seconds...~n'),
    sound_start(Sound),
    fade_vca(VCA, 1.0, 0.0, 30),

    format('Fade in over 3 seconds...~n'),
    fade_vca(VCA, 0.0, 1.0, 30),

    sleep(1),
    sound_stop(Sound),
    sound_unload(Sound),
    format('VCA fade demo complete.~n~n').

fade_vca(VCA, Start, End, Steps) :-
    Step is (End - Start) / Steps,
    fade_vca_(VCA, Start, End, Step).

fade_vca_(VCA, Current, End, Step) :-
    effect_set_parameters(VCA, [gain=Current]),
    (   (Step > 0, Current >= End)
    ;   (Step < 0, Current =< End)
    ), !.
fade_vca_(VCA, Current, End, Step) :-
    effect_set_parameters(VCA, [gain=Current]),
    sleep(0.1),
    Next is Current + Step,
    fade_vca_(VCA, Next, End, Step).

/*
 * LFO -> VCA demo - tremolo effect
 */
demo_lfo_vca :-
    format('~n=== LFO -> VCA Demo (Tremolo) ===~n~n'),

    sound_load('audio/guitar.wav', Sound),
    sound_loop(Sound),

    format('Attaching VCA...~n'),
    sound_attach_effect(Sound, vca, [gain=1.0], effect(_Source, VcaPtr)),

    format('Creating LFO at 4 Hz to modulate gain (0.2-1.0)...~n'),
    mod_lfo_create(sine, 4.0, Lfo),
    mod_route_create(Lfo, vca, VcaPtr, gain, 0.4, 0.6, 0.0, Route),

    format('Playing guitar with tremolo for 8 seconds...~n'),
    sound_start(Sound),
    sleep(8.0),

    sound_stop(Sound),
    mod_route_unload(Route),
    mod_source_unload(Lfo),
    sound_unload(Sound),
    format('Demo complete!~n~n').

/*
 * Summing node demo - mix two sounds through a summing node
 */
demo_summing_node :-
    format('~n=== Summing Node Demo ===~n~n'),

    summing_node_create(Bus),

    sound_load('audio/guitar.wav', Guitar),
    sound_load('audio/counting.wav', Counting),
    sound_loop(Guitar),

    format('Connecting sounds to summing node...~n'),
    summing_node_connect(Bus, sound(Guitar)),
    summing_node_connect(Bus, sound(Counting)),

    format('Playing mixed audio for 6 seconds...~n'),
    sound_start(Guitar),
    sound_start(Counting),
    sleep(6),

    sound_stop(Guitar),
    sound_stop(Counting),
    sound_unload(Guitar),
    sound_unload(Counting),
    summing_node_unload(Bus),
    format('Demo complete!~n~n').
