:- module(test_visualizer, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(visualizer).

test(attach_detach, [nondet]) :-
    promini_init,
    control_init,
    sound_load("audio/guitar.wav", S),
    visualizer_attach(S, true, V),
    visualizer_detach(V),
    sound_unload(S).

:- end_tests(visualizer).
