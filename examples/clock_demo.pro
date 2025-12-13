/*
 * clock_demo.pro - Clock system demo
 * Demonstrates the global clock running and beat position advancing
 */

:- use_module('src/prolog/promini.pro').

demo :-
    writeln('Initializing promini...'),
    promini_init,

    writeln('Setting clock to 120 BPM...'),
    clock_set_bpm(120),

    writeln('Starting clock...'),
    clock_start,

    writeln('Watching beat position for 5 seconds:'),
    show_beats(10),

    writeln('Changing to 60 BPM...'),
    clock_set_bpm(60),
    show_beats(5),

    writeln('Changing to 180 BPM...'),
    clock_set_bpm(180),
    show_beats(5),

    writeln('Stopping clock...'),
    clock_stop,

    writeln('Done.').

show_beats(0) :- !.
show_beats(N) :-
    N > 0,
    clock_get_beat_position(Pos),
    format('  Beat position: ~2f~n', [Pos]),
    sleep(0.5),
    N1 is N - 1,
    show_beats(N1).
