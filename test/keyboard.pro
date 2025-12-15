:- module(test_keyboard, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(keyboard).

test(keyboard_initially_inactive, [nondet, setup(control_init)]) :-
    keyboard_active(false).

test(keyboard_start_activates, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_active(true).

test(keyboard_stop_deactivates, [nondet, setup(control_init)]) :-
    keyboard_start,
    keyboard_stop,
    keyboard_active(false).

test(mod_keyboard_init_creates_source, [nondet, setup(control_init), cleanup(mod_source_uninit(M))]) :-
    mod_keyboard_init(space, [], M),
    M = mod_source(_).

test(mod_keyboard_init_with_options, [nondet, setup(control_init), cleanup(mod_source_uninit(M))]) :-
    mod_keyboard_init(up, [attack=200, release=500, invert=true], M),
    M = mod_source(_).

test(mod_keyboard_init_different_keys, [nondet, setup(control_init), cleanup((mod_source_uninit(M1), mod_source_uninit(M2), mod_source_uninit(M3)))]) :-
    mod_keyboard_init(space, [], M1),
    mod_keyboard_init(left_control, [], M2),
    mod_keyboard_init(down, [], M3),
    M1 = mod_source(S1),
    M2 = mod_source(S2),
    M3 = mod_source(S3),
    S1 \= S2,
    S2 \= S3.

% keyboard_row_set tests

test(row_set_mode_major_triad, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(0, [mode=[0.0, 4.0, 7.0]]).

test(row_set_mode_minor_triad, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(1, [mode=[0.0, 3.0, 7.0]]).

test(row_set_octave_only, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(2, [octave=3]).

test(row_set_mode_and_octave, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(0, [mode=[0.0, 2.0, 4.0, 5.0, 7.0, 9.0, 11.0], octave=1]).

test(row_set_quarter_tones, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(0, [mode=[0.0, 0.5, 1.0, 1.5, 2.0]]).

test(row_set_microtonal_scale, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(0, [mode=[0.0, 2.4, 3.86, 4.98, 7.02, 8.84, 10.88]]).

test(row_set_single_note, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(0, [mode=[0.0]]).

test(row_set_chromatic, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(0, [mode=[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0]]).

test(row_set_negative_octave, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(3, [octave=(-2)]).

test(row_set_all_rows, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(0, [mode=[0.0, 4.0, 7.0], octave=2]),
    keyboard_row_set(1, [mode=[0.0, 3.0, 7.0], octave=1]),
    keyboard_row_set(2, [mode=[0.0, 2.0, 4.0], octave=0]),
    keyboard_row_set(3, [mode=[0.0, 5.0, 7.0], octave=(-1)]).

test(row_set_pentatonic, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(0, [mode=[0.0, 2.0, 4.0, 7.0, 9.0]]).

test(row_set_whole_tone, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(0, [mode=[0.0, 2.0, 4.0, 6.0, 8.0, 10.0]]).

test(row_set_empty_options, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(0, []).

test(row_set_invalid_row_fails, [error(domain_error(keyboard_row, 4)), setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(4, [mode=[0.0, 2.0, 4.0]]).

test(row_set_negative_row_fails, [error(domain_error(keyboard_row, -1)), setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(-1, [mode=[0.0, 2.0, 4.0]]).

test(row_set_before_start_succeeds, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_row_set(0, [mode=[0.0, 4.0, 7.0]]),
    keyboard_start.

test(row_set_with_connect, [nondet, setup(control_init), cleanup((keyboard_stop, granular_uninit(G)))]) :-
    keyboard_start,
    granular_init(2.0, G),
    keyboard_row_set(0, [mode=[0.0, 4.0, 7.0], octave=1]),
    keyboard_connect(0, G).

test(row_set_multiple_times, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(0, [mode=[0.0, 2.0, 4.0]]),
    keyboard_row_set(0, [mode=[0.0, 3.0, 7.0]]),
    keyboard_row_set(0, [octave=2]).

test(row_set_blues_scale, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(0, [mode=[0.0, 3.0, 5.0, 6.0, 7.0, 10.0]]).

test(row_set_harmonic_minor, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    keyboard_row_set(0, [mode=[0.0, 2.0, 3.0, 5.0, 7.0, 8.0, 11.0]]).

test(row_set_just_intonation_major, [nondet, setup(control_init), cleanup(keyboard_stop)]) :-
    keyboard_start,
    /* just intonation ratios converted to cents/100 */
    keyboard_row_set(0, [mode=[0.0, 2.04, 3.86, 4.98, 7.02, 8.84, 10.88]]).

:- end_tests(keyboard).
