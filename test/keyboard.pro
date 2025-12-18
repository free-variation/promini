:- module(test_keyboard, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(keyboard).

test(keyboard_init_returns_handle, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    K = keyboard(_).

test(keyboard_multiple_init, [nondet, setup(control_init), cleanup((keyboard_uninit(K1), keyboard_uninit(K2)))]) :-
    keyboard_init(K1),
    keyboard_init(K2),
    K1 = keyboard(S1),
    K2 = keyboard(S2),
    S1 \= S2.

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

test(row_set_mode_major_triad, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, 0, [mode=[0.0, 4.0, 7.0]]).

test(row_set_mode_minor_triad, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, 1, [mode=[0.0, 3.0, 7.0]]).

test(row_set_octave_only, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, 2, [octave=3]).

test(row_set_mode_and_octave, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, 0, [mode=[0.0, 2.0, 4.0, 5.0, 7.0, 9.0, 11.0], octave=1]).

test(row_set_quarter_tones, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, 0, [mode=[0.0, 0.5, 1.0, 1.5, 2.0]]).

test(row_set_microtonal_scale, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, 0, [mode=[0.0, 2.4, 3.86, 4.98, 7.02, 8.84, 10.88]]).

test(row_set_single_note, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, 0, [mode=[0.0]]).

test(row_set_chromatic, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, 0, [mode=[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0]]).

test(row_set_negative_octave, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, 3, [octave=(-2)]).

test(row_set_all_rows, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, 0, [mode=[0.0, 4.0, 7.0], octave=2]),
    keyboard_row_set(K, 1, [mode=[0.0, 3.0, 7.0], octave=1]),
    keyboard_row_set(K, 2, [mode=[0.0, 2.0, 4.0], octave=0]),
    keyboard_row_set(K, 3, [mode=[0.0, 5.0, 7.0], octave=(-1)]).

test(row_set_pentatonic, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, 0, [mode=[0.0, 2.0, 4.0, 7.0, 9.0]]).

test(row_set_whole_tone, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, 0, [mode=[0.0, 2.0, 4.0, 6.0, 8.0, 10.0]]).

test(row_set_empty_options, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, 0, []).

test(row_set_invalid_row_fails, [error(domain_error(keyboard_row, 4)), setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, 4, [mode=[0.0, 2.0, 4.0]]).

test(row_set_negative_row_fails, [error(domain_error(keyboard_row, -1)), setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, -1, [mode=[0.0, 2.0, 4.0]]).

test(row_set_with_connect, [nondet, setup((promini_init, control_init)), cleanup((keyboard_uninit(K), granular_uninit(G)))]) :-
    granular_init(2.0, G),
    keyboard_init(K),
    keyboard_connect(K, 0, G),
    keyboard_row_set(K, 0, [mode=[0.0, 4.0, 7.0], octave=1]).

test(row_set_multiple_times, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, 0, [mode=[0.0, 2.0, 4.0]]),
    keyboard_row_set(K, 0, [mode=[0.0, 3.0, 7.0]]),
    keyboard_row_set(K, 0, [octave=2]).

test(row_set_blues_scale, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, 0, [mode=[0.0, 3.0, 5.0, 6.0, 7.0, 10.0]]).

test(row_set_harmonic_minor, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_set(K, 0, [mode=[0.0, 2.0, 3.0, 5.0, 7.0, 8.0, 11.0]]).

test(row_set_just_intonation_major, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    /* just intonation ratios converted to cents/100 */
    keyboard_row_set(K, 0, [mode=[0.0, 2.04, 3.86, 4.98, 7.02, 8.84, 10.88]]).

% keyboard_row_add_voice tests

test(row_add_voice_single_envelope, [nondet, setup((promini_init, control_init)),
        cleanup((keyboard_uninit(K), synth_voice_uninit(V), mod_source_uninit(E)))]) :-
    keyboard_init(K),
    synth_voice_init(V),
    mod_envelope_init(0.2, 0.2, 0.2, 0.7, 0.4, 500.0, false, E),
    keyboard_row_add_voice(K, 0, V, [E]).

test(row_add_voice_multiple_envelopes, [nondet, setup((promini_init, control_init)),
        cleanup((keyboard_uninit(K), synth_voice_uninit(V),
                 mod_source_uninit(E1), mod_source_uninit(E2)))]) :-
    keyboard_init(K),
    synth_voice_init(V),
    mod_envelope_init(0.2, 0.2, 0.2, 0.7, 0.4, 500.0, false, E1),
    mod_envelope_init(0.3, 0.1, 0.3, 0.5, 0.3, 800.0, false, E2),
    keyboard_row_add_voice(K, 0, V, [E1, E2]).

test(row_add_voice_multiple_voices, [nondet, setup((promini_init, control_init)),
        cleanup((keyboard_uninit(K), synth_voice_uninit(V1), synth_voice_uninit(V2),
                 mod_source_uninit(E1), mod_source_uninit(E2)))]) :-
    keyboard_init(K),
    synth_voice_init(V1),
    synth_voice_init(V2),
    mod_envelope_init(0.2, 0.2, 0.2, 0.7, 0.4, 500.0, false, E1),
    mod_envelope_init(0.2, 0.2, 0.2, 0.7, 0.4, 500.0, false, E2),
    keyboard_row_add_voice(K, 0, V1, [E1]),
    keyboard_row_add_voice(K, 0, V2, [E2]).

test(row_add_voice_different_rows, [nondet, setup((promini_init, control_init)),
        cleanup((keyboard_uninit(K), synth_voice_uninit(V1), synth_voice_uninit(V2),
                 mod_source_uninit(E1), mod_source_uninit(E2)))]) :-
    keyboard_init(K),
    synth_voice_init(V1),
    synth_voice_init(V2),
    mod_envelope_init(0.2, 0.2, 0.2, 0.7, 0.4, 500.0, false, E1),
    mod_envelope_init(0.2, 0.2, 0.2, 0.7, 0.4, 500.0, false, E2),
    keyboard_row_add_voice(K, 0, V1, [E1]),
    keyboard_row_add_voice(K, 2, V2, [E2]).

test(row_add_voice_invalid_row, [error(domain_error(keyboard_row, 4)),
        setup((promini_init, control_init)),
        cleanup((keyboard_uninit(K), synth_voice_uninit(V), mod_source_uninit(E)))]) :-
    keyboard_init(K),
    synth_voice_init(V),
    mod_envelope_init(0.2, 0.2, 0.2, 0.7, 0.4, 500.0, false, E),
    keyboard_row_add_voice(K, 4, V, [E]).

% keyboard_row_remove_voice tests

test(row_remove_voice, [nondet, setup((promini_init, control_init)),
        cleanup((keyboard_uninit(K), synth_voice_uninit(V), mod_source_uninit(E)))]) :-
    keyboard_init(K),
    synth_voice_init(V),
    mod_envelope_init(0.2, 0.2, 0.2, 0.7, 0.4, 500.0, false, E),
    keyboard_row_add_voice(K, 0, V, [E]),
    keyboard_row_remove_voice(K, 0, V).

test(row_remove_voice_middle, [nondet, setup((promini_init, control_init)),
        cleanup((keyboard_uninit(K), synth_voice_uninit(V1), synth_voice_uninit(V2),
                 synth_voice_uninit(V3), mod_source_uninit(E1), mod_source_uninit(E2),
                 mod_source_uninit(E3)))]) :-
    keyboard_init(K),
    synth_voice_init(V1),
    synth_voice_init(V2),
    synth_voice_init(V3),
    mod_envelope_init(0.2, 0.2, 0.2, 0.7, 0.4, 500.0, false, E1),
    mod_envelope_init(0.2, 0.2, 0.2, 0.7, 0.4, 500.0, false, E2),
    mod_envelope_init(0.2, 0.2, 0.2, 0.7, 0.4, 500.0, false, E3),
    keyboard_row_add_voice(K, 0, V1, [E1]),
    keyboard_row_add_voice(K, 0, V2, [E2]),
    keyboard_row_add_voice(K, 0, V3, [E3]),
    keyboard_row_remove_voice(K, 0, V2).

test(row_remove_voice_invalid_row, [error(domain_error(keyboard_row, 5)),
        setup((promini_init, control_init)),
        cleanup((keyboard_uninit(K), synth_voice_uninit(V), mod_source_uninit(E)))]) :-
    keyboard_init(K),
    synth_voice_init(V),
    mod_envelope_init(0.2, 0.2, 0.2, 0.7, 0.4, 500.0, false, E),
    keyboard_row_add_voice(K, 0, V, [E]),
    keyboard_row_remove_voice(K, 5, V).

% keyboard_row_clear tests

test(row_clear_with_voices, [nondet, setup((promini_init, control_init)),
        cleanup((keyboard_uninit(K), synth_voice_uninit(V1), synth_voice_uninit(V2),
                 mod_source_uninit(E1), mod_source_uninit(E2)))]) :-
    keyboard_init(K),
    synth_voice_init(V1),
    synth_voice_init(V2),
    mod_envelope_init(0.2, 0.2, 0.2, 0.7, 0.4, 500.0, false, E1),
    mod_envelope_init(0.2, 0.2, 0.2, 0.7, 0.4, 500.0, false, E2),
    keyboard_row_add_voice(K, 0, V1, [E1]),
    keyboard_row_add_voice(K, 0, V2, [E2]),
    keyboard_row_clear(K, 0).

test(row_clear_empty_row, [nondet, setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_clear(K, 0).

test(row_clear_preserves_other_rows, [nondet, setup((promini_init, control_init)),
        cleanup((keyboard_uninit(K), synth_voice_uninit(V1), synth_voice_uninit(V2),
                 mod_source_uninit(E1), mod_source_uninit(E2)))]) :-
    keyboard_init(K),
    synth_voice_init(V1),
    synth_voice_init(V2),
    mod_envelope_init(0.2, 0.2, 0.2, 0.7, 0.4, 500.0, false, E1),
    mod_envelope_init(0.2, 0.2, 0.2, 0.7, 0.4, 500.0, false, E2),
    keyboard_row_add_voice(K, 0, V1, [E1]),
    keyboard_row_add_voice(K, 1, V2, [E2]),
    keyboard_row_clear(K, 0).

test(row_clear_invalid_row, [error(domain_error(keyboard_row, -1)),
        setup(control_init), cleanup(keyboard_uninit(K))]) :-
    keyboard_init(K),
    keyboard_row_clear(K, -1).

:- end_tests(keyboard).
