:- module(test_setup, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(setup).

test(register_and_get, [nondet]) :-
    audio_load('audio/counting.wav', Handle),
    setup_register(test:audio1, Handle),
    setup_get(test:audio1, Retrieved),
    Retrieved == Handle,
    setup_unload(test).

test(register_multiple, [nondet]) :-
    audio_load('audio/counting.wav', H1),
    audio_load('audio/counting.wav', H2),
    setup_register(multi:a, H1),
    setup_register(multi:b, H2),
    setup_get(multi:a, R1),
    setup_get(multi:b, R2),
    R1 == H1,
    R2 == H2,
    setup_unload(multi).

test(get_nonexistent, [fail]) :-
    setup_get(nonexistent:label, _).


test(setups_empty, [nondet]) :-
    setups(Names),
    \+ member(unique_test_setup_xyz, Names).

test(setups_after_register, [nondet]) :-
    audio_load('audio/counting.wav', H),
    setup_register(listed:item, H),
    setups(Names),
    member(listed, Names),
    setup_unload(listed).


test(unload_clears_registry, [nondet]) :-
    audio_load('audio/counting.wav', H),
    setup_register(cleartest:item, H),
    setup_get(cleartest:item, _),
    setup_unload(cleartest),
    \+ setup_get(cleartest:item, _).

test(unload_multiple_types, [nondet]) :-
    promini_init,
    granular_init(1.0, G),
    mod_lfo_init(sine, 1.0, Lfo),
    setup_register(types:grain, G),
    setup_register(types:lfo, Lfo),
    setup_unload(types),
    \+ setup_get(types:grain, _),
    \+ setup_get(types:lfo, _).

test(unload_nonexistent, [nondet]) :-
    setup_unload(does_not_exist).


test(separate_setups, [nondet]) :-
    audio_load('audio/counting.wav', H1),
    audio_load('audio/counting.wav', H2),
    setup_register(setup_a:item, H1),
    setup_register(setup_b:item, H2),
    setup_unload(setup_a),
    \+ setup_get(setup_a:item, _),
    setup_get(setup_b:item, Retrieved),
    Retrieved == H2,
    setup_unload(setup_b).

:- end_tests(setup).
