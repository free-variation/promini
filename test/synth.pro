:- module(test_synth, []).
:- use_module('src/prolog/sampler.pro').
:- use_module(library(plunit)).

:- begin_tests(synth).

test(voices_in_use_initially_zero, [nondet]) :-
    sampler_synth_voices_in_use(Count),
    Count =:= 0.

test(create_voice, [nondet, cleanup(sampler_synth_voice_unload(Handle))]) :-
    sampler_synth_voice_create(Handle),
    integer(Handle),
    Handle >= 0.

test(voices_in_use_after_create, [nondet, cleanup(sampler_synth_voice_unload(Handle))]) :-
    sampler_synth_voices_in_use(Before),
    sampler_synth_voice_create(Handle),
    sampler_synth_voices_in_use(After),
    After =:= Before + 1.

test(start_and_stop_voice, [nondet, cleanup(sampler_synth_voice_unload(Handle))]) :-
    sampler_synth_voice_create(Handle),
    sampler_synth_voice_start(Handle),
    sampler_synth_voice_stop(Handle).

test(default_frequency_is_440, [nondet, cleanup(sampler_synth_voice_unload(Handle))]) :-
    sampler_synth_voice_create(Handle),
    sampler_synth_voice_get_frequency(Handle, Freq),
    Freq =:= 440.0.

test(set_and_get_frequency, [nondet, cleanup(sampler_synth_voice_unload(Handle))]) :-
    sampler_synth_voice_create(Handle),
    sampler_synth_voice_set_frequency(Handle, 880.0),
    sampler_synth_voice_get_frequency(Handle, Freq),
    Freq =:= 880.0.

test(default_pan_is_center, [nondet, cleanup(sampler_synth_voice_unload(Handle))]) :-
    sampler_synth_voice_create(Handle),
    sampler_synth_voice_get_pan(Handle, Pan),
    Pan =:= 0.0.

test(set_and_get_pan, [nondet, cleanup(sampler_synth_voice_unload(Handle))]) :-
    sampler_synth_voice_create(Handle),
    sampler_synth_voice_set_pan(Handle, -0.5),
    sampler_synth_voice_get_pan(Handle, Pan),
    Pan =:= -0.5.

:- end_tests(synth).
