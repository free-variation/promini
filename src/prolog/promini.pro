/*
 * promini.pro - Prolog interface to miniaudio (engine API)
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

:- module(promini, [
    promini_version/1,
    promini_init/0,
    promini_init/1,
    promini_devices/1,
    sound_load/2,
    sound_load_reversed/2,
    sound_unload/1,
    sound_start/1,
    sound_stop/1,
    sound_is_playing/1,
    sound_set_looping/2,
    sound_is_looping/1,
    audio_load/2,
    audio_unload/1,
    sound_create/2,
    sound_seek/2,
    sound_get_position/2,
    audio_info/2,
    sound_length/2,
    sound_set_pitch/2,
    sound_get_pitch/2,
    audio_reverse/2,
    sound_set_range/3,
    audio_extract/4,
    sound_attach_effect/4,
    effects/2,
    effect_set_parameters/2,
    effect_detach/1,
    clear_effects/1,
    capture_start/4,
    capture_stop/1,
    capture_get_info/2,
    capture_extract/4,
    synth_voices_in_use/1,
    synth_voice_init/1,
    synth_voice_start/1,
    synth_voice_stop/1,
    synth_voice_uninit/1,
    voice_attach_effect/4,
    synth_oscillator_add/4,
    synth_oscillator_remove/1,
    synth_noise_add/3,
    synth_oscillator_fade/3,
    synth_oscillator_set_volume/2,
    synth_oscillator_get_volume/2,
    synth_oscillator_set_frequency/2,
    synth_oscillator_get_frequency/2,
    synth_oscillator_set_phase/2,
    synth_oscillator_get_phase/2,
    synth_voice_set_frequency/2,
    mod_lfo_init/3,
    mod_lfo_set_frequency/2,
    mod_lfo_get_frequency/2,
    mod_envelope_init/8,
    mod_envelope_trigger/1,
    mod_source_uninit/1,
    mod_route_init/9,
    mod_route_uninit/1,
    mod_route_monitor/3,
    mod_route_monitor/4,
    mod_gamepad_init/3,
    mod_gamepad_button_init/4,
    mod_keyboard_init/3,
    mod_noise_init/2,
    mod_source_set_sh/2,
    summing_node_init/1,
    summing_node_uninit/1,
    summing_node_connect/2,
    summing_node_disconnect/1,
    summing_node_attach_effect/4,
    control_init/0,
    control_shutdown/0,
    control_gamepads/1,
    control_open/2,
    control_close/1,
    control_pump/0,
    control_axis/3,
    control_debug_buttons/1,
    keyboard_init/1,
    keyboard_uninit/1,
    keyboard_connect/3,
    keyboard_row_set/3,
    keyboard_row_add_voice/4,
    keyboard_row_remove_voice/3,
    keyboard_row_clear/2,
    granular_init/2,
    granular_uninit/1,
    granular_trigger/1,
    granular_set/2,
    granular_get/2,
    granular_set_mode/4,
    granular_connect/2,
    granular_get_frames_recorded/2,
    granular_attach_effect/4,
    clock_set_bpm/1,
    clock_get_bpm/1,
    clock_start/0,
    clock_stop/0,
    clock_is_running/0,
    clock_get_beat_position/1,
    clock_route_init/5,
    clock_route_uninit/1,
    visualizer_attach/3,
    visualizer_detach/1,
    setup_register/2,
    setup_get/2,
    setup_unload/1,
    setups/1,
    load_setup/1,
    load_setup/2,
    unload_setup/1
  ]).

:- use_foreign_library('../../lib/promini').

:- initialization(register_cleanup).

register_cleanup :-
    at_halt(unload_foreign_library('../../lib/promini')).

/* ============================================================================
 * SETUP MANAGEMENT
 * ============================================================================ */

setup_register(Name:Label, Handle) :-
	assertz(setup_object(Name:Label, Handle)).

setup_get(Name:Label, Handle) :-
	setup_object(Name:Label, Handle).

setups(Names) :-
	findall(Name, setup_object(Name:_, _), AllNames),
	sort(AllNames, Names).

setup_unload(Name) :-
	findall(Handle, setup_object(Name:_, Handle), Handles),
	cleanup_order(Order),
	unload_in_order(Order, Handles),
	retractall(setup_object(Name:_, _)).

cleanup_order([clock_route, mod_route, effect, granular, keyboard, voice,
               summing_node, oscillator, mod_source, capture, sound, audio]).

unload_in_order([], _).
unload_in_order([Type|Types], Handles) :-
	include(handle_type(Type), Handles, Matching),
	maplist(uninit_handle, Matching),
	unload_in_order(Types, Handles).

handle_type(effect, effect(_, _)) :- !.
handle_type(Type, Handle) :-
	functor(Handle, Type, _).

uninit_handle(capture(N)) :- capture_stop(capture(N)).
uninit_handle(granular(N)) :- granular_uninit(granular(N)).
uninit_handle(keyboard(N)) :- keyboard_uninit(keyboard(N)).
uninit_handle(voice(N)) :- synth_voice_uninit(voice(N)).
uninit_handle(sound(N)) :- sound_unload(sound(N)).
uninit_handle(audio(N)) :- audio_unload(audio(N)).
uninit_handle(summing_node(N)) :- summing_node_uninit(summing_node(N)).
uninit_handle(oscillator(N)) :- synth_oscillator_remove(oscillator(N)).
uninit_handle(mod_source(N)) :- mod_source_uninit(mod_source(N)).
uninit_handle(mod_route(N)) :- mod_route_uninit(mod_route(N)).
uninit_handle(clock_route(N)) :- clock_route_uninit(clock_route(N)).
uninit_handle(effect(Source, Ptr)) :- effect_detach(effect(Source, Ptr)).

load_setup(Name, Source) :-
	atomic_list_concat(['setups/', Name, '.pro'], Path),
	load_files(Path, [imports([])]),
	Name:setup(Source).

load_setup(Name) :-
	atomic_list_concat(['setups/', Name, '.pro'], Path),
	load_files(Path, [imports([])]),
	Name:setup.

unload_setup(Name) :-
	Name:teardown.


/* ============================================================================
 * EFFECTS MANAGEMENT
 * ============================================================================ */

 clear_effects(Source) :-
    effects(Source, Effects),
    maplist(effect_to_handle, Effects, Handles),
    maplist(effect_detach, Handles).

effect_to_handle(effect(Source, _Type, Ptr, _Params), effect(Source, Ptr)).

/* ============================================================================
 * SOUND MANAGEMENT
 * ============================================================================ */

sound_load(Path, SoundHandle) :-
    audio_load(Path, DataHandle),
    sound_create(DataHandle, SoundHandle),
    audio_unload(DataHandle).

sound_load_reversed(Path, ReversedHandle) :-
    audio_load(Path, TempHandle),
    audio_reverse(TempHandle, ReversedHandle),
    audio_unload(TempHandle).

