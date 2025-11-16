/*
 * sampler.pro - Prolog interface to miniaudio (engine API)
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

:- module(sampler, [
      sampler_version/1,
      sampler_init/0,
      sampler_devices/1,
      sampler_sound_load/2,
      sampler_sound_unload/1,
      sampler_sound_start/1,
      sampler_sound_stop/1,
      sampler_sound_is_playing/1,
      sampler_sound_loop/1,
      sampler_sound_no_loop/1,
      sampler_sound_is_looping/1,
      sampler_data_load/2,
      sampler_data_unload/1,
      sampler_sound_create/2
  ]).

:- use_foreign_library('../../lib/sampler').

:- initialization(register_cleanup).

register_cleanup :-
    at_halt(unload_foreign_library('../../lib/sampler')).

sampler_sound_loop(Handle) :-
    sampler_sound_set_looping(Handle, true).

sampler_sound_no_loop(Handle) :-
    sampler_sound_set_looping(Handle, false).


