/*
 * sampler.pro - Prolog interface to miniaudio (engine API)
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

:- module(sampler, [
    sampler_version/1,
    sampler_init/0,
    sampler_devices/1
]).

:- use_foreign_library('../../lib/sampler').

:- initialization(register_cleanup).

register_cleanup :-
    at_halt(unload_foreign_library('../../lib/sampler')).
