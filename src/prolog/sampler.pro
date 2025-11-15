/*
 * sampler.pro - Prolog interface to miniaudio
 * Copyright (c) 2025 John Stewart
 * Licensed under MIT License
 */

:- module(sampler, [
    sampler_version/1,
    sampler_init/0,
    sampler_devices/1,
    sampler_device_init_capture/1,
    sampler_device_init_playback/1,
    sampler_device_start/1,
    sampler_device_stop/1,
    sampler_device_uninit/1
]).

:- use_foreign_library('../../lib/sampler').

:- initialization(register_cleanup).

register_cleanup :-
    at_halt(unload_foreign_library('../../lib/sampler')).
