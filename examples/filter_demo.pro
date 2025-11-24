:- use_module('src/prolog/sampler.pro').

demo_lpf :-
    format('Loading sound...~n'),
    sampler_sound_load('audio/counting.wav', Sound),

    format('Playing original sound...~n'),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    format('Attaching low-pass filter at 2000 Hz...~n'),
    sampler_sound_attach_effect(Sound, lpf, [cutoff=2000.0], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    format('Attaching low-pass filter at 1000 Hz (more muffled)...~n'),
    sampler_sound_seek(Sound, 0),
    sampler_sound_attach_effect(Sound, lpf, [cutoff=1000.0], _),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    format('Attaching low-pass filter at 500 Hz (very muffled)...~n'),
    sampler_sound_seek(Sound, 0),
    sampler_sound_attach_effect(Sound, lpf, [cutoff=500.0], _),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    format('Attaching low-pass filter at 300 Hz, order 8 (steep rolloff)...~n'),
    sampler_sound_seek(Sound, 0),
    sampler_sound_attach_effect(Sound, lpf, [cutoff=300.0, order=8], _),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    sampler_sound_unload(Sound),
    format('LPF demo complete.~n').

demo_hpf :-
    format('Loading sound...~n'),
    sampler_sound_load('audio/counting.wav', Sound),

    format('Playing original sound...~n'),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    format('Attaching high-pass filter at 200 Hz...~n'),
    sampler_sound_attach_effect(Sound, hpf, [cutoff=200.0], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    format('Attaching high-pass filter at 500 Hz (thinner)...~n'),
    sampler_sound_seek(Sound, 0),
    sampler_sound_attach_effect(Sound, hpf, [cutoff=500.0], _),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    format('Attaching high-pass filter at 1000 Hz (very thin)...~n'),
    sampler_sound_seek(Sound, 0),
    sampler_sound_attach_effect(Sound, hpf, [cutoff=1000.0], _),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    format('Attaching high-pass filter at 2000 Hz, order 8 (extreme)...~n'),
    sampler_sound_seek(Sound, 0),
    sampler_sound_attach_effect(Sound, hpf, [cutoff=2000.0, order=8], _),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    sampler_sound_unload(Sound),
    format('HPF demo complete.~n').

demo_combined :-
    format('Loading sound...~n'),
    sampler_sound_load('audio/counting.wav', Sound),

    format('Playing original sound...~n'),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    format('Attaching band-pass effect (HPF 300 Hz + LPF 3000 Hz)...~n'),
    sampler_sound_attach_effect(Sound, hpf, [cutoff=300.0], _),
    sampler_sound_attach_effect(Sound, lpf, [cutoff=3000.0], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    format('Clearing effects...~n'),
    sampler_sound_clear_effects(Sound),

    format('Attaching narrow band-pass (HPF 800 Hz + LPF 1200 Hz)...~n'),
    sampler_sound_attach_effect(Sound, hpf, [cutoff=800.0], _),
    sampler_sound_attach_effect(Sound, lpf, [cutoff=1200.0], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    sampler_sound_unload(Sound),
    format('Combined filter demo complete.~n').

demo_dynamic :-
    format('Loading sound...~n'),
    sampler_sound_load('audio/counting.wav', Sound),

    format('Playing with dynamic low-pass filter sweep...~n'),
    sampler_sound_attach_effect(Sound, lpf, [cutoff=5000.0], Effect),
    sampler_sound_loop(Sound),
    sampler_sound_start(Sound),

    format('Sweeping cutoff from 5000 Hz down to 300 Hz...~n'),
    sweep_lpf_down(Effect, 5000.0, 300.0, 20),

    sleep(1),
    sampler_sound_stop(Sound),
    sampler_sound_unload(Sound),
    format('Dynamic filter demo complete.~n').

sweep_lpf_down(_, Cutoff, Target, _) :-
    Cutoff =< Target, !.
sweep_lpf_down(Effect, Cutoff, Target, Steps) :-
    sampler_effect_set_parameters(Effect, [cutoff=Cutoff]),
    sleep(0.2),
    Step is (Cutoff - Target) / Steps,
    NewCutoff is Cutoff - Step,
    sweep_lpf_down(Effect, NewCutoff, Target, Steps).
