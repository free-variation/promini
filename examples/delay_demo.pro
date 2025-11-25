:- use_module('src/prolog/sampler.pro').

demo_delay :-
    format('Loading sound...~n'),
    sampler_sound_load('audio/counting.wav', Sound),

    format('Playing original sound...~n'),
    sampler_sound_start(Sound),
    sleep(4),
    sampler_sound_stop(Sound),

    format('Attaching delay (0.5s, no feedback)...~n'),
    sampler_sound_attach_effect(Sound, delay, [delay_in_frames=22050, decay=0.0, wet=1.0, dry=1.0], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(5),
    sampler_sound_stop(Sound),

    format('Clearing and adding echo effect (0.3s delay, 50%% feedback)...~n'),
    sampler_sound_clear_effects(Sound),
    sampler_sound_attach_effect(Sound, delay, [delay_in_frames=13230, decay=0.5, wet=0.8, dry=1.0], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(6),
    sampler_sound_stop(Sound),

    format('Clearing and adding short slapback delay (0.1s, no feedback)...~n'),
    sampler_sound_clear_effects(Sound),
    sampler_sound_attach_effect(Sound, delay, [delay_in_frames=4410, decay=0.0, wet=0.6, dry=1.0], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(5),
    sampler_sound_stop(Sound),

    format('Clearing and adding long echo (1s delay, 70%% feedback)...~n'),
    sampler_sound_clear_effects(Sound),
    sampler_sound_attach_effect(Sound, delay, [delay_in_frames=44100, decay=0.7, wet=0.5, dry=1.0], _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(8),
    sampler_sound_stop(Sound),

    sampler_sound_unload(Sound),
    format('Delay demo complete.~n').

demo_delay_dynamic :-
    format('Loading sound...~n'),
    sampler_sound_load('audio/counting.wav', Sound),

    format('Playing with dynamic delay parameters...~n'),
    sampler_sound_attach_effect(Sound, delay, [delay_in_frames=22050, decay=0.0, wet=1.0, dry=1.0], Effect),
    sampler_sound_loop(Sound),
    sampler_sound_start(Sound),

    format('Increasing feedback from 0%% to 80%%...~n'),
    sweep_decay(Effect, 0.0, 0.8, 20),

    sleep(2),

    format('Decreasing wet mix from 100%% to 20%%...~n'),
    sweep_wet(Effect, 1.0, 0.2, 20),

    sleep(2),
    sampler_sound_stop(Sound),
    sampler_sound_unload(Sound),
    format('Dynamic delay demo complete.~n').

sweep_decay(Effect, Start, Target, Steps) :-
    Step is (Target - Start) / Steps,
    sweep_decay_(Effect, Start, Target, Step).

sweep_decay_(_, Decay, Target, _) :-
    Decay >= Target, !.
sweep_decay_(Effect, Decay, Target, Step) :-
    sampler_effect_set_parameters(Effect, [decay=Decay]),
    sleep(0.2),
    NewDecay is Decay + Step,
    sweep_decay_(Effect, NewDecay, Target, Step).

sweep_wet(Effect, Start, Target, Steps) :-
    Step is (Start - Target) / Steps,
    sweep_wet_(Effect, Start, Target, Step).

sweep_wet_(_, Wet, Target, _) :-
    Wet =< Target, !.
sweep_wet_(Effect, Wet, Target, Step) :-
    sampler_effect_set_parameters(Effect, [wet=Wet]),
    sleep(0.2),
    NewWet is Wet - Step,
    sweep_wet_(Effect, NewWet, Target, Step).

demo_ping_pong :-
    format('Loading sound...~n'),
    sampler_sound_load('audio/counting.wav', Sound),

    format('Playing original sound...~n'),
    sampler_sound_start(Sound),
    sleep(4),
    sampler_sound_stop(Sound),

    format('Attaching ping-pong delay (0.25s, no feedback)...~n'),
    sampler_sound_attach_ping_pong_delay(Sound, 22050, 11025, 0.0, 0.8, _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(5),
    sampler_sound_stop(Sound),

    format('Clearing and adding ping-pong with feedback (0.25s, 50%% feedback)...~n'),
    sampler_sound_clear_effects(Sound),
    sampler_sound_attach_ping_pong_delay(Sound, 22050, 11025, 0.5, 0.8, _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(6),
    sampler_sound_stop(Sound),

    format('Clearing and adding fast ping-pong (0.1s, 70%% feedback)...~n'),
    sampler_sound_clear_effects(Sound),
    sampler_sound_attach_ping_pong_delay(Sound, 22050, 4410, 0.7, 0.8, _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(6),
    sampler_sound_stop(Sound),

    format('Clearing and adding slow ping-pong (0.5s, 60%% feedback)...~n'),
    sampler_sound_clear_effects(Sound),
    sampler_sound_attach_ping_pong_delay(Sound, 44100, 22050, 0.6, 0.7, _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(8),
    sampler_sound_stop(Sound),

    sampler_sound_unload(Sound),
    format('Ping-pong delay demo complete.~n').

demo_ping_pong_dynamic :-
    format('Loading sound...~n'),
    sampler_sound_load('audio/counting.wav', Sound),

    format('Playing with dynamic ping-pong delay...~n'),
    sampler_sound_attach_ping_pong_delay(Sound, 44100, 22050, 0.5, 0.8, Effect),
    sampler_sound_loop(Sound),
    sampler_sound_start(Sound),

    format('Sweeping delay time from 0.5s down to 0.1s...~n'),
    sweep_delay(Effect, 22050, 4410, 20),

    sleep(2),

    format('Sweeping delay time back up to 0.5s...~n'),
    sweep_delay_up(Effect, 4410, 22050, 20),

    sleep(2),
    sampler_sound_stop(Sound),
    sampler_sound_unload(Sound),
    format('Dynamic ping-pong demo complete.~n').

sweep_delay(Effect, Start, Target, Steps) :-
    Step is (Start - Target) / Steps,
    sweep_delay_(Effect, Start, Target, Step).

sweep_delay_(_, Delay, Target, _) :-
    Delay =< Target, !.
sweep_delay_(Effect, Delay, Target, Step) :-
    DelayInt is round(Delay),
    sampler_effect_set_parameters(Effect, [delay_in_frames=DelayInt]),
    sleep(0.15),
    NewDelay is Delay - Step,
    sweep_delay_(Effect, NewDelay, Target, Step).

sweep_delay_up(Effect, Start, Target, Steps) :-
    Step is (Target - Start) / Steps,
    sweep_delay_up_(Effect, Start, Target, Step).

sweep_delay_up_(_, Delay, Target, _) :-
    Delay >= Target, !.
sweep_delay_up_(Effect, Delay, Target, Step) :-
    DelayInt is round(Delay),
    sampler_effect_set_parameters(Effect, [delay_in_frames=DelayInt]),
    sleep(0.15),
    NewDelay is Delay + Step,
    sweep_delay_up_(Effect, NewDelay, Target, Step).

demo_smoothing_modes :-
    format('Loading sound...~n'),
    sampler_sound_load('audio/counting.wav', Sound),

    format('~n=== Mode 0: No smoothing (clicks expected) ===~n'),
    sampler_sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100, delay_in_frames=22050, feedback=0.5, wet=0.8, smoothing_mode=0], Effect0),
    sampler_sound_loop(Sound),
    sampler_sound_start(Sound),
    format('Sweeping delay time...~n'),
    sweep_delay(Effect0, 22050, 4410, 20),
    sleep(1),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Mode 1: Pitch-shift smoothing (tape delay style) ===~n'),
    sampler_sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100, delay_in_frames=22050, feedback=0.5, wet=0.8, smoothing_mode=1, smoothing_speed=0.1], Effect1),
    sampler_sound_loop(Sound),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    format('Setting target delay from 22050 to 4410 (pitch up, ~~4s transition)...~n'),
    sampler_effect_set_parameters(Effect1, [delay_in_frames=4410]),
    sleep(5),
    format('Setting target delay from 4410 to 22050 (pitch down, ~~4s transition)...~n'),
    sampler_effect_set_parameters(Effect1, [delay_in_frames=22050]),
    sleep(5),
    sampler_sound_stop(Sound),
    sampler_sound_clear_effects(Sound),

    format('~n=== Mode 2: Crossfade smoothing ===~n'),
    sampler_sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100, delay_in_frames=22050, feedback=0.5, wet=0.8, smoothing_mode=2, crossfade_length=2048], Effect2),
    sampler_sound_loop(Sound),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    format('Sweeping delay time...~n'),
    sweep_delay(Effect2, 22050, 4410, 20),
    sleep(1),
    sampler_sound_stop(Sound),

    sampler_sound_unload(Sound),
    format('~nSmoothing modes demo complete.~n').
