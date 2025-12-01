:- use_module('src/prolog/promini.pro').

demo_delay :-
    format('Loading sound...~n'),
    sound_load('audio/counting.wav', Sound),

    format('Playing original sound...~n'),
    sound_start(Sound),
    sleep(4),
    sound_stop(Sound),

    format('Attaching delay (0.5s, no feedback)...~n'),
    sound_attach_effect(Sound, delay, [delay_in_frames=22050, decay=0.0, wet=1.0, dry=1.0], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(5),
    sound_stop(Sound),

    format('Clearing and adding echo effect (0.3s delay, 50%% feedback)...~n'),
    clear_effects(sound(Sound)),
    sound_attach_effect(Sound, delay, [delay_in_frames=13230, decay=0.5, wet=0.8, dry=1.0], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),

    format('Clearing and adding short slapback delay (0.1s, no feedback)...~n'),
    clear_effects(sound(Sound)),
    sound_attach_effect(Sound, delay, [delay_in_frames=4410, decay=0.0, wet=0.6, dry=1.0], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(5),
    sound_stop(Sound),

    format('Clearing and adding long echo (1s delay, 70%% feedback)...~n'),
    clear_effects(sound(Sound)),
    sound_attach_effect(Sound, delay, [delay_in_frames=44100, decay=0.7, wet=0.5, dry=1.0], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(8),
    sound_stop(Sound),

    sound_unload(Sound),
    format('Delay demo complete.~n').

demo_delay_dynamic :-
    format('Loading sound...~n'),
    sound_load('audio/counting.wav', Sound),

    format('Playing with dynamic delay parameters...~n'),
    sound_attach_effect(Sound, delay, [delay_in_frames=22050, decay=0.0, wet=1.0, dry=1.0], Effect),
    sound_loop(Sound),
    sound_start(Sound),

    format('Increasing feedback from 0%% to 80%%...~n'),
    sweep_decay(Effect, 0.0, 0.8, 20),

    sleep(2),

    format('Decreasing wet mix from 100%% to 20%%...~n'),
    sweep_wet(Effect, 1.0, 0.2, 20),

    sleep(2),
    sound_stop(Sound),
    sound_unload(Sound),
    format('Dynamic delay demo complete.~n').

sweep_decay(Effect, Start, Target, Steps) :-
    Step is (Target - Start) / Steps,
    sweep_decay_(Effect, Start, Target, Step).

sweep_decay_(_, Decay, Target, _) :-
    Decay >= Target, !.
sweep_decay_(Effect, Decay, Target, Step) :-
    effect_set_parameters(Effect, [decay=Decay]),
    sleep(0.2),
    NewDecay is Decay + Step,
    sweep_decay_(Effect, NewDecay, Target, Step).

sweep_wet(Effect, Start, Target, Steps) :-
    Step is (Start - Target) / Steps,
    sweep_wet_(Effect, Start, Target, Step).

sweep_wet_(_, Wet, Target, _) :-
    Wet =< Target, !.
sweep_wet_(Effect, Wet, Target, Step) :-
    effect_set_parameters(Effect, [wet=Wet]),
    sleep(0.2),
    NewWet is Wet - Step,
    sweep_wet_(Effect, NewWet, Target, Step).

demo_ping_pong :-
    format('Loading sound...~n'),
    sound_load('audio/counting.wav', Sound),

    format('Playing original sound...~n'),
    sound_start(Sound),
    sleep(4),
    sound_stop(Sound),

    format('Attaching ping-pong delay (0.25s, no feedback)...~n'),
    sound_attach_ping_pong_delay(Sound, 22050, 11025, 0.0, 0.8, _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(5),
    sound_stop(Sound),

    format('Clearing and adding ping-pong with feedback (0.25s, 50%% feedback)...~n'),
    clear_effects(sound(Sound)),
    sound_attach_ping_pong_delay(Sound, 22050, 11025, 0.5, 0.8, _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),

    format('Clearing and adding fast ping-pong (0.1s, 70%% feedback)...~n'),
    clear_effects(sound(Sound)),
    sound_attach_ping_pong_delay(Sound, 22050, 4410, 0.7, 0.8, _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(6),
    sound_stop(Sound),

    format('Clearing and adding slow ping-pong (0.5s, 60%% feedback)...~n'),
    clear_effects(sound(Sound)),
    sound_attach_ping_pong_delay(Sound, 44100, 22050, 0.6, 0.7, _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(8),
    sound_stop(Sound),

    sound_unload(Sound),
    format('Ping-pong delay demo complete.~n').

demo_ping_pong_dynamic :-
    format('Loading sound...~n'),
    sound_load('audio/counting.wav', Sound),

    format('Playing with dynamic ping-pong delay...~n'),
    sound_attach_ping_pong_delay(Sound, 44100, 22050, 0.5, 0.8, Effect),
    sound_loop(Sound),
    sound_start(Sound),

    format('Sweeping delay time from 0.5s down to 0.1s...~n'),
    sweep_delay(Effect, 22050, 4410, 20),

    sleep(2),

    format('Sweeping delay time back up to 0.5s...~n'),
    sweep_delay_up(Effect, 4410, 22050, 20),

    sleep(2),
    sound_stop(Sound),
    sound_unload(Sound),
    format('Dynamic ping-pong demo complete.~n').

sweep_delay(Effect, Start, Target, Steps) :-
    Step is (Start - Target) / Steps,
    sweep_delay_(Effect, Start, Target, Step).

sweep_delay_(_, Delay, Target, _) :-
    Delay =< Target, !.
sweep_delay_(Effect, Delay, Target, Step) :-
    DelayInt is round(Delay),
    effect_set_parameters(Effect, [delay_in_frames=DelayInt]),
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
    effect_set_parameters(Effect, [delay_in_frames=DelayInt]),
    sleep(0.15),
    NewDelay is Delay + Step,
    sweep_delay_up_(Effect, NewDelay, Target, Step).

demo_ping_pong_voice :-
    format('~n=== Ping Pong Delay on Voice ===~n~n'),

    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 440.0, 0.5, _Osc),

    format('Playing without effect for 3 seconds...~n'),
    synth_voice_start(Voice),
    sleep(3.0),

    % Fast ping pong: 100ms delay, 0.6 feedback, 0.7 wet
    format('Adding fast ping pong delay (100ms)...~n'),
    voice_attach_effect(Voice, ping_pong_delay,
        [max_delay_in_frames=48000, delay_in_frames=4800, feedback=0.6, wet=0.7], _Effect),

    format('Playing with ping pong for 6 seconds...~n'),
    sleep(6.0),

    synth_voice_unload(Voice),
    format('Demo complete!~n~n').

demo_smoothing_modes :-
    format('Loading sound...~n'),
    sound_load('audio/counting.wav', Sound),

    format('~n=== Mode 0: No smoothing (clicks expected) ===~n'),
    sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100, delay_in_frames=22050, feedback=0.5, wet=0.8, smoothing_mode=0], Effect0),
    sound_loop(Sound),
    sound_start(Sound),
    format('Sweeping delay time...~n'),
    sweep_delay(Effect0, 22050, 4410, 20),
    sleep(1),
    sound_stop(Sound),
    clear_effects(sound(Sound)),

    format('~n=== Mode 1: Pitch-shift smoothing (tape delay style) ===~n'),
    sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100, delay_in_frames=22050, feedback=0.5, wet=0.8, smoothing_mode=1, smoothing_speed=0.1], Effect1),
    sound_loop(Sound),
    sound_seek(Sound, 0),
    sound_start(Sound),
    format('Setting target delay from 22050 to 4410 (pitch up, ~~4s transition)...~n'),
    effect_set_parameters(Effect1, [delay_in_frames=4410]),
    sleep(5),
    format('Setting target delay from 4410 to 22050 (pitch down, ~~4s transition)...~n'),
    effect_set_parameters(Effect1, [delay_in_frames=22050]),
    sleep(5),
    sound_stop(Sound),
    clear_effects(sound(Sound)),

    format('~n=== Mode 2: Crossfade smoothing ===~n'),
    sound_attach_effect(Sound, ping_pong_delay, [max_delay_in_frames=44100, delay_in_frames=22050, feedback=0.5, wet=0.8, smoothing_mode=2, crossfade_length=2048], Effect2),
    sound_loop(Sound),
    sound_seek(Sound, 0),
    sound_start(Sound),
    format('Sweeping delay time...~n'),
    sweep_delay(Effect2, 22050, 4410, 20),
    sleep(1),
    sound_stop(Sound),

    sound_unload(Sound),
    format('~nSmoothing modes demo complete.~n').
