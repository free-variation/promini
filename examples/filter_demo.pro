:- use_module('src/prolog/promini.pro').

demo_lpf :-
    format('Loading sound...~n'),
    sound_load('audio/counting.wav', Sound),

    format('Playing original sound...~n'),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Attaching low-pass filter at 2000 Hz...~n'),
    sound_attach_effect(Sound, lpf, [cutoff=2000.0], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Attaching low-pass filter at 1000 Hz (more muffled)...~n'),
    sound_seek(Sound, 0),
    sound_attach_effect(Sound, lpf, [cutoff=1000.0], _),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Attaching low-pass filter at 500 Hz (very muffled)...~n'),
    sound_seek(Sound, 0),
    sound_attach_effect(Sound, lpf, [cutoff=500.0], _),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Attaching low-pass filter at 300 Hz, order 8 (steep rolloff)...~n'),
    sound_seek(Sound, 0),
    sound_attach_effect(Sound, lpf, [cutoff=300.0, order=8], _),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    sound_unload(Sound),
    format('LPF demo complete.~n').

demo_hpf :-
    format('Loading sound...~n'),
    sound_load('audio/counting.wav', Sound),

    format('Playing original sound...~n'),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Attaching high-pass filter at 200 Hz...~n'),
    sound_attach_effect(Sound, hpf, [cutoff=200.0], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Attaching high-pass filter at 500 Hz (thinner)...~n'),
    sound_seek(Sound, 0),
    sound_attach_effect(Sound, hpf, [cutoff=500.0], _),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Attaching high-pass filter at 1000 Hz (very thin)...~n'),
    sound_seek(Sound, 0),
    sound_attach_effect(Sound, hpf, [cutoff=1000.0], _),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Attaching high-pass filter at 2000 Hz, order 8 (extreme)...~n'),
    sound_seek(Sound, 0),
    sound_attach_effect(Sound, hpf, [cutoff=2000.0, order=8], _),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    sound_unload(Sound),
    format('HPF demo complete.~n').

demo_combined :-
    format('Loading sound...~n'),
    sound_load('audio/counting.wav', Sound),

    format('Playing original sound...~n'),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Attaching band-pass effect (HPF 300 Hz + LPF 3000 Hz)...~n'),
    sound_attach_effect(Sound, hpf, [cutoff=300.0], _),
    sound_attach_effect(Sound, lpf, [cutoff=3000.0], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Clearing effects...~n'),
    clear_effects(sound(Sound)),

    format('Attaching narrow band-pass (HPF 800 Hz + LPF 1200 Hz)...~n'),
    sound_attach_effect(Sound, hpf, [cutoff=800.0], _),
    sound_attach_effect(Sound, lpf, [cutoff=1200.0], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    sound_unload(Sound),
    format('Combined filter demo complete.~n').

demo_dynamic :-
    format('Loading sound...~n'),
    sound_load('audio/counting.wav', Sound),

    format('Playing with dynamic low-pass filter sweep...~n'),
    sound_attach_effect(Sound, lpf, [cutoff=5000.0], Effect),
    sound_loop(Sound),
    sound_start(Sound),

    format('Sweeping cutoff from 5000 Hz down to 300 Hz...~n'),
    sweep_lpf_down(Effect, 5000.0, 300.0, 20),

    sleep(1),
    sound_stop(Sound),
    sound_unload(Sound),
    format('Dynamic filter demo complete.~n').

sweep_lpf_down(Effect, Start, Target, Steps) :-
    Step is (Start - Target) / Steps,
    sweep_lpf_down_(Effect, Start, Target, Step).

sweep_lpf_down_(_, Cutoff, Target, _) :-
    Cutoff =< Target, !.
sweep_lpf_down_(Effect, Cutoff, Target, Step) :-
    effect_set_parameters(Effect, [cutoff=Cutoff]),
    sleep(0.2),
    NewCutoff is Cutoff - Step,
    sweep_lpf_down_(Effect, NewCutoff, Target, Step).

demo_bpf :-
    format('Loading sound...~n'),
    sound_load('audio/counting.wav', Sound),

    format('Playing original sound...~n'),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Attaching band-pass filter at 1000 Hz...~n'),
    sound_attach_effect(Sound, bpf, [cutoff=1000.0], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Clearing and adding band-pass at 500 Hz (lower frequencies)...~n'),
    clear_effects(sound(Sound)),
    sound_attach_effect(Sound, bpf, [cutoff=500.0], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Clearing and adding band-pass at 2000 Hz (higher frequencies)...~n'),
    clear_effects(sound(Sound)),
    sound_attach_effect(Sound, bpf, [cutoff=2000.0], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Clearing and adding band-pass at 1000 Hz, order 8 (narrower band)...~n'),
    clear_effects(sound(Sound)),
    sound_attach_effect(Sound, bpf, [cutoff=1000.0, order=8], _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    sound_unload(Sound),
    format('BPF demo complete.~n').

demo_bpf_sweep :-
    format('Loading sound...~n'),
    sound_load('audio/counting.wav', Sound),

    format('Playing with dynamic band-pass filter sweep...~n'),
    sound_attach_effect(Sound, bpf, [cutoff=300.0], Effect),
    sound_loop(Sound),
    sound_start(Sound),

    format('Sweeping center frequency from 300 Hz up to 3000 Hz...~n'),
    sweep_bpf_up(Effect, 300.0, 3000.0, 30),

    format('Sweeping center frequency back down to 300 Hz...~n'),
    sweep_bpf_down(Effect, 3000.0, 300.0, 30),

    sleep(1),
    sound_stop(Sound),
    sound_unload(Sound),
    format('BPF sweep demo complete.~n').

sweep_bpf_up(Effect, Start, Target, Steps) :-
    Step is (Target - Start) / Steps,
    sweep_bpf_up_(Effect, Start, Target, Step).

sweep_bpf_up_(_, Cutoff, Target, _) :-
    Cutoff >= Target, !.
sweep_bpf_up_(Effect, Cutoff, Target, Step) :-
    effect_set_parameters(Effect, [cutoff=Cutoff]),
    sleep(0.15),
    NewCutoff is Cutoff + Step,
    sweep_bpf_up_(Effect, NewCutoff, Target, Step).

sweep_bpf_down(Effect, Start, Target, Steps) :-
    Step is (Start - Target) / Steps,
    sweep_bpf_down_(Effect, Start, Target, Step).

sweep_bpf_down_(_, Cutoff, Target, _) :-
    Cutoff =< Target, !.
sweep_bpf_down_(Effect, Cutoff, Target, Step) :-
    effect_set_parameters(Effect, [cutoff=Cutoff]),
    sleep(0.15),
    NewCutoff is Cutoff - Step,
    sweep_bpf_down_(Effect, NewCutoff, Target, Step).

demo_lfo_lpf :-
    format('~n=== LFO -> LPF Cutoff Demo ===~n~n'),

    sound_load('audio/guitar.wav', Sound),
    sound_loop(Sound),

    format('Attaching LPF at 1500 Hz...~n'),
    sound_attach_effect(Sound, lpf, [cutoff=1500.0], effect(_Source, LpfPtr)),

    format('Creating LFO at 0.5 Hz to modulate cutoff (500-2500 Hz)...~n'),
    mod_lfo_create(sine, 0.5, Lfo),
    mod_route_create(Lfo, lpf, LpfPtr, cutoff, 1000.0, 1500.0, 0.0, Route),

    format('Playing guitar with auto-wah for 8 seconds...~n'),
    sound_start(Sound),
    sleep(8.0),

    sound_stop(Sound),
    mod_route_unload(Route),
    mod_source_unload(Lfo),
    sound_unload(Sound),
    format('Demo complete!~n~n').

demo_envelope_lpf :-
    format('~n=== Envelope -> LPF Cutoff Demo ===~n~n'),

    synth_voice_create(Voice),
    % Add harmonics for a richer sound to filter
    synth_oscillator_add(Voice, 110.0, 0.0, _),
    synth_oscillator_add(Voice, 220.0, 0.0, _),
    synth_oscillator_add(Voice, 330.0, 0.0, _),
    synth_oscillator_add(Voice, 440.0, 0.0, _),
    synth_oscillator_add(Voice, 550.0, 0.0, _),
    synth_oscillator_add(Voice, 660.0, 0.0, _),

    format('Attaching LPF at 200 Hz...~n'),
    voice_attach_effect(Voice, lpf, [cutoff=200.0], effect(_Source, LpfPtr)),

    format('Creating filter envelope (opens then closes)...~n'),
    mod_envelope_create(0.1, 0.3, 0.2, 0.5, 0.4, 200.0, false, Env),
    mod_route_create(Env, lpf, LpfPtr, cutoff, 2000.0, 200.0, 0.0, Route),

    format('Playing triggered filter sweeps...~n'),
    synth_voice_start(Voice),

    trigger_notes(Env, 8, 0.4),

    sleep(0.5),
    mod_route_unload(Route),
    mod_source_unload(Env),
    synth_voice_unload(Voice),
    format('Demo complete!~n~n').

trigger_notes(_, 0, _) :- !.
trigger_notes(Env, N, Delay) :-
    N > 0,
    format('  Trigger ~w~n', [N]),
    mod_envelope_trigger(Env),
    sleep(Delay),
    N1 is N - 1,
    trigger_notes(Env, N1, Delay).

demo_moog :-
    format('~n=== Moog Ladder Filter Demo ===~n~n'),

    sound_load('audio/guitar.wav', Sound),

    format('Playing original sound...~n'),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),
    sound_unload(Sound),

    sound_load('audio/guitar.wav', Sound2),
    format('MA LPF at 1000 Hz (2-pole, 12dB/octave)...~n'),
    sound_attach_effect(Sound2, lpf, [cutoff=1000.0], _),
    sound_start(Sound2),
    sleep(3),
    sound_stop(Sound2),
    sound_unload(Sound2),

    sound_load('audio/guitar.wav', Sound3),
    format('Moog filter at 1000 Hz, resonance 0 (4-pole, 24dB/octave)...~n'),
    sound_attach_effect(Sound3, moog, [cutoff=1000.0, resonance=0.0], _),
    sound_start(Sound3),
    sleep(3),
    sound_stop(Sound3),
    sound_unload(Sound3),

    sound_load('audio/guitar.wav', Sound4),
    format('Moog at 1000 Hz with resonance 2.0...~n'),
    sound_attach_effect(Sound4, moog, [cutoff=1000.0, resonance=2.0], _),
    sound_start(Sound4),
    sleep(3),
    sound_stop(Sound4),
    sound_unload(Sound4),

    sound_load('audio/guitar.wav', Sound5),
    format('Moog at 300 Hz with resonance 1.5...~n'),
    sound_attach_effect(Sound5, moog, [cutoff=300.0, resonance=1.5], _),
    sound_start(Sound5),
    sleep(3),
    sound_stop(Sound5),
    sound_unload(Sound5),

    format('Moog demo complete!~n~n').

demo_lfo_moog :-
    format('~n=== LFO -> Moog Cutoff Demo (Auto-Wah) ===~n~n'),

    sound_load('audio/guitar.wav', Sound),
    sound_loop(Sound),

    format('Attaching Moog filter at 1500 Hz with resonance 1.5...~n'),
    sound_attach_effect(Sound, moog, [cutoff=1500.0, resonance=1.5], effect(_Source, MoogPtr)),

    format('Creating LFO at 4.0 Hz to modulate cutoff (500-2500 Hz)...~n'),
    mod_lfo_create(sine, 4.0, Lfo),
    mod_route_create(Lfo, moog, MoogPtr, cutoff, 1000.0, 1500.0, 0.0, Route),

    format('Playing guitar with auto-wah for 8 seconds...~n'),
    sound_start(Sound),
    sleep(8.0),

    sound_stop(Sound),
    mod_route_unload(Route),
    mod_source_unload(Lfo),
    sound_unload(Sound),
    format('Demo complete!~n~n').

demo_envelope_moog :-
    format('~n=== Envelope -> Moog Cutoff Demo ===~n~n'),

    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 110.0, 0.5, _),
    synth_oscillator_add(Voice, 220.0, 0.3, _),
    synth_oscillator_add(Voice, 330.0, 0.2, _),
    synth_oscillator_add(Voice, 440.0, 0.15, _),
    synth_oscillator_add(Voice, 550.0, 0.1, _),

    format('Attaching Moog filter at 200 Hz with resonance 2.0...~n'),
    voice_attach_effect(Voice, moog, [cutoff=200.0, resonance=2.0], effect(_Source, MoogPtr)),

    format('Creating filter envelope (fast attack, medium decay)...~n'),
    mod_envelope_create(0.01, 0.2, 0.3, 0.4, 0.3, 400.0, false, Env),
    mod_route_create(Env, moog, MoogPtr, cutoff, 3000.0, 200.0, 0.0, Route),

    format('Playing triggered filter sweeps...~n'),
    synth_voice_start(Voice),

    trigger_notes(Env, 8, 0.4),

    sleep(0.5),
    mod_route_unload(Route),
    mod_source_unload(Env),
    synth_voice_unload(Voice),
    format('Demo complete!~n~n').
