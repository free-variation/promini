:- use_module('src/prolog/promini.pro').

demo_visualizer :-
    promini_init,
    control_init,
    sound_load('audio/guitar.wav', S),
    sound_loop(S),
    sound_start(S),
    visualizer_attach(S, true, V),
    format('Sound: ~w~n', [S]),
    format('Visualizer: ~w~n', [V]),
    format('Controls:~n'),
    format('  T     - toggle triggered/scrolling waveform~n'),
    format('  Tab   - toggle waveform/spectrogram~n'),
    format('  Space - pause/resume~n'),
    format('  ESC   - close visualizer~n').

demo_visualizer_synth :-
    promini_init,
    control_init,
    synth_voice_init(V),
    synth_oscillator_add(V, 220.0, 0.0, O1),
    synth_oscillator_add(V, 440.0, 0.0, O2),
    synth_oscillator_add(V, 660.0, 0.0, O3),
    synth_oscillator_add(V, 880.0, 0.0, O4),
    synth_oscillator_set_volume(O1, 0.5),
    synth_oscillator_set_volume(O2, 0.25),
    synth_oscillator_set_volume(O3, 0.17),
    synth_oscillator_set_volume(O4, 0.125),
    synth_voice_start(V),
    visualizer_attach(V, true, Viz),
    format('Visualizer: ~w~n', [Viz]),
    format('T to toggle trigger - should stabilize the waveform~n').

demo_visualizer_multi :-
    promini_init,
    control_init,
    % Two different sources
    sound_load('audio/guitar.wav', S),
    sound_loop(S),
    synth_voice_init(V),
    synth_oscillator_add(V, 220.0, 0.0, O1),
    synth_oscillator_add(V, 440.0, 0.0, O2),
    synth_oscillator_set_volume(O1, 0.3),
    synth_oscillator_set_volume(O2, 0.15),
    % Visualizer for each
    visualizer_attach(S, true, Viz1),
    visualizer_attach(V, true, Viz2),
    sound_start(S),
    synth_voice_start(V),
    format('Viz1 (guitar): ~w~n', [Viz1]),
    format('Viz2 (synth): ~w~n', [Viz2]),
    format('Two independent visualizer windows~n').
