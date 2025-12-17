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
    synth_oscillator_add(V, 220.0, 0.0, _),
    synth_voice_start(V),
    visualizer_attach(V, true, Viz),
    format('Visualizer: ~w~n', [Viz]),
    format('T to toggle trigger - should stabilize the sine wave~n').
