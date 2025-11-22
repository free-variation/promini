:- use_module('src/prolog/sampler.pro').

demo_envelope :-
    format('Loading sound...~n'),
    sampler_sound_load('audio/counting.wav', Sound1),

    format('Playing original sound...~n'),
    sampler_sound_start(Sound1),
    sleep(3),
    sampler_sound_stop(Sound1),
    sampler_sound_unload(Sound1),

    format('Attaching short percussive envelope (50ms, no loop)...~n'),
    sampler_sound_load('audio/counting.wav', Sound2),
    sampler_sound_attach_envelope(Sound2, 0.1, 0.2, 0.5, 0.3, 50.0, false, _),
    sampler_sound_start(Sound2),
    sleep(3),
    sampler_sound_stop(Sound2),
    sampler_sound_unload(Sound2),

    format('Attaching longer envelope (500ms) with sustained break (no loop)...~n'),
    sampler_sound_load('audio/counting.wav', Sound3),
    sampler_sound_attach_envelope(Sound3, 0.1, 0.1, 0.6, 0.7, 500.0, false, _),
    sampler_sound_start(Sound3),
    sleep(3),
    sampler_sound_stop(Sound3),
    sampler_sound_unload(Sound3),

    format('Attaching looping envelope (200ms)...~n'),
    sampler_sound_load('audio/counting.wav', Sound4),
    sampler_sound_attach_envelope(Sound4, 0.2, 0.3, 0.3, 0.5, 200.0, true, _),
    sampler_sound_start(Sound4),
    sleep(3),
    sampler_sound_stop(Sound4),
    sampler_sound_unload(Sound4),

    format('Demo complete.~n').

demo_gong :-
    format('Loading gong...~n'),
    sampler_sound_load('audio/gong.wav', Sound1),

    format('Playing original gong...~n'),
    sampler_sound_start(Sound1),
    sleep(3),
    sampler_sound_stop(Sound1),
    sampler_sound_unload(Sound1),

    format('Playing with short percussive envelope (100ms, no loop)...~n'),
    sampler_sound_load('audio/gong.wav', Sound2),
    sampler_sound_attach_envelope(Sound2, 0.05, 0.15, 0.6, 0.4, 100.0, false, _),
    sampler_sound_start(Sound2),
    sleep(3),
    sampler_sound_stop(Sound2),
    sampler_sound_unload(Sound2),

    format('Playing with looping envelope (300ms)...~n'),
    sampler_sound_load('audio/gong.wav', Sound3),
    sampler_sound_attach_envelope(Sound3, 0.1, 0.2, 0.4, 0.6, 300.0, true, _),
    sampler_sound_start(Sound3),
    sleep(3),
    sampler_sound_stop(Sound3),
    sampler_sound_unload(Sound3),

    format('Gong demo complete.~n').
