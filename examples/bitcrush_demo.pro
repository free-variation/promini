:- use_module('src/prolog/sampler.pro').

demo_bitcrush :-
    format('Loading sound...~n'),
    sampler_sound_load('audio/counting.wav', Sound),

    format('Playing original sound...~n'),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    format('Attaching 8-bit bitcrush...~n'),
    sampler_sound_attach_bitcrush(Sound, 8, 0, _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    format('Attaching 4-bit bitcrush with sample rate reduction...~n'),
    sampler_sound_seek(Sound, 0),
    sampler_sound_attach_bitcrush(Sound, 4, 8000, _),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    format('Attaching extreme 2-bit bitcrush...~n'),
    sampler_sound_seek(Sound, 0),
    sampler_sound_attach_bitcrush(Sound, 2, 4000, _),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    sampler_sound_unload(Sound),
    format('Demo complete.~n').

demo_gong :-
    format('Loading gong...~n'),
    sampler_sound_load('audio/gong.wav', Sound),

    format('Playing original gong...~n'),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    format('Playing with 6-bit crush and 6kHz sample rate...~n'),
    sampler_sound_attach_bitcrush(Sound, 6, 6000, _),
    sampler_sound_seek(Sound, 0),
    sampler_sound_start(Sound),
    sleep(3),
    sampler_sound_stop(Sound),

    sampler_sound_unload(Sound),
    format('Gong demo complete.~n').
