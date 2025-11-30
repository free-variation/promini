:- use_module('src/prolog/promini.pro').

demo_bitcrush :-
    format('Loading sound...~n'),
    sound_load('audio/counting.wav', Sound),

    format('Playing original sound...~n'),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Attaching 8-bit bitcrush...~n'),
    sound_attach_bitcrush(Sound, 8, 0, _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Attaching 4-bit bitcrush with sample rate reduction...~n'),
    sound_seek(Sound, 0),
    sound_attach_bitcrush(Sound, 4, 8000, _),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Attaching extreme 2-bit bitcrush...~n'),
    sound_seek(Sound, 0),
    sound_attach_bitcrush(Sound, 2, 4000, _),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    sound_unload(Sound),
    format('Demo complete.~n').

demo_gong :-
    format('Loading gong...~n'),
    sound_load('audio/gong.wav', Sound),

    format('Playing original gong...~n'),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    format('Playing with 6-bit crush and 6kHz sample rate...~n'),
    sound_attach_bitcrush(Sound, 6, 6000, _),
    sound_seek(Sound, 0),
    sound_start(Sound),
    sleep(3),
    sound_stop(Sound),

    sound_unload(Sound),
    format('Gong demo complete.~n').
