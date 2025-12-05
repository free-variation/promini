:- module(test_image, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(image).

test(load_image, [nondet, cleanup(image_unload(Img))]) :-
    image_load('images/pascal.jpg', Img),
    Img = image(_).

test(image_properties, [nondet, cleanup(image_unload(Img))]) :-
    image_load('images/pascal.jpg', Img),
    image_properties(Img, W, H, C),
    integer(W), W > 0,
    integer(H), H > 0,
    integer(C), C > 0.

test(image_unload, [nondet]) :-
    image_load('images/pascal.jpg', Img),
    image_unload(Img).

test(image_write_png, [nondet, setup(make_directory_path('tmp/images')),
                       cleanup((image_unload(Img), delete_file('tmp/images/test_output.png')))]) :-
    image_load('images/pascal.jpg', Img),
    image_write_png(Img, test_output),
    exists_file('tmp/images/test_output.png').

test(image_to_grayscale, [nondet, setup(make_directory_path('tmp/images')),
                          cleanup((image_unload(Img), delete_file('tmp/images/test_gray.png')))]) :-
    image_load('images/pascal.jpg', Img),
    image_properties(Img, _, _, C1),
    C1 > 1,
    image_to_grayscale(Img),
    image_properties(Img, _, _, C2),
    C2 =:= 1,
    image_write_png(Img, test_gray).

test(image_downsample, [nondet, setup(make_directory_path('tmp/images')),
                        cleanup((image_unload(Img), delete_file('tmp/images/test_blocky.png')))]) :-
    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_downsample(Img, 10, 10),
    image_write_png(Img, test_blocky),
    exists_file('tmp/images/test_blocky.png').

test(image_downsample_rgb, [nondet, setup(make_directory_path('tmp/images')),
                            cleanup((image_unload(Img), delete_file('tmp/images/test_blocky_rgb.png')))]) :-
    image_load('images/mauritania.jpg', Img),
    image_properties(Img, _, _, C),
    C > 1,
    image_downsample(Img, 10, 10),
    image_write_png(Img, test_blocky_rgb),
    exists_file('tmp/images/test_blocky_rgb.png').

test(image_quantize, [nondet, setup(make_directory_path('tmp/images')),
                      cleanup((image_unload(Img), delete_file('tmp/images/test_quantized.png')))]) :-
    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_quantize(Img, 2),
    image_write_png(Img, test_quantized),
    exists_file('tmp/images/test_quantized.png').

test(image_quantize_rgb, [nondet, setup(make_directory_path('tmp/images')),
                          cleanup((image_unload(Img), delete_file('tmp/images/test_quantized_rgb.png')))]) :-
    image_load('images/mauritania.jpg', Img),
    image_properties(Img, _, _, C),
    C > 1,
    image_quantize(Img, 2),
    image_write_png(Img, test_quantized_rgb),
    exists_file('tmp/images/test_quantized_rgb.png').

test(image_reset, [nondet, cleanup(image_unload(Img))]) :-
    image_load('images/pascal.jpg', Img),
    image_properties(Img, W, H, _),
    image_downsample(Img, 10, 10),
    image_buffer_properties(Img, BW1, BH1),
    BW1 < W,
    BH1 < H,
    image_reset(Img),
    image_buffer_properties(Img, BW2, BH2),
    BW2 =:= W,
    BH2 =:= H.

test(image_transpose_cw, [nondet, cleanup(image_unload(Img))]) :-
    image_load('images/pascal.jpg', Img),
    image_downsample(Img, 10, 10),
    image_buffer_properties(Img, W1, H1),
    image_transpose(Img, cw),
    image_buffer_properties(Img, W2, H2),
    W2 =:= H1,
    H2 =:= W1.

test(image_transpose_ccw, [nondet, cleanup(image_unload(Img))]) :-
    image_load('images/pascal.jpg', Img),
    image_downsample(Img, 10, 10),
    image_buffer_properties(Img, W1, H1),
    image_transpose(Img, ccw),
    image_buffer_properties(Img, W2, H2),
    W2 =:= H1,
    H2 =:= W1.

test(image_transpose_roundtrip, [nondet, setup(make_directory_path('tmp/images')),
                                 cleanup((image_unload(Img), delete_file('tmp/images/test_transpose.png')))]) :-
    image_load('images/pascal.jpg', Img),
    image_downsample(Img, 10, 10),
    image_buffer_properties(Img, W1, H1),
    image_transpose(Img, cw),
    image_transpose(Img, cw),
    image_transpose(Img, cw),
    image_transpose(Img, cw),
    image_buffer_properties(Img, W2, H2),
    W2 =:= W1,
    H2 =:= H1,
    image_write_png(Img, test_transpose),
    exists_file('tmp/images/test_transpose.png').

test(image_synth_create, [nondet, cleanup((image_synth_unload(Synth), image_unload(Img)))]) :-
    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_downsample(Img, 50, 50),
    image_synth_create(Img, 0, additive, Synth),
    integer(Synth).

test(image_synth_unload, [nondet, cleanup(image_unload(Img))]) :-
    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_downsample(Img, 50, 50),
    image_synth_create(Img, 0, additive, Synth),
    image_synth_unload(Synth).

test(image_synth_get_parameters, [nondet, cleanup((image_synth_unload(Synth), image_unload(Img)))]) :-
    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_downsample(Img, 50, 50),
    image_synth_create(Img, 0, additive, Synth),
    image_synth_get_parameters(Synth, Params),
    memberchk(bpm=120.0, Params),
    memberchk(beats_per_scan=4.0, Params),
    memberchk(looping=true, Params),
    memberchk(scan_position=0.0, Params),
    memberchk(frequencies=Freqs, Params),
    memberchk(phases=Phases, Params),
    is_list(Freqs),
    is_list(Phases).

test(image_synth_set_parameters, [nondet, cleanup((image_synth_unload(Synth), image_unload(Img)))]) :-
    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_downsample(Img, 50, 50),
    image_buffer_properties(Img, _, BufHeight),
    image_synth_create(Img, 0, additive, Synth),
    length(Freqs, BufHeight),
    maplist(=(440.0), Freqs),
    image_synth_set_parameters(Synth, [bpm=90.0, frequencies=Freqs]),
    image_synth_get_parameters(Synth, Params),
    memberchk(bpm=90.0, Params),
    memberchk(frequencies=Freqs, Params).

test(image_synth_start_stop, [nondet, cleanup((image_synth_unload(Synth), image_unload(Img)))]) :-
    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_downsample(Img, 50, 50),
    image_synth_create(Img, 0, additive, Synth),
    image_synth_start(Synth),
    image_synth_stop(Synth).

test(image_synth_waveform_create, [nondet, cleanup((image_synth_unload(Synth), image_unload(Img)))]) :-
    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_downsample(Img, 50, 50),
    image_synth_create(Img, 0, waveform, Synth),
    integer(Synth).

test(image_synth_waveform_get_parameters, [nondet, cleanup((image_synth_unload(Synth), image_unload(Img)))]) :-
    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_downsample(Img, 50, 50),
    image_synth_create(Img, 0, waveform, Synth),
    image_synth_get_parameters(Synth, Params),
    memberchk(frequency=440.0, Params),
    memberchk(amplitude=1.0, Params),
    memberchk(phase=0.0, Params),
    memberchk(row=0, Params).

test(image_synth_waveform_set_parameters, [nondet, cleanup((image_synth_unload(Synth), image_unload(Img)))]) :-
    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_downsample(Img, 50, 50),
    image_synth_create(Img, 0, waveform, Synth),
    image_synth_set_parameters(Synth, [frequency=880.0, amplitude=0.5, row=1]),
    image_synth_get_parameters(Synth, Params),
    memberchk(frequency=880.0, Params),
    memberchk(amplitude=0.5, Params),
    memberchk(row=1, Params).

test(image_synth_waveform_start_stop, [nondet, cleanup((image_synth_unload(Synth), image_unload(Img)))]) :-
    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_downsample(Img, 50, 50),
    image_synth_create(Img, 0, waveform, Synth),
    image_synth_start(Synth),
    image_synth_stop(Synth).

:- end_tests(image).
