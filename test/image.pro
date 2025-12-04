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

:- end_tests(image).
