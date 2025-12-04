:- use_module('src/prolog/promini.pro').

demo :-
    image_load('images/pascal.jpg', Img),
    image_properties(Img, W, H, C),
    format('Image: images/pascal.jpg~n'),
    format('Width: ~d~n', [W]),
    format('Height: ~d~n', [H]),
    format('Channels: ~d~n', [C]),
    image_unload(Img).

demo_grayscale :-
    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_write_png(Img, pascal_gray),
    format('Wrote tmp/images/pascal_gray.png~n'),
    image_unload(Img).

demo_downsample :-
    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_downsample(Img, 10, 10),
    image_write_png(Img, pascal_blocky),
    format('Wrote tmp/images/pascal_blocky.png~n'),
    image_unload(Img).

demo_downsample_rgb :-
    image_load('images/mauritania.jpg', Img),
    image_downsample(Img, 100, 60),
    image_write_png(Img, mauritania_blocky),
    format('Wrote tmp/images/mauritania_blocky.png~n'),
    image_unload(Img).

demo_quantize :-
    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_quantize(Img, 2),
    image_write_png(Img, pascal_posterized),
    format('Wrote tmp/images/pascal_posterized.png~n'),
    image_unload(Img).

demo_quantize_rgb :-
    image_load('images/mauritania.jpg', Img),
    image_quantize(Img, 2),
    image_write_png(Img, mauritania_posterized),
    format('Wrote tmp/images/mauritania_posterized.png~n'),
    image_unload(Img).

demo_bitcrush :-
    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_downsample(Img, 40, 40),
    image_quantize(Img, 3),
    image_write_png(Img, pascal_bitcrushed),
    format('Wrote tmp/images/pascal_bitcrushed.png~n'),
    image_unload(Img).

demo_bitcrush_rgb :-
    image_load('images/mauritania.jpg', Img),
    image_downsample(Img, 80, 50),
    image_quantize(Img, 3),
    image_write_png(Img, mauritania_bitcrushed),
    format('Wrote tmp/images/mauritania_bitcrushed.png~n'),
    image_unload(Img).
