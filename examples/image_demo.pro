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

/*
 * demo_image_synth/0
 * Demonstrates image-to-sound synthesis.
 * Scans an image horizontally, mapping rows to oscillator frequencies
 * and pixel brightness to volume.
 */
demo_image_synth :-
    format('~n=== Image Synth Demo ===~n'),
    format('Loading and preparing image...~n'),

    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_downsample(Img, 50, 50),
    image_buffer_properties(Img, BufW, BufH),
    format('Buffer: ~dx~d pixels~n', [BufW, BufH]),

    format('Creating image synth (channel 0, additive mode)...~n'),
    image_synth_create(Img, 0, additive, Synth),

    % Set up frequencies and amplitudes: harmonic series with 1/n² rolloff
    format('Setting frequencies (harmonic series from 55Hz) and amplitudes (1/n^2 rolloff)...~n'),
    Fundamental = 110.0,
    setup_harmonic_frequencies(BufH, Fundamental, Freqs),
    setup_harmonic_amplitudes(Freqs, Fundamental, Amps),
    image_synth_set_parameters(Synth, [bpm=120.0, beats_per_scan=8.0, frequencies=Freqs, amplitudes=Amps]),

    format('Playing image synth for 10 seconds...~n'),
    image_synth_start(Synth),
    sleep(10),
    image_synth_stop(Synth),

    format('Cleaning up...~n'),
    image_synth_unload(Synth),
    image_unload(Img),
    format('Done.~n~n').

/*
 * setup_harmonic_frequencies/3
 * Creates a list of frequencies for BufH rows as harmonics of a fundamental.
 * Row 0 = fundamental, row 1 = 2*fundamental, etc.
 */
setup_harmonic_frequencies(BufH, Fundamental, Freqs) :-
    length(Freqs, BufH),
    setup_harmonic_frequencies_helper(1, BufH, Fundamental, Freqs).

setup_harmonic_frequencies_helper(N, BufH, _, []) :- N > BufH, !.
setup_harmonic_frequencies_helper(N, BufH, Fundamental, [Freq|Rest]) :-
    N =< BufH,
    Freq is Fundamental * N,
    N1 is N + 1,
    setup_harmonic_frequencies_helper(N1, BufH, Fundamental, Rest).

/*
 * setup_harmonic_amplitudes/3
 * Creates amplitude list with 1/n² rolloff based on frequency ratios.
 */
setup_harmonic_amplitudes(Freqs, Fundamental, Amps) :-
    maplist(freq_to_amp(Fundamental), Freqs, Amps).

freq_to_amp(Fundamental, Freq, Amp) :-
    N is Freq / Fundamental,
    %Amp is 1.0 / (N * N).
    Amp is 1.0 / N^0.5.

/*
 * setup_odd_harmonics/4
 * Creates frequencies using odd harmonics only (1, 3, 5, 7, ...).
 * Produces a square-wave-like timbre.
 */
setup_odd_harmonics(BufH, Fundamental, Freqs, Amps) :-
    length(Freqs, BufH),
    length(Amps, BufH),
    setup_odd_harmonics_helper(1, BufH, Fundamental, Freqs, Amps).

setup_odd_harmonics_helper(_, 0, _, [], []) :- !.
setup_odd_harmonics_helper(N, Remaining, Fundamental, [Freq|Freqs], [Amp|Amps]) :-
    Remaining > 0,
    Freq is Fundamental * N,
    Amp is 1.0 / N^0.5,
    N1 is N + 2,
    Remaining1 is Remaining - 1,
    setup_odd_harmonics_helper(N1, Remaining1, Fundamental, Freqs, Amps).

/*
 * setup_even_harmonics/4
 * Creates frequencies using even harmonics only (2, 4, 6, 8, ...).
 */
setup_even_harmonics(BufH, Fundamental, Freqs, Amps) :-
    length(Freqs, BufH),
    length(Amps, BufH),
    setup_even_harmonics_helper(2, BufH, Fundamental, Freqs, Amps).

setup_even_harmonics_helper(_, 0, _, [], []) :- !.
setup_even_harmonics_helper(N, Remaining, Fundamental, [Freq|Freqs], [Amp|Amps]) :-
    Remaining > 0,
    Freq is Fundamental * N,
    Amp is 1.0 / N^0.5,
    N1 is N + 2,
    Remaining1 is Remaining - 1,
    setup_even_harmonics_helper(N1, Remaining1, Fundamental, Freqs, Amps).

/*
 * setup_third_harmonics/4
 * Creates frequencies using every third harmonic (3, 6, 9, 12, ...).
 */
setup_third_harmonics(BufH, Fundamental, Freqs, Amps) :-
    length(Freqs, BufH),
    length(Amps, BufH),
    setup_third_harmonics_helper(3, BufH, Fundamental, Freqs, Amps).

setup_third_harmonics_helper(_, 0, _, [], []) :- !.
setup_third_harmonics_helper(N, Remaining, Fundamental, [Freq|Freqs], [Amp|Amps]) :-
    Remaining > 0,
    Freq is Fundamental * N,
    Amp is 1.0 / N^0.5,
    N1 is N + 3,
    Remaining1 is Remaining - 1,
    setup_third_harmonics_helper(N1, Remaining1, Fundamental, Freqs, Amps).

/*
 * demo_image_synth_slow/0
 * Slower scan with looping disabled - plays through image once.
 */
demo_image_synth_slow :-
    format('~n=== Image Synth Slow Scan Demo ===~n'),

    image_load('images/mauritania.jpg', Img),
    image_to_grayscale(Img),
    image_downsample(Img, 50, 50),
    image_buffer_properties(Img, BufW, BufH),
    format('Buffer: ~dx~d pixels~n', [BufW, BufH]),

    image_synth_create(Img, 0, additive, Synth),

    Fundamental = 110.0,
    setup_harmonic_frequencies(BufH, Fundamental, Freqs),
    setup_harmonic_amplitudes(Freqs, Fundamental, Amps),
    image_synth_set_parameters(Synth, [
        bpm=60.0,
        beats_per_scan=16.0,
        looping=false,
        frequencies=Freqs,
        amplitudes=Amps
    ]),

    format('Playing slow scan (no loop)...~n'),
    image_synth_start(Synth),
    sleep(16),
    image_synth_stop(Synth),

    image_synth_unload(Synth),
    image_unload(Img),
    format('Done.~n~n').

/*
 * demo_image_synth_waveform/0
 * Demonstrates waveform mode: uses an image row as a wavetable oscillator.
 */
demo_image_synth_waveform :-
    format('~n=== Image Synth Waveform Demo ===~n'),
    format('Loading and preparing image...~n'),

    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_downsample(Img, 10, 10),
    image_buffer_properties(Img, BufW, BufH),
    format('Buffer: ~dx~d pixels (width = wavetable length)~n', [BufW, BufH]),

    format('Creating waveform synth (row 0 as wavetable)...~n'),
    image_synth_create(Img, 0, waveform, Synth),
    image_synth_set_parameters(Synth, [frequency=220.0, amplitude=0.5]),

    format('Playing A3 (220Hz) for 3 seconds...~n'),
    image_synth_start(Synth),
    sleep(3),

    MidRow is BufH // 2,
    format('Changing to row ~d...~n', [MidRow]),
    image_synth_set_parameters(Synth, [row=MidRow]),
    sleep(3),

    format('Sweeping through rows...~n'),
    sweep_rows(Synth, 0, BufH),

    image_synth_stop(Synth),
    image_synth_unload(Synth),
    image_unload(Img),
    format('Done.~n~n').

sweep_rows(_, Row, BufH) :- Row >= BufH, !.
sweep_rows(Synth, Row, BufH) :-
    image_synth_set_parameters(Synth, [row=Row]),
    sleep(0.1),
    Row1 is Row + 1,
    sweep_rows(Synth, Row1, BufH).

/*
 * demo_image_synth_waveform_transpose/0
 * Demonstrates waveform mode with transposed image.
 * Transposing swaps rows/columns, so the wavetable becomes the image height.
 */
demo_image_synth_waveform_transpose :-
    format('~n=== Image Synth Waveform Transpose Demo ===~n'),

    image_load('images/pascal.jpg', Img),
    image_to_grayscale(Img),
    image_downsample(Img, 10, 10),
    image_buffer_properties(Img, BufW1, BufH1),
    format('Original buffer: ~dx~d~n', [BufW1, BufH1]),

    image_transpose(Img, cw),
    image_buffer_properties(Img, BufW2, BufH2),
    format('Transposed buffer: ~dx~d (longer wavetable)~n', [BufW2, BufH2]),

    image_synth_create(Img, 0, waveform, Synth),
    image_synth_set_parameters(Synth, [frequency=110.0, amplitude=0.5]),

    format('Playing with transposed wavetable for 5 seconds...~n'),
    image_synth_start(Synth),
    sleep(5),

    image_synth_stop(Synth),
    image_synth_unload(Synth),
    image_unload(Img),
    format('Done.~n~n').

/*
 * demo_image_synth_rgb_stereo/0
 * Demonstrates RGB stereo: red channel panned left, green center, blue right.
 */
demo_image_synth_rgb_stereo :-
    format('~n=== Image Synth RGB Stereo Demo ===~n'),
    format('Loading color image...~n'),

    image_load('images/mauritania.jpg', Img),
    image_properties(Img, _, _, Channels),
    (Channels < 3 ->
        format('Error: image must have at least 3 channels~n'),
        image_unload(Img),
        fail
    ; true),
    image_downsample(Img, 10, 10),
    image_buffer_properties(Img, BufW, BufH),
    format('Buffer: ~dx~d pixels, ~d channels~n', [BufW, BufH, Channels]),

    format('Creating 3 waveform synths (R, G, B channels)...~n'),
    image_synth_create(Img, 0, waveform, SynthR),
    image_synth_create(Img, 1, waveform, SynthG),
    image_synth_create(Img, 2, waveform, SynthB),

    format('Attaching pan effects with LFO auto-pan on R and B...~n'),
    image_synth_attach_effect(SynthR, pan, [pan=(-1.0)], effect(_, PanPtrR)),
    image_synth_attach_effect(SynthG, pan, [pan=0.0], _),
    image_synth_attach_effect(SynthB, pan, [pan=1.0], effect(_, PanPtrB)),

    format('Creating LFOs for auto-pan (R: 0.2 Hz, B: 0.15 Hz opposite phase)...~n'),
    mod_lfo_create(sine, 0.2, LfoPanR),
    mod_lfo_create(sine, 0.15, LfoPanB),
    mod_route_create(LfoPanR, pan, PanPtrR, pan, 1.0, 0.0, 0.0, RoutePanR),
    mod_route_create(LfoPanB, pan, PanPtrB, pan, -1.0, 0.0, 0.0, RoutePanB),

    format('Creating summing node with Moog filter and blackhole reverb...~n'),
    summing_node_create(Bus),
    summing_node_connect(Bus, image_synth(SynthR)),
    summing_node_connect(Bus, image_synth(SynthG)),
    summing_node_connect(Bus, image_synth(SynthB)),
    summing_node_attach_effect(Bus, moog, [cutoff=2000.0, resonance=1.5], effect(_, MoogPtr)),
    summing_node_attach_effect(Bus, reverb, [
        predelay_ms=100.0, decay=0.99, damping=0.1, bandwidth=0.5,
        shimmer1_shift=12.0, shimmer1_mix=0.15,
        shimmer2_shift=19.0, shimmer2_mix=0.1,
        mod_depth=1.0, width=2.0, low_cut=150.0, wet=0.6
    ], _),

    format('Creating LFO for filter cutoff modulation (0.1 Hz)...~n'),
    mod_lfo_create(sine, 0.1, LfoMoog),
    mod_route_create(LfoMoog, moog, MoogPtr, cutoff, 1500.0, 2000.0, 0.0, RouteMoog),

    format('Setting frequencies: R=220Hz, G=277Hz, B=330Hz (A minor triad)~n'),
    image_synth_set_parameters(SynthR, [frequency=220.0, amplitude=0.8]),
    image_synth_set_parameters(SynthG, [frequency=277.18, amplitude=0.8]),
    image_synth_set_parameters(SynthB, [frequency=329.63, amplitude=0.8]),

    format('Playing RGB stereo for 5 seconds...~n'),
    image_synth_start(SynthR),
    image_synth_start(SynthG),
    image_synth_start(SynthB),
    sleep(5),

    format('Sweeping through rows...~n'),
    sweep_rows_rgb(SynthR, SynthG, SynthB, 0, BufH),

    image_synth_stop(SynthR),
    image_synth_stop(SynthG),
    image_synth_stop(SynthB),
    mod_route_unload(RoutePanR),
    mod_route_unload(RoutePanB),
    mod_route_unload(RouteMoog),
    mod_source_unload(LfoPanR),
    mod_source_unload(LfoPanB),
    mod_source_unload(LfoMoog),
    summing_node_unload(Bus),
    image_synth_unload(SynthR),
    image_synth_unload(SynthG),
    image_synth_unload(SynthB),
    image_unload(Img),
    format('Done.~n~n').

sweep_rows_rgb(_, _, _, Row, BufH) :- Row >= BufH, !.
sweep_rows_rgb(SynthR, SynthG, SynthB, Row, BufH) :-
    image_synth_set_parameters(SynthR, [row=Row]),
    image_synth_set_parameters(SynthG, [row=Row]),
    image_synth_set_parameters(SynthB, [row=Row]),
    sleep(0.15),
    Row1 is Row + 1,
    sweep_rows_rgb(SynthR, SynthG, SynthB, Row1, BufH).

/*
 * demo_image_synth_rgb_additive/0
 * Demonstrates RGB stereo with additive synthesis.
 * Each channel scans the image, panned L/C/R.
 */
demo_image_synth_rgb_additive :-
    format('~n=== Image Synth RGB Additive Demo ===~n'),
    format('Loading color image...~n'),

    image_load('images/mauritania.jpg', Img),
    image_properties(Img, _, _, Channels),
    (Channels < 3 ->
        format('Error: image must have at least 3 channels~n'),
        image_unload(Img),
        fail
    ; true),
    image_downsample(Img, 50, 50),
    image_buffer_properties(Img, BufW, BufH),
    format('Buffer: ~dx~d pixels, ~d channels~n', [BufW, BufH, Channels]),

    format('Creating 3 additive synths (R, G, B channels)...~n'),
    image_synth_create(Img, 0, additive, SynthR),
    image_synth_create(Img, 1, additive, SynthG),
    image_synth_create(Img, 2, additive, SynthB),

    format('Attaching pan effects with LFO auto-pan on R and B...~n'),
    image_synth_attach_effect(SynthR, pan, [pan=(-1.0)], effect(_, PanPtrR)),
    image_synth_attach_effect(SynthG, pan, [pan=0.0], _),
    image_synth_attach_effect(SynthB, pan, [pan=1.0], effect(_, PanPtrB)),

    format('Creating LFOs for auto-pan (R: 0.2 Hz, B: 0.15 Hz opposite phase)...~n'),
    mod_lfo_create(sine, 0.2, LfoPanR),
    mod_lfo_create(sine, 0.15, LfoPanB),
    mod_route_create(LfoPanR, pan, PanPtrR, pan, 1.0, 0.0, 0.0, RoutePanR),
    mod_route_create(LfoPanB, pan, PanPtrB, pan, -1.0, 0.0, 0.0, RoutePanB),

    format('Creating summing node with Moog filter and reverb...~n'),
    summing_node_create(Bus),
    summing_node_connect(Bus, image_synth(SynthR)),
    summing_node_connect(Bus, image_synth(SynthG)),
    summing_node_connect(Bus, image_synth(SynthB)),
    summing_node_attach_effect(Bus, moog, [cutoff=2000.0, resonance=1.5], effect(_, MoogPtr)),
    summing_node_attach_effect(Bus, reverb, [
        predelay_ms=100.0, decay=0.99, damping=0.1, bandwidth=0.5,
        shimmer1_shift=12.0, shimmer1_mix=0.4,
        shimmer2_shift=19.0, shimmer2_mix=0.3,
        mod_depth=1.0, width=2.0, low_cut=150.0, wet=0.6
    ], _),

    format('Creating LFO for filter cutoff modulation (0.1 Hz)...~n'),
    mod_lfo_create(sine, 0.1, LfoMoog),
    mod_route_create(LfoMoog, moog, MoogPtr, cutoff, 1500.0, 2000.0, 0.0, RouteMoog),

    format('Setting up separated harmonic frequencies (odd/even/thirds)...~n'),
    Fundamental = 110.0,
    setup_odd_harmonics(BufH, Fundamental, FreqsR, AmpsR),
    setup_even_harmonics(BufH, Fundamental, FreqsG, AmpsG),
    setup_third_harmonics(BufH, Fundamental, FreqsB, AmpsB),
    image_synth_set_parameters(SynthR, [bpm=60.0, beats_per_scan=8.0, frequencies=FreqsR, amplitudes=AmpsR]),
    image_synth_set_parameters(SynthG, [bpm=60.0, beats_per_scan=8.0, frequencies=FreqsG, amplitudes=AmpsG]),
    image_synth_set_parameters(SynthB, [bpm=60.0, beats_per_scan=8.0, frequencies=FreqsB, amplitudes=AmpsB]),

    format('Playing RGB additive stereo for 16 seconds...~n'),
    image_synth_start(SynthR),
    image_synth_start(SynthG),
    image_synth_start(SynthB),
    sleep(16),

    image_synth_stop(SynthR),
    image_synth_stop(SynthG),
    image_synth_stop(SynthB),
    mod_route_unload(RoutePanR),
    mod_route_unload(RoutePanB),
    mod_route_unload(RouteMoog),
    mod_source_unload(LfoPanR),
    mod_source_unload(LfoPanB),
    mod_source_unload(LfoMoog),
    summing_node_unload(Bus),
    image_synth_unload(SynthR),
    image_synth_unload(SynthG),
    image_synth_unload(SynthB),
    image_unload(Img),
    format('Done.~n~n').
