:- use_module('src/prolog/sampler.pro').

/*
 * Synth Demo - Additive Synthesis Examples
 *
 * Demonstrates the oscillator-based synth API:
 * - Voices as containers for multiple oscillators
 * - Additive synthesis with harmonic partials
 * - Phase manipulation for timbral variation
 * - Dynamic frequency and pan control
 */

% Helper to create oscillator with 1/n amplitude rolloff (sawtooth-like)
add_harmonic(Voice, Fundamental, N, Osc) :-
    Freq is Fundamental * N,
    sampler_synth_oscillator_add(Voice, Freq, 0.0, Osc).

% Helper to set pan on a voice
set_voice_pan(Pan, Voice) :-
    sampler_synth_voice_set_pan(Voice, Pan).

% Helper to set oscillator frequency
set_osc_freq(Freq, Osc) :-
    sampler_synth_oscillator_set_frequency(Osc, Freq).

% Helper to fade out and stop a voice (click-free)
voice_fade_stop(Voice) :-
    sampler_synth_voice_fade(Voice, 0.0, 15),
    sleep(0.02),
    sampler_synth_voice_stop(Voice).

% Helper to fade out and remove an oscillator (click-free)
osc_fade_remove(Osc) :-
    sampler_synth_oscillator_fade(Osc, 0.0, 15),
    sleep(0.02),
    sampler_synth_oscillator_remove(Osc).


/*
 * demo_chord/0
 * Plays an A minor triad using three voices, each with one oscillator.
 */
demo_chord :-
    format('~n=== A Minor Chord Demo ===~n'),
    format('Creating three voices for A minor triad...~n'),

    sampler_synth_voice_create(Voice1),
    sampler_synth_voice_create(Voice2),
    sampler_synth_voice_create(Voice3),

    format('Adding oscillators (A4=440Hz, C5=523Hz, E5=659Hz)...~n'),
    sampler_synth_oscillator_add(Voice1, 440.0, 0.0, _),
    sampler_synth_oscillator_add(Voice2, 523.25, 0.0, _),
    sampler_synth_oscillator_add(Voice3, 659.25, 0.0, _),

    format('Starting voices...~n'),
    sampler_synth_voice_start(Voice1),
    sampler_synth_voice_start(Voice2),
    sampler_synth_voice_start(Voice3),

    format('Playing for 3 seconds...~n'),
    sleep(3),

    format('Stopping and cleaning up...~n'),
    sampler_synth_voice_stop(Voice1),
    sampler_synth_voice_stop(Voice2),
    sampler_synth_voice_stop(Voice3),
    sampler_synth_voice_unload(Voice1),
    sampler_synth_voice_unload(Voice2),
    sampler_synth_voice_unload(Voice3),

    format('Done.~n~n').


/*
 * demo_additive/0
 * Single voice with 8 harmonic oscillators creating a sawtooth-like timbre.
 */
demo_additive :-
    format('~n=== Additive Synthesis Demo ===~n'),
    format('Creating single voice with 8 harmonic partials...~n'),
    format('Fundamental: 110Hz (A2)~n'),

    Fundamental = 110.0,
    sampler_synth_voice_create(Voice),

    format('Adding harmonics 1-8...~n'),
    add_harmonic(Voice, Fundamental, 1, _),
    add_harmonic(Voice, Fundamental, 2, _),
    add_harmonic(Voice, Fundamental, 3, _),
    add_harmonic(Voice, Fundamental, 4, _),
    add_harmonic(Voice, Fundamental, 5, _),
    add_harmonic(Voice, Fundamental, 6, _),
    add_harmonic(Voice, Fundamental, 7, _),
    add_harmonic(Voice, Fundamental, 8, _),

    format('Starting voice (sawtooth-like spectrum)...~n'),
    sampler_synth_voice_start(Voice),

    format('Playing for 4 seconds...~n'),
    sleep(4),

    sampler_synth_voice_stop(Voice),
    sampler_synth_voice_unload(Voice),
    format('Done.~n~n').


/*
 * demo_odd_harmonics/0
 * Single voice with only odd harmonics (square wave-like timbre).
 */
demo_odd_harmonics :-
    format('~n=== Odd Harmonics Demo (Square-like) ===~n'),
    format('Creating voice with odd harmonics only (1, 3, 5, 7)...~n'),
    format('Fundamental: 110Hz (A2)~n'),

    Fundamental = 110.0,
    sampler_synth_voice_create(Voice),

    add_harmonic(Voice, Fundamental, 1, _),
    add_harmonic(Voice, Fundamental, 3, _),
    add_harmonic(Voice, Fundamental, 5, _),
    add_harmonic(Voice, Fundamental, 7, _),

    format('Starting voice...~n'),
    sampler_synth_voice_start(Voice),

    format('Playing for 4 seconds...~n'),
    sleep(4),

    sampler_synth_voice_stop(Voice),
    sampler_synth_voice_unload(Voice),
    format('Done.~n~n').


/*
 * demo_phase_offset/0
 * Demonstrates how phase offsets affect timbre when combining oscillators.
 */
demo_phase_offset :-
    format('~n=== Phase Offset Demo ===~n'),
    format('Comparing in-phase vs phase-offset harmonics...~n'),
    format('Fundamental: 110Hz~n~n'),

    Fundamental = 110.0,

    % First: all in phase
    format('1. All oscillators in phase (0.0)...~n'),
    sampler_synth_voice_create(V1),
    sampler_synth_oscillator_add(V1, Fundamental, 0.0, _),
    F2 is Fundamental * 2,
    sampler_synth_oscillator_add(V1, F2, 0.0, _),
    F3 is Fundamental * 3,
    sampler_synth_oscillator_add(V1, F3, 0.0, _),
    F4 is Fundamental * 4,
    sampler_synth_oscillator_add(V1, F4, 0.0, _),

    sampler_synth_voice_start(V1),
    sleep(2),
    sampler_synth_voice_stop(V1),
    sampler_synth_voice_unload(V1),

    sleep(0.5),

    % Second: staggered phases
    format('2. Staggered phases (0.0, 0.25, 0.5, 0.75)...~n'),
    sampler_synth_voice_create(V2),
    sampler_synth_oscillator_add(V2, Fundamental, 0.0, _),
    sampler_synth_oscillator_add(V2, F2, 0.25, _),
    sampler_synth_oscillator_add(V2, F3, 0.5, _),
    sampler_synth_oscillator_add(V2, F4, 0.75, _),

    sampler_synth_voice_start(V2),
    sleep(2),
    sampler_synth_voice_stop(V2),
    sampler_synth_voice_unload(V2),

    format('Done.~n~n').


/*
 * demo_detuned/0
 * Creates a rich detuned unison sound using multiple slightly-detuned oscillators.
 */
demo_detuned :-
    format('~n=== Detuned Unison Demo ===~n'),
    format('Creating thick detuned sound with 5 oscillators...~n'),
    format('Center frequency: 220Hz, detune: +/- 3Hz~n'),

    sampler_synth_voice_create(Voice),

    % Center oscillator
    sampler_synth_oscillator_add(Voice, 220.0, 0.0, _),
    % Detuned pairs with different phases for movement
    sampler_synth_oscillator_add(Voice, 217.0, 0.1, _),
    sampler_synth_oscillator_add(Voice, 223.0, 0.2, _),
    sampler_synth_oscillator_add(Voice, 218.5, 0.3, _),
    sampler_synth_oscillator_add(Voice, 221.5, 0.4, _),

    format('Starting voice...~n'),
    sampler_synth_voice_start(Voice),

    format('Playing for 5 seconds (notice the beating/movement)...~n'),
    sleep(5),

    sampler_synth_voice_stop(Voice),
    sampler_synth_voice_unload(Voice),
    format('Done.~n~n').


/*
 * demo_frequency_sweep/0
 * Demonstrates dynamic frequency changes by sweeping an oscillator.
 */
demo_frequency_sweep :-
    format('~n=== Frequency Sweep Demo ===~n'),
    format('Sweeping oscillator from 110Hz to 880Hz...~n'),

    sampler_synth_voice_create(Voice),
    sampler_synth_oscillator_add(Voice, 110.0, 0.0, Osc),

    sampler_synth_voice_start(Voice),

    sweep_frequency(Osc, 110.0, 880.0, 100),

    format('Sweeping back down...~n'),
    sweep_frequency(Osc, 880.0, 110.0, 100),

    sampler_synth_voice_stop(Voice),
    sampler_synth_voice_unload(Voice),
    format('Done.~n~n').

sweep_frequency(_, _, _, 0) :- !.
sweep_frequency(Osc, Start, End, Steps) :-
    Steps > 0,
    Remaining is Steps - 1,
    Progress is 1.0 - (Remaining / 100.0),
    Freq is Start + (End - Start) * Progress,
    sampler_synth_oscillator_set_frequency(Osc, Freq),
    sleep(0.03),
    sweep_frequency(Osc, Start, End, Remaining).


/*
 * demo_stereo_harmonics/0
 * Spreads harmonics across the stereo field with animated panning.
 */
demo_stereo_harmonics :-
    format('~n=== Stereo Harmonics Demo ===~n'),
    format('8 harmonics with animated stereo spread...~n'),
    format('Fundamental: 55Hz (A1)~n'),

    Fundamental = 55.0,

    % Create 8 voices, each with one harmonic
    sampler_synth_voice_create(V1),
    sampler_synth_voice_create(V2),
    sampler_synth_voice_create(V3),
    sampler_synth_voice_create(V4),
    sampler_synth_voice_create(V5),
    sampler_synth_voice_create(V6),
    sampler_synth_voice_create(V7),
    sampler_synth_voice_create(V8),

    % Add oscillators to each voice
    sampler_synth_oscillator_add(V1, Fundamental, 0.0, _),
    F2 is Fundamental * 2, sampler_synth_oscillator_add(V2, F2, 0.0, _),
    F3 is Fundamental * 3, sampler_synth_oscillator_add(V3, F3, 0.0, _),
    F4 is Fundamental * 4, sampler_synth_oscillator_add(V4, F4, 0.0, _),
    F5 is Fundamental * 5, sampler_synth_oscillator_add(V5, F5, 0.0, _),
    F6 is Fundamental * 6, sampler_synth_oscillator_add(V6, F6, 0.0, _),
    F7 is Fundamental * 7, sampler_synth_oscillator_add(V7, F7, 0.0, _),
    F8 is Fundamental * 8, sampler_synth_oscillator_add(V8, F8, 0.0, _),

    % Start all voices
    sampler_synth_voice_start(V1),
    sampler_synth_voice_start(V2),
    sampler_synth_voice_start(V3),
    sampler_synth_voice_start(V4),
    sampler_synth_voice_start(V5),
    sampler_synth_voice_start(V6),
    sampler_synth_voice_start(V7),
    sampler_synth_voice_start(V8),

    format('Animating pan positions (lower harmonics left, higher right)...~n'),
    animate_pan([V1,V2,V3,V4], [V5,V6,V7,V8], 0.0, 500),

    % Stop and cleanup
    sampler_synth_voice_stop(V1),
    sampler_synth_voice_stop(V2),
    sampler_synth_voice_stop(V3),
    sampler_synth_voice_stop(V4),
    sampler_synth_voice_stop(V5),
    sampler_synth_voice_stop(V6),
    sampler_synth_voice_stop(V7),
    sampler_synth_voice_stop(V8),

    sampler_synth_voice_unload(V1),
    sampler_synth_voice_unload(V2),
    sampler_synth_voice_unload(V3),
    sampler_synth_voice_unload(V4),
    sampler_synth_voice_unload(V5),
    sampler_synth_voice_unload(V6),
    sampler_synth_voice_unload(V7),
    sampler_synth_voice_unload(V8),

    format('Done.~n~n').

animate_pan(_, _, _, 0) :- !.
animate_pan(GroupA, GroupB, Time, Steps) :-
    Steps > 0,
    Pan is sin(Time) * 0.8,
    PanOpp is -Pan,
    maplist(set_voice_pan(Pan), GroupA),
    maplist(set_voice_pan(PanOpp), GroupB),
    sleep(0.01),
    NextTime is Time + 0.02,
    NextSteps is Steps - 1,
    animate_pan(GroupA, GroupB, NextTime, NextSteps).


/*
 * demo_dynamic_partials/0
 * Demonstrates adding and removing oscillators from a playing voice.
 */
demo_dynamic_partials :-
    format('~n=== Dynamic Partials Demo ===~n'),
    format('Adding and removing harmonics while playing...~n'),

    Fundamental = 110.0,
    sampler_synth_voice_create(Voice),

    format('Starting with fundamental only (110Hz)...~n'),
    sampler_synth_oscillator_add(Voice, Fundamental, 0.0, O1),
    sampler_synth_voice_start(Voice),
    sleep(1),

    format('Adding 2nd harmonic...~n'),
    F2 is Fundamental * 2,
    sampler_synth_oscillator_add(Voice, F2, 0.0, O2),
    sleep(1),

    format('Adding 3rd harmonic...~n'),
    F3 is Fundamental * 3,
    sampler_synth_oscillator_add(Voice, F3, 0.0, O3),
    sleep(1),

    format('Adding 4th harmonic...~n'),
    F4 is Fundamental * 4,
    sampler_synth_oscillator_add(Voice, F4, 0.0, O4),
    sleep(1),

    format('Adding 5th harmonic...~n'),
    F5 is Fundamental * 5,
    sampler_synth_oscillator_add(Voice, F5, 0.0, O5),
    sleep(1),

    format('Removing harmonics one by one...~n'),
    sleep(0.5),
    osc_fade_remove(O5),
    format('  Removed 5th~n'),
    sleep(0.5),
    osc_fade_remove(O4),
    format('  Removed 4th~n'),
    sleep(0.5),
    osc_fade_remove(O3),
    format('  Removed 3rd~n'),
    sleep(0.5),
    osc_fade_remove(O2),
    format('  Removed 2nd~n'),
    sleep(1),

    format('Only fundamental remains...~n'),
    sleep(1),

    osc_fade_remove(O1),
    sampler_synth_voice_stop(Voice),
    sampler_synth_voice_unload(Voice),
    format('Done.~n~n').


/*
 * demo_bell/0
 * Creates a bell-like sound using inharmonic partials.
 */
demo_bell :-
    format('~n=== Bell Sound Demo ===~n'),
    format('Creating bell with inharmonic partials...~n'),
    format('Fundamental: 440Hz~n'),

    Fundamental = 440.0,
    sampler_synth_voice_create(Voice),

    % Bell-like inharmonic ratios (approximating metal modes)
    sampler_synth_oscillator_add(Voice, Fundamental, 0.0, _),
    F2 is Fundamental * 2.0, sampler_synth_oscillator_add(Voice, F2, 0.1, _),
    F3 is Fundamental * 2.4, sampler_synth_oscillator_add(Voice, F3, 0.2, _),
    F4 is Fundamental * 3.0, sampler_synth_oscillator_add(Voice, F4, 0.3, _),
    F5 is Fundamental * 4.5, sampler_synth_oscillator_add(Voice, F5, 0.4, _),
    F6 is Fundamental * 5.2, sampler_synth_oscillator_add(Voice, F6, 0.5, _),

    format('Playing bell tone for 4 seconds...~n'),
    sampler_synth_voice_start(Voice),
    sleep(4),

    sampler_synth_voice_stop(Voice),
    sampler_synth_voice_unload(Voice),
    format('Done.~n~n').


/*
 * demo_chord_progression/0
 * Plays a simple chord progression: Am - F - C - G
 */
demo_chord_progression :-
    format('~n=== Chord Progression Demo ===~n'),
    format('Playing: Am - F - C - G~n~n'),

    % Create all voices upfront
    sampler_synth_voice_create(V1),
    sampler_synth_voice_create(V2),
    sampler_synth_voice_create(V3),
    sampler_synth_voice_create(V4),

    % A minor: A3, C4, E4
    sampler_synth_oscillator_add(V1, 220.0, 0.0, _),
    sampler_synth_oscillator_add(V1, 261.63, 0.0, _),
    sampler_synth_oscillator_add(V1, 329.63, 0.0, _),

    % F major: F3, A3, C4
    sampler_synth_oscillator_add(V2, 174.61, 0.0, _),
    sampler_synth_oscillator_add(V2, 220.0, 0.0, _),
    sampler_synth_oscillator_add(V2, 261.63, 0.0, _),

    % C major: C3, E3, G3
    sampler_synth_oscillator_add(V3, 130.81, 0.0, _),
    sampler_synth_oscillator_add(V3, 164.81, 0.0, _),
    sampler_synth_oscillator_add(V3, 196.0, 0.0, _),

    % G major: G3, B3, D4
    sampler_synth_oscillator_add(V4, 196.0, 0.0, _),
    sampler_synth_oscillator_add(V4, 246.94, 0.0, _),
    sampler_synth_oscillator_add(V4, 293.66, 0.0, _),

    % Play progression
    format('Am...~n'),
    sampler_synth_voice_start(V1),
    sleep(1.5),
    voice_fade_stop(V1),

    format('F...~n'),
    sampler_synth_voice_start(V2),
    sleep(1.5),
    voice_fade_stop(V2),

    format('C...~n'),
    sampler_synth_voice_start(V3),
    sleep(1.5),
    voice_fade_stop(V3),

    format('G...~n'),
    sampler_synth_voice_start(V4),
    sleep(1.5),
    voice_fade_stop(V4),

    % Cleanup at end
    sampler_synth_voice_unload(V1),
    sampler_synth_voice_unload(V2),
    sampler_synth_voice_unload(V3),
    sampler_synth_voice_unload(V4),

    format('Done.~n~n').


/*
 * demo_voice_reverb/0
 * Demonstrates reverb effect on a synth voice.
 */
demo_voice_reverb :-
    format('~n=== Voice Reverb Demo ===~n'),
    format('Adding reverb to a chord...~n'),

    sampler_synth_voice_create(Voice),
    sampler_synth_oscillator_add(Voice, 220.0, 0.0, _),
    sampler_synth_oscillator_add(Voice, 277.18, 0.0, _),
    sampler_synth_oscillator_add(Voice, 329.63, 0.0, _),

    sampler_voice_attach_effect(Voice, reverb, [decay=0.85, wet=0.4, predelay_ms=20], _),

    format('Playing A minor chord with reverb...~n'),
    sampler_synth_voice_start(Voice),
    sleep(3),

    voice_fade_stop(Voice),
    sampler_synth_voice_unload(Voice),
    format('Done.~n~n').


/*
 * demo_voice_delay/0
 * Demonstrates ping-pong delay on a synth voice.
 */
demo_voice_delay :-
    format('~n=== Voice Ping-Pong Delay Demo ===~n'),
    format('Adding stereo delay to an arpeggio...~n'),

    sampler_synth_voice_create(Voice),
    sampler_synth_oscillator_add(Voice, 440.0, 0.0, O1),
    sampler_voice_attach_effect(Voice, ping_pong_delay,
        [max_delay_in_frames=24000, delay_in_frames=12000, feedback=0.5, wet=0.6], _),

    format('Playing arpeggio with ping-pong delay...~n'),
    sampler_synth_voice_start(Voice),
    sleep(0.3),
    osc_fade_remove(O1),

    sampler_synth_oscillator_add(Voice, 554.37, 0.0, O2),
    sleep(0.3),
    osc_fade_remove(O2),

    sampler_synth_oscillator_add(Voice, 659.25, 0.0, O3),
    sleep(0.3),
    osc_fade_remove(O3),

    sampler_synth_oscillator_add(Voice, 554.37, 0.0, O4),
    sleep(0.3),
    osc_fade_remove(O4),

    format('Letting delay tail ring out...~n'),
    sleep(2),

    sampler_synth_voice_stop(Voice),
    sampler_synth_voice_unload(Voice),
    format('Done.~n~n').


/*
 * demo_voice_filter/0
 * Demonstrates filter sweep on a synth voice.
 */
demo_voice_filter :-
    format('~n=== Voice Filter Sweep Demo ===~n'),
    format('Sweeping LPF cutoff on a sawtooth-like tone...~n'),

    sampler_synth_voice_create(Voice),

    % Build sawtooth-like with harmonics
    Fundamental = 110.0,
    sampler_synth_oscillator_add(Voice, Fundamental, 0.0, _),
    F2 is Fundamental * 2, sampler_synth_oscillator_add(Voice, F2, 0.0, _),
    F3 is Fundamental * 3, sampler_synth_oscillator_add(Voice, F3, 0.0, _),
    F4 is Fundamental * 4, sampler_synth_oscillator_add(Voice, F4, 0.0, _),
    F5 is Fundamental * 5, sampler_synth_oscillator_add(Voice, F5, 0.0, _),
    F6 is Fundamental * 6, sampler_synth_oscillator_add(Voice, F6, 0.0, _),

    sampler_voice_attach_effect(Voice, lpf, [cutoff=200, order=4], Effect),

    format('Playing with filter sweep (200Hz -> 4000Hz -> 200Hz)...~n'),
    sampler_synth_voice_start(Voice),

    % Sweep up (logarithmic)
    forall(between(0, 79, I), (
        T is I / 79.0,
        Cutoff is 200 * exp(T * log(4000/200)),
        sampler_effect_set_parameters(Effect, [cutoff=Cutoff]),
        sleep(0.05)
    )),

    % Sweep down (logarithmic)
    forall(between(0, 79, I), (
        T is I / 79.0,
        Cutoff is 4000 * exp(T * log(200/4000)),
        sampler_effect_set_parameters(Effect, [cutoff=Cutoff]),
        sleep(0.05)
    )),

    voice_fade_stop(Voice),
    sampler_synth_voice_unload(Voice),
    format('Done.~n~n').


/*
 * demo_voice_bitcrush/0
 * Demonstrates bitcrush effect on a synth voice.
 */
demo_voice_bitcrush :-
    format('~n=== Voice Bitcrush Demo ===~n'),
    format('Lo-fi degradation on a chord...~n'),

    sampler_synth_voice_create(Voice),
    sampler_synth_oscillator_add(Voice, 261.63, 0.0, _),
    sampler_synth_oscillator_add(Voice, 329.63, 0.0, _),
    sampler_synth_oscillator_add(Voice, 392.0, 0.0, _),

    format('Clean chord...~n'),
    sampler_synth_voice_start(Voice),
    sleep(1.5),

    format('Adding 8-bit crush...~n'),
    sampler_voice_attach_effect(Voice, bitcrush, [bits=8, sample_rate=11025], _),
    sleep(1.5),

    format('More extreme: 4-bit...~n'),
    sampler_synth_voice_stop(Voice),
    sampler_synth_voice_create(Voice2),
    sampler_synth_oscillator_add(Voice2, 261.63, 0.0, _),
    sampler_synth_oscillator_add(Voice2, 329.63, 0.0, _),
    sampler_synth_oscillator_add(Voice2, 392.0, 0.0, _),
    sampler_voice_attach_effect(Voice2, bitcrush, [bits=4, sample_rate=8000], _),
    sampler_synth_voice_start(Voice2),
    sleep(1.5),

    sampler_synth_voice_stop(Voice2),
    sampler_synth_voice_unload(Voice),
    sampler_synth_voice_unload(Voice2),
    format('Done.~n~n').


/*
 * demo_noise/0
 * Demonstrates the three noise types: white, pink, and brownian.
 */
demo_noise :-
    format('~n=== Noise Generator Demo ===~n'),
    format('Demonstrating white, pink, and brownian noise...~n~n'),

    sampler_synth_voice_create(Voice),

    format('White noise (flat spectrum, harsh)...~n'),
    sampler_synth_noise_add(Voice, white, N1),
    sampler_synth_oscillator_set_volume(N1, 0.3),
    sampler_synth_voice_start(Voice),
    sleep(2),
    sampler_synth_voice_stop(Voice),
    sampler_synth_oscillator_remove(N1),
    sleep(0.3),

    format('Pink noise (1/f spectrum, natural)...~n'),
    sampler_synth_noise_add(Voice, pink, N2),
    sampler_synth_oscillator_set_volume(N2, 0.3),
    sampler_synth_voice_start(Voice),
    sleep(2),
    sampler_synth_voice_stop(Voice),
    sampler_synth_oscillator_remove(N2),
    sleep(0.3),

    format('Brownian noise (1/f^2 spectrum, rumble)...~n'),
    sampler_synth_noise_add(Voice, brownian, N3),
    sampler_synth_oscillator_set_volume(N3, 0.5),
    sampler_synth_voice_start(Voice),
    sleep(2),
    sampler_synth_voice_stop(Voice),

    sampler_synth_voice_unload(Voice),
    format('Done.~n~n').


/*
 * demo_noise_mix/0
 * Demonstrates mixing noise with oscillators.
 */
demo_noise_mix :-
    format('~n=== Noise + Oscillator Mix Demo ===~n'),
    format('Mixing pink noise with a sine wave...~n'),

    sampler_synth_voice_create(Voice),

    % Add a low sine wave
    sampler_synth_oscillator_add(Voice, 110.0, 0.0, Osc),
    sampler_synth_oscillator_set_volume(Osc, 0.6),

    % Add pink noise at lower volume
    sampler_synth_noise_add(Voice, pink, Noise),
    sampler_synth_oscillator_set_volume(Noise, 0.15),

    format('Playing mixed tone...~n'),
    sampler_synth_voice_start(Voice),
    sleep(3),

    format('Fading out noise...~n'),
    sampler_synth_oscillator_fade(Noise, 0.0, 1000),
    sleep(1.5),

    format('Pure sine remains...~n'),
    sleep(1.5),

    voice_fade_stop(Voice),
    sampler_synth_voice_unload(Voice),
    format('Done.~n~n').


/*
 * demo_wind/0
 * Creates a wind-like effect using filtered brownian noise.
 */
demo_wind :-
    format('~n=== Wind Effect Demo ===~n'),
    format('Creating wind using filtered brownian noise...~n'),

    sampler_synth_voice_create(Voice),
    sampler_synth_noise_add(Voice, brownian, Noise),
    sampler_synth_oscillator_set_volume(Noise, 0.8),

    % Add a bandpass filter to shape the wind
    sampler_voice_attach_effect(Voice, bpf, [cutoff=400.0, order=4], _),

    format('Playing wind effect...~n'),
    sampler_synth_voice_start(Voice),
    sleep(4),

    voice_fade_stop(Voice),
    sampler_synth_voice_unload(Voice),
    format('Done.~n~n').


/*
 * demo_additive_proper/0
 * Demonstrates proper additive synthesis with 1/n amplitude rolloff.
 */
demo_additive_proper :-
    format('~n=== Proper Additive Synthesis Demo ===~n'),
    format('Building sawtooth with 1/n amplitude rolloff...~n'),
    format('Fundamental: 110Hz (A2)~n'),

    Fundamental = 110.0,
    sampler_synth_voice_create(Voice),

    format('Adding harmonics with proper amplitudes...~n'),
    add_harmonic_proper(Voice, Fundamental, 1, _),
    add_harmonic_proper(Voice, Fundamental, 2, _),
    add_harmonic_proper(Voice, Fundamental, 3, _),
    add_harmonic_proper(Voice, Fundamental, 4, _),
    add_harmonic_proper(Voice, Fundamental, 5, _),
    add_harmonic_proper(Voice, Fundamental, 6, _),
    add_harmonic_proper(Voice, Fundamental, 7, _),
    add_harmonic_proper(Voice, Fundamental, 8, _),

    format('Playing sawtooth-like tone...~n'),
    sampler_synth_voice_start(Voice),
    sleep(4),

    sampler_synth_voice_stop(Voice),
    sampler_synth_voice_unload(Voice),
    format('Done.~n~n').

% Helper: add harmonic with 1/n amplitude
add_harmonic_proper(Voice, Fundamental, N, Osc) :-
    Freq is Fundamental * N,
    Amp is 1.0 / N,
    sampler_synth_oscillator_add(Voice, Freq, 0.0, Osc),
    sampler_synth_oscillator_set_volume(Osc, Amp).


/*
 * demo_all/0
 * Runs all demos in sequence.
 */
demo_all :-
    format('~n========================================~n'),
    format('     Synth Demo - All Examples~n'),
    format('========================================~n'),

    demo_chord,
    demo_additive,
    demo_odd_harmonics,
    demo_phase_offset,
    demo_detuned,
    demo_frequency_sweep,
    demo_stereo_harmonics,
    demo_dynamic_partials,
    demo_bell,
    demo_chord_progression,
    demo_voice_reverb,
    demo_voice_delay,
    demo_voice_filter,
    demo_voice_bitcrush,
    demo_noise,
    demo_noise_mix,
    demo_wind,
    demo_additive_proper,

    format('========================================~n'),
    format('     All demos complete!~n'),
    format('========================================~n~n').
