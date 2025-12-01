:- use_module('../src/prolog/promini.pro').

/*
 * Envelope Demo - Modulation envelope controlling oscillator frequency
 *
 * Demonstrates:
 * - Creating ADBR envelopes as modulation sources
 * - Routing envelopes to oscillator frequency
 * - Triggering envelopes
 * - Looping vs one-shot envelopes
 */

demo_envelope :-
    format('~n=== Modulation Envelope Demo ===~n~n'),

    % Create a voice with oscillator
    format('Creating voice with 440 Hz sine oscillator...~n'),
    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 440.0, 0.5, Osc),

    % Create short percussive envelope (200ms)
    format('Creating pitch envelope (200ms, one-shot)...~n'),
    mod_envelope_create(0.1, 0.3, 0.3, 0.5, 0.3, 200.0, false, Env),

    % Route envelope to oscillator frequency (pitch sweep up 200 Hz)
    format('Routing envelope to oscillator frequency...~n'),
    mod_route_create(Env, oscillator, Osc, frequency, 200.0, 440.0, 0.0, Route),

    % Start voice and trigger envelope several times
    format('~nPlaying triggered pitch sweeps...~n'),
    synth_voice_start(Voice),

    trigger_notes(Env, 6, 0.4),

    sleep(0.5),

    % Cleanup
    format('~nCleaning up...~n'),
    mod_route_unload(Route),
    mod_source_unload(Env),
    synth_voice_unload(Voice),

    format('Demo complete!~n~n').


demo_looping_envelope :-
    format('~n=== Looping Envelope Demo ===~n~n'),

    % Create a voice with oscillator
    format('Creating voice with 330 Hz sine oscillator...~n'),
    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 330.0, 0.5, Osc),

    % Create looping envelope (300ms cycle)
    format('Creating looping envelope (300ms cycle)...~n'),
    mod_envelope_create(0.2, 0.3, 0.3, 0.7, 0.2, 300.0, true, Env),

    % Route envelope to oscillator frequency (wobble effect)
    mod_route_create(Env, oscillator, Osc, frequency, 50.0, 330.0, 0.0, Route),

    % Start and trigger
    format('Starting voice and triggering envelope...~n'),
    synth_voice_start(Voice),
    mod_envelope_trigger(Env),

    format('Looping pitch wobble for 3 seconds...~n'),
    sleep(3.0),

    % Cleanup
    format('~nCleaning up...~n'),
    mod_route_unload(Route),
    mod_source_unload(Env),
    synth_voice_unload(Voice),

    format('Demo complete!~n~n').


demo_dual_envelope :-
    format('~n=== Dual Envelope Demo ===~n~n'),

    % Create a voice with two oscillators
    format('Creating voice with two oscillators (220 Hz, 330 Hz)...~n'),
    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 220.0, 0.4, Osc1),
    synth_oscillator_add(Voice, 330.0, 0.4, Osc2),

    % Create two envelopes with different timings
    format('Creating two envelopes with different shapes...~n'),
    mod_envelope_create(0.1, 0.2, 0.4, 0.6, 0.3, 400.0, false, Env1),
    mod_envelope_create(0.3, 0.1, 0.3, 0.5, 0.3, 500.0, false, Env2),

    % Route envelopes to each oscillator
    mod_route_create(Env1, oscillator, Osc1, frequency, 100.0, 220.0, 0.0, Route1),
    mod_route_create(Env2, oscillator, Osc2, frequency, 150.0, 330.0, 0.0, Route2),

    % Start voice and trigger both envelopes
    format('~nPlaying dual envelope sweeps...~n'),
    synth_voice_start(Voice),

    trigger_dual(Env1, Env2, 4, 0.6),

    sleep(0.5),

    % Cleanup
    format('~nCleaning up...~n'),
    mod_route_unload(Route1),
    mod_route_unload(Route2),
    mod_source_unload(Env1),
    mod_source_unload(Env2),
    synth_voice_unload(Voice),

    format('Demo complete!~n~n').


demo_tremolo :-
    format('~n=== Tremolo Demo (LFO -> Oscillator Volume) ===~n~n'),

    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 440.0, 0.5, Osc),

    % LFO modulating volume: 0.2 to 0.8 at 5 Hz
    mod_lfo_create(sine, 5.0, Lfo),
    mod_route_create(Lfo, oscillator, Osc, volume, 0.3, 0.5, 0.0, Route),

    format('Playing tremolo for 3 seconds...~n'),
    synth_voice_start(Voice),
    sleep(3.0),

    mod_route_unload(Route),
    mod_source_unload(Lfo),
    synth_voice_unload(Voice),
    format('Demo complete!~n~n').


demo_amp_envelope :-
    format('~n=== Amp Envelope Demo (Envelope -> Oscillator Volume) ===~n~n'),

    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 440.0, 0.0, Osc),

    % Envelope controlling volume (0 to 1)
    mod_envelope_create(0.1, 0.2, 0.4, 0.6, 0.3, 300.0, false, Env),
    mod_route_create(Env, oscillator, Osc, volume, 1.0, 0.0, 0.0, Route),

    format('Playing triggered notes with amp envelope...~n'),
    synth_voice_start(Voice),

    trigger_notes(Env, 6, 0.5),

    sleep(0.5),
    mod_route_unload(Route),
    mod_source_unload(Env),
    synth_voice_unload(Voice),
    format('Demo complete!~n~n').


demo_guitar_tremolo :-
    format('~n=== Guitar Tremolo Demo (LFO -> Sound Volume) ===~n~n'),

    sound_load('audio/guitar.wav', Sound),
    sound_loop(Sound),

    % LFO modulating volume at 4 Hz
    mod_lfo_create(sine, 4.0, Lfo),
    mod_route_create(Lfo, sound, Sound, volume, 0.4, 0.5, 0.0, Route),

    format('Playing guitar with tremolo for 5 seconds...~n'),
    sound_start(Sound),
    sleep(5.0),

    sound_stop(Sound),
    mod_route_unload(Route),
    mod_source_unload(Lfo),
    sound_unload(Sound),
    format('Demo complete!~n~n').


demo_guitar_swell :-
    format('~n=== Guitar Swell Demo (Envelope -> Sound Volume) ===~n~n'),

    sound_load('audio/guitar.wav', Sound),
    sound_loop(Sound),

    % Slow swell envelope (2 seconds)
    mod_envelope_create(0.5, 0.0, 0.3, 0.5, 0.2, 2000.0, true, Env),
    mod_route_create(Env, sound, Sound, volume, 1.0, 0.0, 0.0, Route),

    format('Playing guitar with looping swell for 6 seconds...~n'),
    sound_start(Sound),
    mod_envelope_trigger(Env),
    sleep(6.0),

    sound_stop(Sound),
    mod_route_unload(Route),
    mod_source_unload(Env),
    sound_unload(Sound),
    format('Demo complete!~n~n').


% Helper to trigger envelope N times with delay between
trigger_notes(_, 0, _) :- !.
trigger_notes(Env, N, Delay) :-
    N > 0,
    format('  Trigger ~w~n', [N]),
    mod_envelope_trigger(Env),
    sleep(Delay),
    N1 is N - 1,
    trigger_notes(Env, N1, Delay).

% Helper to trigger two envelopes together
trigger_dual(_, _, 0, _) :- !.
trigger_dual(Env1, Env2, N, Delay) :-
    N > 0,
    format('  Trigger ~w~n', [N]),
    mod_envelope_trigger(Env1),
    mod_envelope_trigger(Env2),
    sleep(Delay),
    N1 is N - 1,
    trigger_dual(Env1, Env2, N1, Delay).


% Run basic demo by default
main :-
    promini_init,
    demo_envelope.

main_looping :-
    promini_init,
    demo_looping_envelope.

main_dual :-
    promini_init,
    demo_dual_envelope.

main_tremolo :-
    promini_init,
    demo_tremolo.

main_amp :-
    promini_init,
    demo_amp_envelope.

main_guitar_tremolo :-
    promini_init,
    demo_guitar_tremolo.

main_guitar_swell :-
    promini_init,
    demo_guitar_swell.


demo_auto_pan :-
    format('~n=== Auto-Pan Demo (LFO -> Pan Effect) ===~n~n'),

    synth_voice_create(Voice),
    synth_oscillator_add(Voice, 440.0, 0.5, _Osc),

    format('Playing without pan for 3 seconds...~n'),
    synth_voice_start(Voice),
    sleep(3.0),

    % Attach pan effect and route LFO to it
    format('Adding pan effect with LFO modulation...~n'),
    voice_attach_effect(Voice, pan, [pan=0.0], effect(_Source, PanPtr)),
    mod_lfo_create(sine, 0.5, Lfo),
    mod_route_create(Lfo, pan, PanPtr, pan, 1.0, 0.0, 0.0, Route),

    format('Playing with auto-pan for 6 seconds...~n'),
    sleep(6.0),

    mod_route_unload(Route),
    mod_source_unload(Lfo),
    synth_voice_unload(Voice),
    format('Demo complete!~n~n').


demo_guitar_pan :-
    format('~n=== Guitar Auto-Pan Demo (LFO -> Pan Effect) ===~n~n'),

    sound_load('audio/guitar.wav', Sound),
    sound_loop(Sound),

    % Attach pan effect and route LFO to it
    sound_attach_effect(Sound, pan, [pan=0.0], effect(_Source, PanPtr)),
    mod_lfo_create(triangle, 0.25, Lfo),
    mod_route_create(Lfo, pan, PanPtr, pan, 1.0, 0.0, 0.0, Route),

    format('Playing guitar with auto-pan for 8 seconds...~n'),
    sound_start(Sound),
    sleep(8.0),

    sound_stop(Sound),
    mod_route_unload(Route),
    mod_source_unload(Lfo),
    sound_unload(Sound),
    format('Demo complete!~n~n').


main_auto_pan :-
    promini_init,
    demo_auto_pan.

main_guitar_pan :-
    promini_init,
    demo_guitar_pan.
