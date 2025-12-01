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
