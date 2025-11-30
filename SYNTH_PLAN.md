# Synth System Implementation Plan

## Overall Vision

Live granular sampler + additive synth + effects + modulation, controlled from Prolog.

### Architecture Layers
- **C layer (miniaudio)**: Audio primitives - playback, effects, modulation sources, capture
- **Prolog layer**: High-level control - grain scheduling, sequencing, patch management

### Progress (~60% complete)

| Component | Status |
|-----------|--------|
| Audio playback (sounds, buffers, regions) | ✓ Complete |
| Additive synth (voices, oscillators, noise) | ✓ Complete |
| Effects (filters, reverb, delay, bitcrush, envelope) | ✓ Complete |
| Live capture | ✓ Complete |
| Modulation (LFO → osc frequency) | ✓ Phase 1 complete |
| Modulation (remaining targets/sources) | In progress |
| Granular engine | Planned (Prolog layer) |

---

## Modulation System Status

**Phase 1 COMPLETE**: LFO → oscillator frequency modulation working.

### Completed
- [x] Type definitions in `sampler_internal.h`
- [x] `src/c/mod.c` with source pool, route table, processing
- [x] Audio callback hook (`engine_audio_callback` in sampler.c)
- [x] LFO source (create, set_frequency, get_frequency)
- [x] Route creation with target type/param lookup
- [x] Oscillator frequency setter
- [x] Source and route unload
- [x] Registration in init.c
- [x] Exports in sampler.pro
- [x] Unit tests (test/mod.pro)
- [x] Demo (examples/mod_demo.pro)

### Remaining
- [ ] Additional setters: oscillator volume, voice pan
- [ ] Noise modulation source
- [ ] Sampler (audio buffer) modulation source
- [ ] ADBR envelope modulation source (loopable, replaces envelope effect)
- [ ] Remove envelope effect code (effects.c, sampler.pro, tests)
- [ ] Sample & Hold predicate
- [ ] Effect parameter setters (filter cutoff, reverb, delay)
- [ ] Route parameter setters (depth, offset, slew as modulation targets)
- [ ] Sound parameter setters (pitch, volume, pan)
- [ ] Crossfader mixer node (position as modulation target)

## Architecture

### Source Types
- **LFO**: `ma_waveform` (sine, square, triangle, sawtooth)
- **Noise**: `ma_noise` (white, pink, brownian) - not yet implemented
- **Sampler**: Audio buffer as control signal - not yet implemented
- **ADBR Envelope**: Attack-Decay-Break-Release envelope as control signal - not yet implemented
  - Outputs 0-1 over envelope stages
  - Can route to any parameter (filter cutoff, oscillator volume, etc.)
  - No sustain/gate - uses timed break point instead
  - Loopable (restarts after release completes)
  - Replaces envelope-as-effect (which only shapes amplitude)

### Processing
- Control-rate: once per audio block (~100 Hz at 48kHz/480 frames)
- Sources read `frame_count` frames, averaged to single value
- Routes apply: `value = offset + (source_value * depth)`
- Slew limiting: max change per second in target units

### Files
| File | Purpose |
|------|---------|
| `src/c/sampler_internal.h` | Type definitions, externs |
| `src/c/mod.c` | Source pool, route table, processing, setters, predicates |
| `src/c/sampler.c` | Audio callback hook |
| `src/c/init.c` | Registration |
| `src/prolog/sampler.pro` | Exports |
| `test/mod.pro` | Unit tests |
| `examples/mod_demo.pro` | Demo |

## Prolog API

### Implemented
```prolog
sampler_mod_lfo_create(+Type, +Freq, -Handle)           % Type = sine|square|triangle|sawtooth
sampler_mod_lfo_set_frequency(+Handle, +Freq)
sampler_mod_lfo_get_frequency(+Handle, -Freq)
sampler_mod_source_unload(+Handle)
sampler_mod_route_create(+SourceHandle, +TargetType, +TargetHandle, +Param, +Depth, +Offset, +Slew, -RouteHandle)
sampler_mod_route_unload(+RouteHandle)
```

### Not Yet Implemented
```prolog
sampler_mod_noise_create(+Type, -Handle)                % Type = white|pink|brownian
sampler_mod_sampler_create(+DataHandle, +Rate, -Handle)
sampler_mod_envelope_create(+Attack, +Decay, +Break, +BreakLevel, +Release, +Loop, -Handle)
sampler_mod_envelope_trigger(+Handle)                   % Start/restart envelope
sampler_mod_source_set_sh(+Handle, +IntervalFrames)     % 0 = disabled
```

## Route Target Types

### Implemented
- `oscillator` + `frequency`

### Not Yet Implemented
- `oscillator` + `volume`
- `voice` + `pan`
- `sound` + `pitch`, `volume`, `pan`
- `route` + `depth`, `offset`, `slew`
- `crossfader` + `position`
- Effect parameters (filter cutoff, reverb, delay, etc.)

## Crossfader

Audio mixer node that blends two sound sources based on position (0.0 = all A, 1.0 = all B).

```prolog
sampler_crossfader_create(+SoundA, +SoundB, -Handle)
sampler_crossfader_set_position(+Handle, +Position)  % 0.0-1.0
sampler_crossfader_get_position(+Handle, -Position)
sampler_crossfader_unload(+Handle)
```

Position is a modulation target - route an envelope or LFO to automate crossfades.

## Design Notes

### Depth Units
Depth is in target units. For oscillator frequency with offset=440 and depth=50, the LFO (outputting -1 to 1) produces frequency values from 390 to 490 Hz.

### Slew Units
Slew is max change per second in target units. Slew=1000 on frequency means max 1000 Hz/sec change.

### Thread Safety
Mutex (`g_mod_mutex`) protects all source/route operations. Audio thread locks briefly during `process_modulation()`.

---

## Patches / Presets

A patch is simply a Prolog goal that sets up voices, effects, modulation routes:

```prolog
patch_underwater :-
    sampler_synth_voice_create(V),
    sampler_synth_oscillator_add(V, 220.0, 0.0, O1),
    sampler_synth_oscillator_add(V, 221.5, 0.0, O2),  % slight detune
    sampler_mod_lfo_create(sine, 0.3, LFO),
    sampler_mod_route_create(LFO, oscillator, O1, frequency, 5.0, 220.0, 0.0, _),
    sampler_voice_attach_effect(V, lpf, [cutoff=400, order=4], _),
    sampler_synth_voice_start(V).
```

Call `patch_underwater.` to activate. Store multiple patches in a file, load and switch between them.

See `examples/patches.pro`.

---

## Control Interface (Future)

### Gamepad (real-time continuous control)

Xbox-style controller (e.g., Logitech) for hands-on parameter control.

**Available inputs:**
- 4 analog axes: left stick X/Y, right stick X/Y (-1.0 to 1.0)
- 2 analog triggers: LT, RT (0.0 to 1.0)
- ~14 buttons: X/Y/A/B, D-pad, bumpers (LB/RB), stick clicks (L3/R3), START/BACK

**Implementation:** SDL2 (or macOS Game Controller framework). C thread polls SDL, pushes events to thread-safe queue. Prolog drains queue:

```c
// C: event queue
typedef struct { int type; int id; float value; } gamepad_event_t;
void gamepad_push_event(gamepad_event_t e);  // SDL thread
int gamepad_pop_event(gamepad_event_t* e);   // Prolog calls this
```

```prolog
% Prolog: gamepad runs in separate thread, REPL stays free
start_gamepad :- thread_create(gamepad_loop, _, [detached(true)]).

gamepad_loop :- gamepad_drain_events, sleep(0.01), gamepad_loop.

gamepad_drain_events :- gamepad_next_event(Type, Id, Value), !,
    handle_gamepad(Type, Id, Value), gamepad_drain_events.
gamepad_drain_events.

handle_gamepad(axis, left_x, V) :- sampler_crossfader_set_position(XF, V).
handle_gamepad(button, a, 1) :- sampler_mod_envelope_trigger(Env).
```

### MCP + Claude (compositional control)

Expose Prolog predicates as MCP tools. Claude translates natural language to Prolog goals.

```
User: "Make it sound more underwater"
Claude → sampler_effect_set_parameters(Filter, [cutoff=200]),
         sampler_mod_lfo_set_frequency(L, 0.3)
```

- Not suitable for real-time continuous control (latency too high)
- Good for: patch changes, compositional decisions, parameter adjustments, complex multi-step operations
- Implementation: MCP server wrapping the Prolog layer

---

## Granular Engine (Future - Prolog Layer)

The granular engine will be implemented in Prolog, using existing C primitives:

**Available C primitives:**
- `sampler_data_load/2`, `sampler_data_extract/4` - load and slice audio
- `sampler_sound_create/2`, `sampler_sound_start/1`, `sampler_sound_stop/1` - playback
- `sampler_sound_set_pitch/2`, `sampler_sound_set_volume/2`, `sampler_sound_set_pan/2` - grain parameters
- `sampler_sound_attach_envelope/8` - per-grain amplitude envelope
- `sampler_capture_*` - live input as grain source

**Prolog responsibilities:**
- Grain scheduling (density, timing, randomization)
- Position selection (scan, random, follow)
- Parameter variation (pitch spread, pan spread)
- Voice/grain pool management

**No new C code needed** - timing precision of Prolog scheduling is sufficient for granular synthesis where some jitter is often desirable.
