# Synth System Implementation Plan

## Overall Vision

Live granular sampler + additive synth + effects + modulation, controlled from Prolog.

### Architecture Layers
- **C layer (miniaudio)**: Audio primitives - playback, effects, modulation sources, capture
- **Prolog layer**: High-level control - grain scheduling, sequencing, patch management

### Progress (~65% complete)

| Component | Status |
|-----------|--------|
| Audio playback (sounds, buffers, regions) | ✓ Complete |
| Additive synth (voices, oscillators, noise) | ✓ Complete |
| Effects (filters, reverb, delay, bitcrush) | ✓ Complete |
| Live capture | ✓ Complete |
| Modulation sources (LFO, envelope) | ✓ Complete |
| Modulation routing (osc frequency) | ✓ Complete |
| Modulation (remaining targets) | In progress |
| Granular engine | Planned (Prolog layer) |

---

## Modulation System

### Source Types
- **LFO**: sine, square, triangle, sawtooth (implemented)
- **ADBR Envelope**: attack-decay-break-release, loopable (implemented)
- **Noise**: white, pink, brownian (planned)
- **Sampler**: audio buffer as control signal (planned)

### Route Targets
- `oscillator` + `frequency` (implemented)
- `oscillator` + `volume`, `voice` + `pan`, `sound` + `pitch/volume/pan` (planned)
- `route` + `depth/offset/slew`, `crossfader` + `position` (planned)
- Effect parameters: filter cutoff, reverb, delay (planned)

### Processing
- Control-rate: once per audio block (~100 Hz at 48kHz/480 frames)
- Routes apply: `value = offset + (source_value * depth)`
- LFO outputs -1 to 1, envelope outputs 0 to 1
- Slew limiting: max change per second in target units

### Remaining Work
- Additional setters (oscillator volume, voice pan, sound params)
- Noise and sampler modulation sources
- Sample & Hold
- Crossfader mixer node

---

## Crossfader (Planned)

Audio mixer node blending two sources. Position (0.0-1.0) is a modulation target for automated crossfades.

---

## Patches / Presets

A patch is a Prolog goal that sets up voices, effects, modulation routes. See `examples/patches.pro`.

---

## Control Interface (Future)

### Gamepad
Xbox-style controller for real-time continuous control. SDL2 polls in C thread, Prolog drains event queue.

### MCP + Claude
Expose predicates as MCP tools. Good for patch changes and compositional decisions, not real-time control.

---

## Granular Engine (Future)

Implemented in Prolog using existing C primitives (`audio_load`, `sound_create`, `sound_set_pitch`, etc.). Prolog handles grain scheduling, position selection, parameter variation.
