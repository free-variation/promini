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
- [x] **LFO**: sine, square, triangle, sawtooth
- [x] **ADBR Envelope**: attack-decay-break-release, loopable
- [ ] **Noise**: white, pink, brownian
- [ ] **Sampler**: audio buffer as control signal

### Processing
- Control-rate: once per audio block (~100 Hz at 48kHz/480 frames)
- Routes apply: `value = offset + (source_value * depth)`
- LFO outputs -1 to 1, envelope outputs 0 to 1
- Slew limiting: max change per second in target units
- Volume setters use frame_count fade to avoid clicks

---

## Modulation Targets Checklist

### Oscillator (`oscillator`)
- [x] `frequency`
- [x] `volume`
- [ ] `phase`

### Voice (`voice`)
- [x] `volume`
- [x] `pan`

### Sound (`sound`)
- [x] `volume`
- [ ] `pitch`
- [x] `pan`

### LPF Effect (`lpf`)
- [ ] `cutoff`

### HPF Effect (`hpf`)
- [ ] `cutoff`

### BPF Effect (`bpf`)
- [ ] `cutoff`

### Delay Effect (`delay`)
- [ ] `wet`
- [ ] `decay`
- [ ] `delay_in_frames`

### Ping-Pong Delay Effect (`ping_pong_delay`)
- [ ] `wet`
- [ ] `feedback`
- [ ] `delay_in_frames`

### Reverb Effect (`reverb`)
- [ ] `wet`
- [ ] `decay`
- [ ] `damping`
- [ ] `predelay_ms`
- [ ] `shimmer1_mix`
- [ ] `shimmer2_mix`

### Bitcrush Effect (`bitcrush`)
- [ ] `bits`
- [ ] `sample_rate`

### LFO Source (`lfo`)
- [ ] `frequency`

### Envelope Source (`envelope`)
- [ ] `duration_ms`

### Route (`route`)
- [ ] `depth`
- [ ] `offset`
- [ ] `slew`

### Crossfader (`crossfader`) - not yet implemented
- [ ] `position`

---

## Known Issues

- [ ] **Ping pong delay volume increase** - wet+dry output louder than input, needs gain compensation
- [ ] **Pan modulation artifacts** - control-rate stepping causes buzz at high modulation rates; slew helps but doesn't eliminate; proper fix requires sample-rate pan interpolation

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
