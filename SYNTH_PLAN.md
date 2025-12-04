# Synth System Implementation Plan

## Overall Vision

Live granular sampler + additive synth + effects + modulation, controlled from Prolog.

### Architecture Layers
- **C layer (miniaudio)**: Audio primitives - playback, effects, modulation sources, capture
- **Prolog layer**: High-level control - grain scheduling, sequencing, patch management, mixing

---

## C Layer (Audio Primitives)

### Complete

| Component | Notes |
|-----------|-------|
| Audio playback | sounds, buffers, regions |
| Additive synth | voices, oscillators, noise |
| Effects | filters, reverb, delay, bitcrush, pan, moog, VCA, limiter |
| Live capture | |
| Modulation sources | LFO, envelope |
| Modulation routing | osc freq/vol, moog cutoff, VCA gain |
| Summing node | multiple sources to single output with effect chain |

### Next Steps

1. **Output device selection** - `promini_init/1` with device name for routing to BlackHole, etc.
2. **Route depth/center get/set predicates** - with per-sample interpolation
3. **Route as modulation target** - modulate depth/center from LFO/envelope
4. **Additional modulation targets** - sound pitch, delay params, reverb params, bitcrush
5. **Additional modulation sources** - noise, sampler (audio buffer as control signal)
6. **Image-to-audio synthesis** - pixel brightness → oscillator amplitude
7. **Control interface** - gamepad/keyboard input via SDL2, event queue for Prolog to drain

### Known Issues

- [ ] **Ping pong delay volume increase** - wet+dry output louder than input, needs gain compensation

---

## Prolog Layer (Full System)

### Complete

| Component | Notes |
|-----------|-------|
| Patch management | patches, presets saved as prolog clauses |

### Next Steps

1. **Mixer module** - composes VCA + pan + summing + limiter
   - `mixer_create/1`, `mixer_add_input/3`, `mixer_set_gain/2`, `mixer_set_pan/2`
   - Each input gets VCA and pan effects, connects to summing node
   - Summing node has master VCA and limiter in effect chain

2. **Crossfader** - composed from existing primitives (no C code needed)
   - Two VCAs on two sources, both to same summing node
   - Two mod routes from same LFO with inverse depths
   - Position controlled by LFO or envelope

3. **Granular engine** - grain scheduling using `sound_create`, `sound_set_pitch`, etc.

4. **Control interface** - drain SDL event queue, map inputs to parameters/actions

5. **MCP integration** - expose predicates as MCP tools for AI-assisted patch design

---

## Modulation System Details

### Source Types
- [x] LFO (sine, square, triangle, sawtooth)
- [x] ADBR Envelope (attack-decay-break-release, loopable)
- [ ] Noise (white, pink, brownian)
- [ ] Sampler (audio buffer as control signal)

### Targets Implemented
- Oscillator: frequency, volume
- Moog filter: cutoff
- VCA effect: gain

### Targets Planned
- Sound: pitch
- Delay: wet, decay, delay_in_frames
- Ping-pong delay: wet, feedback, delay_in_frames
- Reverb: wet, decay, damping, shimmer mix
- Bitcrush: bits, sample_rate
- LFO: frequency
- Envelope: duration_ms
- Route: depth, offset

---

## Image-to-Audio Synthesis

### Image Buffer Model

Images have original (`pixels`) and working buffer (`buffer`). Buffer may be smaller after downsampling:
- `buf_width` = time steps
- `buf_height` = oscillators per voice
- Downsampling averages blocks, shrinks buffer (2D run-length encoding)
- Quantizing reduces bit depth (posterization)

### Channel Mapping (Stereo)

| Channels | Voices | Panning |
|----------|--------|---------|
| 1 (grayscale) | 1 voice | center |
| 3 (RGB) | 3 voices | R=left, G=center, B=right |
| 4 (RGBA) | 3 voices | R=left, G=center, B=right, A=ignored |

Grayscale conversion is a creative choice: mono vs stereo RGB synth.

### Scan

Horizontal: columns = time, rows = frequency. Row 0 = lowest freq, row buf_height-1 = highest. Log-spaced frequencies.

### Interpolation

Cubic interpolation during audio generation to smooth transitions between buffer samples. Buffer stays small/blocky, interpolation happens per-sample during playback. Avoids clicks from sharp amplitude changes at block boundaries while preserving exact buffer values at grid points.
