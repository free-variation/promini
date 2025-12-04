# Synth System Implementation Plan

## Overall Vision

Live granular sampler + additive synth + effects + modulation, controlled from Prolog.

### Architecture Layers
- **C layer (miniaudio)**: Audio primitives - playback, effects, modulation sources, capture
- **Prolog layer**: High-level control - grain scheduling, sequencing, patch management, mixing

### Progress

| Component | Status |
|-----------|--------|
| Audio playback (sounds, buffers, regions) | ✓ Complete |
| Additive synth (voices, oscillators, noise) | ✓ Complete |
| Effects (filters, reverb, delay, bitcrush, pan, moog) | ✓ Complete |
| VCA effect | ✓ Complete |
| Limiter effect | ✓ Complete |
| Live capture | ✓ Complete |
| Modulation sources (LFO, envelope) | ✓ Complete |
| Modulation routing | ✓ Complete (osc freq/vol, moog cutoff, VCA gain) |
| Summing node | ✓ Complete |
| Output device selection | Planned |
| Prolog mixer module | Planned |
| Image-to-audio synthesis | Planned |
| Granular engine | Planned (Prolog layer) |

### Next Steps (in order)
1. Output device selection (`promini_init/1` with device name for routing to BlackHole, etc.)
2. Prolog mixer module (composes VCA + pan + summing + limiter + crossfader)
3. Route depth/center get/set predicates and interpolation
4. Image-to-audio synthesis
5. Additional modulation targets (sound pitch, delay time, reverb params, bitcrush)
6. Additional modulation sources (noise, sampler)

---

## Mixing Architecture

Simple primitives in C, composed into mixers in Prolog.

### C Layer Primitives (all complete)

- **VCA Effect**: Gain control with per-sample interpolation
- **Pan Effect**: Stereo panning with per-sample interpolation
- **Limiter Effect**: Soft limiter with envelope follower
- **Summing Node**: Multiple sources mix to single output, can have effect chain

### Signal Flow

```
source → source's effect chain → summing node → summing node's effect chain → endpoint
```

### Prolog Mixer Module (planned)

Composes C primitives into higher-level abstractions:

```prolog
% Create mixer with master limiter
mixer_create(-Mixer)

% Add source with individual gain/pan control
mixer_add_input(+Mixer, +Source, -InputHandle)
mixer_set_gain(+InputHandle, +Gain)
mixer_set_pan(+InputHandle, +Pan)
mixer_remove_input(+InputHandle)

% Master controls
mixer_set_master_gain(+Mixer, +Gain)
mixer_set_limiter_threshold(+Mixer, +Threshold)

mixer_unload(+Mixer)
```

Implementation: Each input gets VCA and pan effects attached, connects to summing node. Summing node has master VCA and limiter in its effect chain.

### Crossfader (Prolog pattern)

Composed from existing primitives - no C-level node needed:
- Two VCAs (one on each source)
- Both sources connected to same summing node
- Two mod routes from same LFO, with inverse depths (one positive, one negative)

Position controlled by LFO or envelope. Both VCAs interpolate per-sample, so crossfade is smooth.

---

## Modulation System

### Source Types
- [x] **LFO**: sine, square, triangle, sawtooth
- [x] **ADBR Envelope**: attack-decay-break-release, loopable
- [ ] **Noise**: white, pink, brownian
- [ ] **Sampler**: audio buffer as control signal

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

## Known Issues

- [ ] **Ping pong delay volume increase** - wet+dry output louder than input, needs gain compensation

---

## Image-to-Audio Synthesis

Convert grayscale images to audio using pixel-to-sine additive synthesis. Each pixel brightness controls the amplitude of an oscillator at a corresponding frequency.

### Architecture

**C module**: `image_synth.c`
- Self-contained audio node using miniaudio
- Internal bank of sine oscillators
- Pixel data stored as float array

**Data flow**:
1. Prolog passes pixel data (list of lists, 0.0-1.0 values) at creation
2. C copies to internal buffer, precomputes log-spaced frequency table
3. Audio callback scans through image, updating oscillator amplitudes
4. Oscillators summed to output

### Scan Modes

| Mode | Frequency axis | Time axis | Oscillator count |
|------|---------------|-----------|------------------|
| horizontal | rows (Y) | columns (X), left→right | Height |
| vertical | columns (X) | rows (Y), top→down | Width |

### Prolog Predicates

```prolog
image_synth_create(PixelData, Width, Height, FreqLow, FreqHigh, ScanMode, Handle)
image_synth_start(Handle)
image_synth_stop(Handle)
image_synth_loop(Handle, Bool)
image_synth_set_speed(Handle, SlicesPerSecond)
image_synth_set_position(Handle, SliceIndex)
image_synth_get_position(Handle, SliceIndex)
image_synth_unload(Handle)
```

---

## Granular Engine (Future)

Implemented in Prolog using existing C primitives (`audio_load`, `sound_create`, `sound_set_pitch`, etc.). Prolog handles grain scheduling, position selection, parameter variation.

---

## Control Interface (Future)

### Gamepad
Xbox-style controller for real-time continuous control. SDL2 polls in C thread, Prolog drains event queue.

### MCP + Claude
Expose predicates as MCP tools. Good for patch changes and compositional decisions, not real-time control.
