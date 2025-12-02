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
| Crossfader | Planned |
| Granular engine | Planned (Prolog layer) |

### Next Steps (in order)
1. Image-to-audio synthesis (pixel-to-sine additive approach)
2. Sound pitch modulation target
3. Delay time modulation target
4. Reverb shimmer mix modulation targets
5. Bitcrush bits and sample_rate modulation targets
6. Crossfader node + position modulation target
7. Noise modulation source
8. Sampler modulation source

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

### Moog Effect (`moog`)
- [x] `cutoff`

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
- [x] **Pan modulation artifacts** - fixed with sample-rate interpolation in pan_process_pcm_frames()

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

### Frequency Mapping

Logarithmic spacing between `freq_low` and `freq_high`:
```
freq[i] = freq_low * pow(freq_high / freq_low, i / (num_bins - 1))
```

### C Structure

```c
typedef struct {
    ma_node_base base;

    /* pixel data */
    float* pixels;          /* width * height */
    ma_uint32 width;
    ma_uint32 height;

    /* oscillator bank */
    float* phases;          /* one per frequency bin */
    float* frequencies;     /* precomputed, log-spaced */
    ma_uint32 num_oscillators;

    /* playback state */
    float position;         /* current column/row (float for interpolation) */
    float speed;            /* time slices per second */
    ma_bool32 playing;
    ma_bool32 looping;
    ma_uint32 scan_mode;    /* 0 = horizontal, 1 = vertical */
    ma_uint32 num_slices;   /* width or height depending on mode */
} image_synth_t;
```

### Audio Callback

```
for each frame:
    if playing:
        advance position by (speed / sample_rate)
        if position >= num_slices:
            if looping: wrap to 0
            else: stop, set position to 0

        for each oscillator i:
            amplitude = get_pixel(position, i)  /* interpolate between slices */
            phase[i] += 2π * frequency[i] / sample_rate
            output += amplitude * sin(phase[i])

        output /= num_oscillators  /* normalize */
```

### Prolog Predicates

```prolog
% Creation - PixelData is list of rows, each row is list of floats 0.0-1.0
% ScanMode is 'horizontal' or 'vertical'
image_synth_create(PixelData, Width, Height, FreqLow, FreqHigh, ScanMode, Handle)

% Playback
image_synth_start(Handle)
image_synth_stop(Handle)
image_synth_loop(Handle, Bool)

% Parameters
image_synth_set_speed(Handle, SlicesPerSecond)
image_synth_set_position(Handle, SliceIndex)  % for scrubbing
image_synth_get_position(Handle, SliceIndex)

% Cleanup
image_synth_unload(Handle)
```

### Implementation Steps

1. Add `image_synth.c` module with structure and vtable
2. Implement `image_synth_create` - allocate, copy pixels, compute frequencies
3. Implement audio callback with horizontal scan
4. Add vertical scan mode
5. Add playback controls (start/stop/loop)
6. Add position get/set for scrubbing
7. Register Prolog predicates
8. Test with simple gradient images
9. Add position as modulation target (for LFO/envelope scrubbing)

### Future Enhancements

- Bidirectional scan (ping-pong)
- Per-oscillator attack/release envelope for smoother amplitude changes
- Multiple images blended/crossfaded
- Real-time pixel updates from Prolog

---

## Granular Engine (Future)

Implemented in Prolog using existing C primitives (`audio_load`, `sound_create`, `sound_set_pitch`, etc.). Prolog handles grain scheduling, position selection, parameter variation.
