# Synth System Implementation Plan

## Overall Vision

Live granular sampler + concatenative synthesis + additive synth + effects + modulation, controlled from Prolog.

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
| Modulation sources | LFO, envelope, gamepad, noise (with S&H) |
| Modulation routing | osc freq/vol, moog cutoff, VCA gain, pan, ping-pong delay, granular position |
| Control interface | SDL3 gamepad input with dispatch hook for REPL integration |
| Summing node | multiple sources to single output with effect chain |
| Clock system | BPM-based clock with routes to LFO, envelope, granular, delays |
| Granular delay | Beads-style with ring buffer, tempo sync, sound buffer loading |
| Image-to-audio | additive, waveform, RGB stereo modes |
| Reverb | Dattorro plate with shimmer (external 4-grain, in-loop 2-grain), freeze, size, tank HP, modulation |
| SDL visualizer | waveform, spectrum, spectrogram; multiple windows; auto-range; themes |
| Multiple keyboard windows | slot-based array, per-window event routing |
| Polyphonic keyboard | voice pool per row, voice stealing (oldest), envelope gate API, key repeat handling |
| Gamepad buttons | cycling, momentary, trigger, toggle modes |
| Keyboard mod source | velocity and note as modulation sources |

### Next Steps

1. **Concatenative granular synthesis** - corpus-based descriptor matching
   - Corpus analysis: segment audio, extract descriptors (centroid, flatness, pitch, RMS, etc.)
   - Search/matching: nearest neighbor in descriptor space
   - Target modes: manual, audio follower, trajectory, random walk
2. **Step sequencer / Turing Machine** - pattern-based note/parameter control, clock-synced
3. **Euclidean sequencer** - Bjorklund algorithm for rhythm generation
4. **Arpeggiator** - held notes to sequence patterns
5. **Effects** - chorus/flanger/phaser, waveshaping/saturation, comb filter
6. **Karplus-Strong** - plucked string synthesis
7. **Spectral effects** - FFT-based freeze, blur, morph
8. **Prolog composition DSL** - declarative patterns, harmonic rules, generative constraints

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

3. **Granular engine (Beads-style)** - blind position-based granulation
   - Prolog schedules grains by position/size/pitch
   - Modes: continuous density, gated bursts, clocked

4. **Concatenative queries** - semantic grain selection from corpus
   - `find_grain(G, [high_loudness, low_zcr])` - query by features
   - Define categories: `snare_sound(G)`, `kick_sound(G)`
   - Pattern-based sequencing with logic

5. **MCP integration** - expose predicates as MCP tools for AI-assisted patch design

---

## Modulation System Details

### Source Types
- [x] LFO (sine, square, triangle, sawtooth)
- [x] ADBR Envelope (attack-decay-break-release, loopable)
- [x] Gamepad axes (sticks, triggers, d-pad as virtual axes)
- [x] Gamepad buttons (cycling, momentary, trigger, toggle modes)
- [x] Noise (white, pink, brownian) with sample & hold
- [x] Keyboard (velocity, note number)
- [ ] Sampler (audio buffer as control signal)

### Targets Implemented
- Oscillator: frequency, volume
- Moog filter: cutoff, resonance
- VCA effect: gain
- Pan effect: pan
- Ping-pong delay: delay
- Granular: density, pitch, position, size, position_spray, size_spray, envelope, regularity, reverse, pan, pan_spray, recording, trigger, reset, deviation_up, deviation_down, wet
- Reverb: wet, decay, damping, size, freeze

### Targets Planned
- Sound: pitch
- Delay: wet, decay, delay_in_frames
- Ping-pong delay: wet, feedback
- Reverb: shimmer mix, predelay, bandwidth
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

---

## Granular & Concatenative Synthesis

### Two Models

**Beads-style (blind/positional):** ✓ Complete
- Position-based: "play grain at 500ms ago"
- Good for textures, smearing, time-stretching
- Parameters: position, size, density, pitch, spray, reverse, envelope shape, mode quantization

**Concatenative (corpus-based):** Planned
- Feature-based: "play a grain that sounds like X"
- Good for reconstruction, remix, semantic control
- Treats buffer as queryable database

### Corpus Data Structure

```c
#define MAX_CORPUS_SEGMENTS 4096
#define NUM_DESCRIPTORS 8

typedef struct {
    ma_uint32 start_frame;
    ma_uint32 length_frames;
    float descriptors[NUM_DESCRIPTORS];  /* centroid, spread, flatness, rolloff, rms, zcr, pitch, pitch_confidence */
} corpus_segment_t;

typedef struct {
    ma_bool32 in_use;
    int audio_buffer_slot;
    corpus_segment_t segments[MAX_CORPUS_SEGMENTS];
    int segment_count;
} corpus_t;
```

### Descriptors

| Index | Name | Description |
|-------|------|-------------|
| 0 | centroid | Spectral brightness (weighted mean frequency) |
| 1 | spread | Spectral width |
| 2 | flatness | Noisiness (geometric/arithmetic mean ratio) |
| 3 | rolloff | Frequency below which 85% energy lies |
| 4 | rms | Loudness |
| 5 | zcr | Zero-crossing rate |
| 6 | pitch | Fundamental frequency estimate |
| 7 | pitch_confidence | How tonal vs noisy |

### Segmentation Strategies

1. **Fixed grid** - equal-sized slices (simple, predictable)
2. **Onset detection** - spectral flux peaks (musical boundaries)
3. **Beat-aligned** - use clock/tempo for rhythmic material

### Target Modes

1. **Manual** - user specifies descriptor values directly
2. **Audio follower** - analyze live input, match corpus to it
3. **Trajectory** - interpolate through descriptor space over time
4. **Random walk** - Brownian motion through descriptor space

### Prolog Interface

```prolog
corpus_init(-Corpus, +AudioBuffer)
corpus_analyze(+Corpus, +Options)     % method, segment_ms
corpus_segment_count(+Corpus, -Count)
corpus_get_descriptor(+Corpus, +SegmentIdx, +DescriptorIdx, -Value)

concat_player_init(-Player, +Corpus)
concat_player_set_target(+Player, +Descriptors)
concat_player_set_weights(+Player, +Weights)
concat_player_follow(+Player, +AudioSource)
concat_player_start(+Player)
concat_player_stop(+Player)
```

### Use Cases

- **Live remix**: detect kicks/snares in drum loop, resequence
- **Mosaicing**: reconstruct target sound from corpus
- **Timbral search**: "find something bright and loud"
- **Audio following**: voice controls which drum sounds play
- **Generative sequencing**: logic-driven grain selection
