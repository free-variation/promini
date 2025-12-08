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
| Modulation sources | LFO, envelope, gamepad |
| Modulation routing | osc freq/vol, moog cutoff, VCA gain, pan, ping-pong delay |
| Control interface | SDL3 gamepad input with dispatch hook for REPL integration |
| Summing node | multiple sources to single output with effect chain |

### Next Steps

1. **Additional modulation targets** - sound pitch, delay params, reverb params, bitcrush
2. **Additional modulation sources** - noise, sampler (audio buffer as control signal)
3. **Route depth/center get/set predicates** - with per-sample interpolation
4. **Route as modulation target** - modulate depth/center from LFO/envelope
5. **Output device selection** - `promini_init/1` with device name for routing to BlackHole, etc.
6. **Tap node** - passthrough node that copies audio to ring buffer for live granulation
   - Fixed size ring buffer (overwrites oldest)
   - Exposes write position for grain scheduling
   - Attaches to any source (synth, sound, summing node, image synth)

7. **Grain engine (hybrid C/Prolog)**
   - Voice pool (30-60 concurrent grains)
   - Sample-accurate playback from ring buffer or loaded audio
   - Per-grain: position, size, pitch, envelope shape, pan
   - Trigger queue for Prolog → C scheduling

8. **Corpus analyzer** - for concatenative synthesis
   - Segmenter: fixed interval or onset detection (envelope follower)
   - Analyzer: RMS (loudness), ZCR (brightness), optional pitch
   - Metadata array parallel to ring buffer
   - Query interface: expose grain features to Prolog

9. **Clock system**
   - Internal clock generator (BPM-based trigger stream)
   - Clock as mod source for tempo-synced LFOs, delays
   - Division/multiplication
   - Tap tempo support

10. ~~**Image-to-audio synthesis**~~ ✓ complete (additive, waveform, RGB stereo)

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
- [x] Gamepad (axes, triggers, d-pad as virtual axes)
- [ ] Noise (white, pink, brownian)
- [ ] Sampler (audio buffer as control signal)

### Targets Implemented
- Oscillator: frequency, volume
- Moog filter: cutoff
- VCA effect: gain
- Pan effect: pan
- Ping-pong delay: delay

### Targets Planned
- Sound: pitch
- Delay: wet, decay, delay_in_frames
- Ping-pong delay: wet, feedback
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

---

## Granular & Concatenative Synthesis

### Two Models

**Beads-style (blind/positional):**
- Position-based: "play grain at 500ms ago"
- Good for textures, smearing, time-stretching
- Parameters: position, size, density, pitch, feedback

**Concatenative (corpus-based):**
- Feature-based: "play a grain that sounds like X"
- Good for reconstruction, remix, semantic control
- Treats buffer as queryable database

### Corpus Data Structure

```c
typedef struct {
    size_t start_frame;
    size_t length;
    float rms;           /* loudness */
    float zcr;           /* zero-crossing rate (brightness) */
    float pitch;         /* 0 if unpitched/unknown */
} grain_metadata_t;
```

### Segmentation Strategies

1. **Fixed interval** - chop every N ms (simple, predictable)
2. **Onset detection** - envelope follower triggers on transients (musical)

### Prolog Query Interface

```prolog
% Get all grains from corpus
corpus_grains(CorpusId, GrainList).

% Get features for a grain
grain_features(GrainId, [rms=R, zcr=Z, pitch=P]).

% Find grains matching criteria
find_grain(G, Criteria) :-
    corpus_grains(corpus, Grains),
    member(G, Grains),
    matches_criteria(G, Criteria).

% Semantic categories
snare_sound(G) :- grain_features(G, F), member(rms=R, F), R > 0.7, member(zcr=Z, F), Z > 0.5.
kick_sound(G) :- grain_features(G, F), member(rms=R, F), R > 0.7, member(zcr=Z, F), Z < 0.3.

% Schedule grain playback
grain_play(GrainId, [pitch=1.0, envelope=hann, pan=0.0]).
```

### Use Cases

- **Live remix**: detect kicks/snares in drum loop, resequence
- **Mosaicing**: reconstruct target sound from corpus
- **Timbral search**: "find something bright and loud"
- **Generative sequencing**: logic-driven grain selection
