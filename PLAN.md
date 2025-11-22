# Promini: Granular Sampler Project Plan

## Project Overview
Building a granular sampler using SWI-Prolog interface to miniaudio library (engine API). The system enables spawning many overlapping short audio grains from shared sample buffers, each with independent playback parameters (position, pitch, pan, volume, envelope), suitable for granular synthesis applications.

## Architecture
- **Language:** C implementation with SWI-Prolog Foreign Language Interface
- **Audio Engine:** miniaudio v0.11.23 (engine API)
- **Memory Model:**
  - Shared PCM data buffers (MAX_DATA_BUFFERS = 32)
  - Independent sound instances with separate cursors (MAX_SOUNDS = 1024)
  - Each sound has its own ma_audio_buffer referencing shared pData
  - Reference counting for buffer lifecycle management
- **Effects Architecture:**
  - Per-sound effects using node graph
  - Effects are nodes attached between sound and engine mixer
  - Each sound can have independent effect chain

## Completed Steps

### Step 1: Basic Engine & Device Management ✓
**Files:** src/c/sampler.c, src/prolog/sampler.pro
- `sampler_version/1` - Returns miniaudio version
- `sampler_init/0` - Initialize audio engine (auto-called if needed)
- `sampler_devices/1` - Query available audio devices
- Engine singleton pattern with ENSURE_ENGINE_INITIALIZED macro

### Step 2: Sound Loading & Playback Control ✓
- `sampler_sound_load/2` - Load sound from file, returns handle
- `sampler_sound_unload/1` - Free sound resources
- `sampler_sound_start/1` - Begin playback
- `sampler_sound_stop/1` - Stop playback
- `sampler_sound_is_playing/1` - Query playback state
- `sampler_sound_set_looping/2` - Enable/disable looping
- `sampler_sound_is_looping/1` - Query loop state
- Helper predicates: `sampler_sound_loop/1`, `sampler_sound_no_loop/1`

### Step 3: Data Buffer Sharing & Polyphony ✓
**Key Achievement:** Multiple sounds can share PCM data with independent playback cursors
- `sampler_data_load/2` - Load audio file into shared buffer, returns data handle
- `sampler_data_unload/1` - Free data buffer when refcount reaches zero
- `sampler_sound_create/2` - Create sound instance from data buffer
- `sampler_sound_seek/2` - Seek to frame position
- `sampler_sound_get_position/2` - Get current playback frame
- `sampler_data_info/2` - Returns data_info(frames, channels, sample_rate, duration)
- `sampler_sound_length/2` - Get total length in frames
- `sampler_sound_start_at/2` - Convenience: seek + start
- `sampler_data_extract/4` - Extract slice of frames into new buffer
- `sampler_sound_set_range/3` - Set playback range (start/end frames)

**Implementation Details:**
- `pl_sampler_sound_create()` creates new ma_audio_buffer per sound
- All buffers share same pData pointer (memory efficient)
- Each buffer has independent ma_audio_buffer_ref (independent cursor)
- Fixed bug: must set `bufferConfig.sampleRate` explicitly after ma_audio_buffer_config_init

### Step 4: Pitch/Rate/Pan/Volume Control ✓
**Pitch Control (Semitones Interface):**
- `sampler_sound_set_pitch/2` - Set pitch in semitones (12 = octave up, -12 = octave down)
- `sampler_sound_get_pitch/2` - Get pitch in semitones
- Conversion: `ratio = pow(2.0, semitones/12.0)` and `semitones = 12.0 * log2(ratio)`
- Uses ma_sound_set_pitch() which takes ratio (1.0 = normal, 2.0 = double speed)

**Pan Control:**
- `sampler_sound_set_pan/2` - Set stereo pan (-1.0 = left, 0.0 = center, 1.0 = right)
- `sampler_sound_get_pan/2` - Get pan value
- `sampler_sound_set_pan_mode/2` - Set mode: 'balance' or 'pan'
- `sampler_sound_get_pan_mode/2` - Get current mode
  - **balance mode (default):** Panning right attenuates left channel, no blending
  - **pan mode:** Left channel content moves to right and blends with right channel
- **Note:** Panning only affects stereo (2 channel) sources; mono sources ignored by panner
- **Note:** Mono sources can be spatialized using 3D positioning (ma_sound_set_position, etc.)

**Volume Control:**
- `sampler_sound_set_volume/2` - Set volume (1.0 = normal, 0.0 = silence, >1.0 = amplification)
- `sampler_sound_get_volume/2` - Get current volume

**Code Quality:**
- GET_SOUND_FROM_HANDLE macro eliminates boilerplate validation code
- Applied to 11+ functions for cleaner implementation

### Step 5: Buffer Transformations ✓
**Goal:** Enable buffer transformations for granular effects and processing

**5a. Reverse Playback**
- `sampler_data_reverse/2` - Creates new data buffer with reversed frame order
- `sampler_data_load_reversed/2` - Convenience wrapper: load, reverse, unload original
- Reverses frame order while preserving channel interleaving within frames
- Implementation at sampler.c:1014

**5b. Sample Rate Conversion (Resampling)**
- `sampler_data_resample/3` - Creates new data buffer at different sample rate
- Uses `ma_resampler` with linear algorithm
- Automatically calculates new frame count: `new_frames = old_frames * (new_rate / old_rate)`
- Implementation at sampler.c:1065

**Use Cases:**
- Time-stretching without pitch change: Resample to different rate, play at adjusted pitch
- Pitch-shifting without time change: Resample then play at different rate
- Sample rate normalization: Convert all buffers to engine sample rate

**5c. Bit Depth Reduction (Bit Crushing)**
- `sampler_data_bit_reduce/3` - Reduces bit depth by quantizing samples
- Supports 1-16 bits (1 = extreme, 4-8 = classic lo-fi, 16 = no reduction)
- **BONUS:** NEON SIMD optimizations for ARM platforms (sampler.c:1193-1206)
- Handles both f32 and s16 formats with scalar fallback
- Implementation at sampler.c:1137

**Use Cases:**
- Lo-fi aesthetics (8-bit, chiptune sounds)
- Vintage sampler emulation (12-bit)
- Extreme digital distortion (1-4 bits)
- Granular texture variation

### Grain Primitives ✓
**All core functionality for creating and controlling individual grains:**
- Create sound from data buffer (`sampler_sound_create/2`)
- Set playback position (`sampler_sound_seek/2`)
- Control pitch, volume, pan (Steps 4 functions)
- Start/stop playback (`sampler_sound_start/1`, `sampler_sound_stop/1`)
- Query state (playing, looping, position)
- Extract buffer slices (`sampler_data_extract/4`)
- Set playback range (`sampler_sound_set_range/3`)

**Status:** All primitives needed to manually create and play grains are implemented. Remaining steps add real-time effects processing, envelopes, and live capture capabilities.

## Remaining Steps

### Step 6: Per-Sound Effects (Bit Crushing Implementation)
**Goal:** Implement per-sound real-time effects using miniaudio's node graph

**Architecture:**
- Effects are custom nodes using `ma_node_vtable`
- Each sound can have effect nodes attached between sound and engine mixer
- Signal flow: `sound → effect1 → effect2 → ... → engine_mixer → device`

**Effect Chain Design:**
- Linked list tracks effects for each sound
- Each effect has internal state (per-sound instances required)
- Supports arbitrary number of effects per sound
- Effects appended to chain in attachment order

**Key Design Decisions:**
- **Linked list over array:** No arbitrary limits, flexible insertion/removal
- **Per-sound instances:** Effects have state (filter registers, envelope stages, delay buffers)
- **Node graph routing:** miniaudio handles actual signal flow via node connections

**Components to Implement:**
1. Effect type enum (BITCRUSH, ENVELOPE, REVERB, filters)
2. Effect chain linked list structure
3. Bitcrush effect node with vtable and processing callback
4. Attach/detach/update functions for bitcrush
5. Effect chain cleanup in sound unload
6. Prolog interface: `attach_bitcrush`, `detach_bitcrush`, `update_bitcrush`

**Bitcrush Parameters:**
- Bit depth: 1-16 bits
- Sample rate reduction: 0 = disabled, or target Hz
- Sample-and-hold for downsampling effect

**This Establishes Pattern For:**
- All other per-sound effects (envelope, filters, reverb)
- Effect chain management
- Dynamic parameter updates

### Step 7: ADBR Envelope Generator ✓
**Goal:** Per-sound amplitude envelopes for grain shaping

**Why ADBR (not ADSR):**
- Granular synthesis uses fixed-duration triggers, not held notes
- All stages time-based, no gate/release distinction
- Simpler state machine

**ADBR Stages:**
1. **Attack:** 0 → 1.0 over attack_proportion × duration
2. **Decay:** 1.0 → break_level over decay_proportion × duration
3. **Break:** Hold at break_level for break_proportion × duration
4. **Release:** break_level → 0 for remaining duration

**Implementation:**
- Effect node following bitcrush pattern (sampler.c:1550-1618)
- State: current stage, stage progress (0.0-1.0), current envelope value
- Processing: Calculate envelope value per-frame, multiply input samples
- Parameters: attack/decay/break proportions (0.0-1.0), break_level (0.0-1.0), duration_ms, loop flag

**Prolog Interface:**
- `sampler_sound_attach_envelope/8` - Attach envelope with all parameters
- `sampler_sound_attach_effect/4` - Generic attach (envelope or any effect)
- `sampler_effect_detach/1` - Remove effect from chain
- `sampler_effect_set_parameters/2` - Update parameters without detaching
- `sampler_sound_effects/2` - Query attached effects and parameters
- `sampler_sound_clear_effects/1` - Remove all effects from sound

**Effect Management:**
- Effects return handles: `effect(SoundHandle, EffectPointer)`
- Handles used for parameter updates and removal
- Effect chain automatically reconnected on detachment
- GET_EFFECT_FROM_HANDLE macro for validation

**Future Enhancements:**
- Curve shaping: linear, exponential, logarithmic
- Per-stage curve control

### Step 8: Live Capture with Ring Buffer
**Goal:** Real-time audio capture for live granular synthesis

**Architecture:**
- Capture device writes to lock-free ring buffer (`ma_pcm_rb`)
- Continuous recording, oldest data overwritten
- Snapshot extraction: copy frames from ring buffer to data buffer
- Extracted buffers used like any other data buffer (spawn grains, etc.)

**Key Design: Snapshot Approach**
- Copy data from ring buffer instead of direct playback
- Grains play from stable snapshot, unaffected by ring buffer wrap
- Trade-off: Memory usage vs simplicity (acceptable for granular synthesis)

**Components:**
- Capture ring buffer structure with device, ring buffer, metadata
- Capture callback: writes input to ring buffer (audio thread)
- Extract function: snapshots frames to new data buffer (Prolog thread)
- Info query: current position, available frames, buffer capacity

**Thread Safety:**
- `ma_pcm_rb` is lock-free (single producer/single consumer)
- Audio thread writes, Prolog thread reads
- No mutexes needed

**Prolog Interface:**
- `capture_start`: Initialize capture device and ring buffer
- `capture_stop`: Stop device, free resources
- `capture_extract`: Snapshot frames to data buffer (handles wrap-around)
- `capture_get_info`: Query buffer state

**Buffer Sizing:**
- Typical: 5-30 seconds (240k-1.44M frames at 48kHz)
- Must exceed longest grain duration + extraction latency
- Larger = more history for grain extraction flexibility

### Step 9: Expose Existing miniaudio Filters
**Goal:** Wrap miniaudio's built-in filters as per-sound effect nodes

**Filters to Expose:**
- **Biquad:** Generic biquad with direct coefficient control
- **Low-pass:** Butterworth low-pass (1st, 2nd, or higher order)
- **High-pass:** Butterworth high-pass (1st, 2nd, or higher order)
- **Band-pass:** Band-pass filter (2nd order or higher)
- **Notch:** Notch filter for frequency removal
- **Peaking EQ:** Boost/cut at specific frequency
- **Shelving:** Low shelf and high shelf filters

**Implementation:**
- Effect nodes following bitcrush pattern
- Wrap miniaudio filter structs (ma_lpf2, ma_hpf2, etc.)
- Processing callback delegates to miniaudio's filter functions
- Each filter type has attach/detach/update functions

**Common Parameters:**
- Cutoff/center frequencies in Hz
- Filter order (1, 2, or up to 8 for high-order)
- Q factor for resonance (0.5-10.0 typical)
- Gain in dB (for EQ filters)
- All support dynamic parameter updates

**Prolog Interface Pattern:**
- `attach_[filter]`: Initialize filter with parameters
- `detach_[filter]`: Remove from effect chain
- `update_[filter]`: Change parameters without detaching

### Step 10: Reverb
**Goal:** High-quality reverb effect

**Algorithm Options:**
1. **FreeVerb** - Public domain, Schroeder reverb (comb + allpass)
2. **Dattorro Plate** - Mutable Instruments Clouds algorithm (complex, high quality)
3. **Simple Algorithmic** - Basic comb + allpass from scratch

**Recommendation:** Start with FreeVerb
- Well-documented, proven algorithm
- Good quality-to-complexity ratio
- C++ but straightforward to port to C

**Implementation:**
- Effect node following established pattern
- Internal state: comb filter delay lines, allpass delay lines
- Processing: Apply reverb algorithm to input samples
- Parameters: room size, damping, wet/dry mix, stereo width

**Per-Sound vs Global:**
- **Per-sound:** Each grain has independent reverb (more CPU, more control)
- **Global:** All sounds feed one reverb (less CPU, still useful)
- Start with per-sound for flexibility

**Prolog Interface:**
- `attach_reverb`: Initialize reverb on sound
- `detach_reverb`: Remove from chain
- `set_reverb_*`: Update room size, damping, wet, width (0.0-1.0 range)

## Additional Considerations

### Performance Targets
- Support 100+ concurrent grains without glitches
- Per-sound effects with <5% CPU overhead each
- Grain spawn latency < 1ms
- Overall CPU usage < 25% on modern hardware

### Testing Strategy
- **Unit Tests:** Each function in isolation
- **Integration Tests:** Multi-step workflows
- **Stress Tests:** MAX_SOUNDS concurrent grains with effects
- **Audio Quality:** Listen tests, no clicks/pops, smooth envelopes
- **Memory:** No leaks, verify effect node cleanup

### Code Organization
```
promini/
├── include/
│   └── miniaudio.h
├── lib/
│   └── sampler.dylib
├── src/
│   ├── c/
│   │   ├── sampler.c         # Core implementation
│   │   ├── effects.c         # Effect nodes (bitcrush, envelope)
│   │   ├── filters.c         # miniaudio filter wrappers
│   │   ├── capture.c         # Ring buffer capture
│   │   └── reverb.c          # Reverb implementation
│   └── prolog/
│       └── sampler.pro       # API exports
├── audio/
├── examples/
└── Makefile
```

### Naming Conventions
- Underscores only, no camelCase
- Prolog predicates: `sampler_*`
- C functions: `pl_sampler_*` (Prolog callable)
- Effect nodes: `*_node_t`
- Node processing: `*_process_pcm_frames`

## Summary
**Current state:** Steps 1-5 complete - all grain primitives implemented

**Remaining:** Steps 6-10 - per-sound effects, envelopes, live capture, filters, reverb

**Next Priority:** Step 6 (bit crushing as effect node) - establishes pattern for all other effects

**End goal:** Production-ready granular sampler with per-sound effects, live capture, and rich sound processing
