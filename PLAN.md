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

## Remaining Steps

### Step 5: Buffer Transformations (Reverse, Resample)
**Goal:** Enable buffer transformations for granular effects and processing

**5a. Reverse Playback**

**Design Decision:**
- miniaudio does not support negative pitch for reverse playback (pitch <= 0 is rejected)
- Solution: Create reversed copies of data buffers in memory
- Grains can then be spawned from either original or reversed buffer

**Function to Implement:**

**Reverse Buffer Creation**
```c
sampler_data_reverse(+SourceDataHandle, -ReversedDataHandle)
```
- Creates new data buffer with reversed frame order
- Allocates new data slot
- Copies PCM data from source
- Reverses frames in memory (preserves channel interleaving within frames)
- Returns handle to reversed buffer

**Convenience Wrapper in Prolog:**
```prolog
sampler_data_load_reversed(Path, ReversedHandle) :-
    sampler_data_load(Path, TempHandle),
    sampler_data_reverse(TempHandle, ReversedHandle),
    sampler_data_unload(TempHandle).
```

**Implementation Details:**
```c
// For stereo: frame = [left, right]
// Reverse frame order, not individual samples
// Original: [F0_L, F0_R, F1_L, F1_R, F2_L, F2_R]
// Reversed: [F2_L, F2_R, F1_L, F1_R, F0_L, F0_R]

for (i = 0; i < frame_count; i++) {
    for (ch = 0; ch < channels; ch++) {
        reversed_data[(frame_count - 1 - i) * channels + ch] =
            original_data[i * channels + ch];
    }
}
```

**Use Cases:**
- Reversed grains for texture variation
- Backwards/forwards grain clouds
- Palindromic grain sequences
- Granular time-stretching effects

**Testing Plan:**
- Load buffer, reverse it, verify playback is backwards
- Check multi-channel files preserve channel order
- Verify memory management (refcounting, cleanup)
- Test with different sample rates and bit depths

**5b. Sample Rate Conversion (Resampling)**

**Goal:** Convert buffers between sample rates for time-stretching and pitch-shifting effects

**Function to Implement:**

**Resample Buffer**
```c
sampler_data_resample(+SourceDataHandle, +NewSampleRate, -ResampledDataHandle)
```
- Creates new data buffer at different sample rate
- Uses `ma_resampler` to convert between rates
- Automatically calculates new frame count: `new_frames = old_frames * (new_rate / old_rate)`
- Supports linear or higher quality resampling algorithms
- Returns handle to resampled buffer

**Implementation Details:**
```c
// 1. Get source buffer info
get_buffer_info(source_buffer, &old_frames, &channels, &old_rate);
format = source_buffer->ref.format;

// 2. Calculate output size
new_frames = (ma_uint64)((double)old_frames * new_rate / old_rate);

// 3. Initialize resampler
ma_resampler_config config = ma_resampler_config_init(
    format, channels, old_rate, new_rate,
    ma_resample_algorithm_linear);  // or speex for higher quality

ma_resampler resampler;
ma_resampler_init(&config, NULL, &resampler);

// 4. Allocate output buffer
resampled_data = malloc(new_frames * ma_get_bytes_per_frame(format, channels));

// 5. Process all frames
ma_uint64 frames_in = old_frames;
ma_uint64 frames_out = new_frames;
ma_resampler_process_pcm_frames(&resampler, source_data, &frames_in,
                                 resampled_data, &frames_out);

// 6. Create audio buffer with new sample rate
ma_resampler_uninit(&resampler);
```

**Use Cases:**
- **Time-stretching without pitch change:** Resample to different rate, then play at adjusted pitch to compensate
  ```prolog
  % Stretch to 2x length without changing pitch
  sampler_data_resample(Source, 24000, Stretched),  % Half sample rate
  sampler_sound_create(Stretched, Sound),
  sampler_sound_set_pitch(Sound, 12.0).  % Up one octave to compensate
  ```
- **Pitch-shifting without time change:** Resample then play at different rate
  ```prolog
  % Shift pitch up octave without changing duration
  sampler_data_resample(Source, 96000, Shifted),  % Double sample rate
  sampler_sound_create(Shifted, Sound),
  sampler_sound_set_pitch(Sound, -12.0).  % Down octave for normal speed
  ```
- **Sample rate normalization:** Convert all buffers to engine sample rate
- **Granular effects:** Create grains at different densities via resampling

**Testing Plan:**
- Resample up (e.g., 44.1kHz → 48kHz), verify pitch/duration changes
- Resample down (e.g., 48kHz → 22.05kHz), verify quality degradation is acceptable
- Test round-trip: resample up then down, compare to original
- Verify multi-channel resampling preserves channel relationships
- Performance: measure resampling time for various buffer sizes

**5c. Bit Depth Reduction (Bit Crushing)**

**Goal:** Lo-fi effects via quantization and sample rate reduction

**Function to Implement:**

**Bit Crush Buffer**
```c
sampler_data_bit_crush(+SourceDataHandle, +TargetBits, +TargetSampleRate, -CrushedDataHandle)
```
- Reduces bit depth by quantizing samples to fewer bits
- Optionally downsamples to lower sample rate
- Returns buffer at original or target sample rate (with aliasing preserved)
- Creates characteristic digital distortion and noise

**Implementation Details:**
```c
// 1. Bit depth reduction (quantization)
// For N-bit output, quantize to 2^N levels
int levels = 1 << target_bits;  // e.g., 4 bits = 16 levels
float step = 2.0f / levels;      // Quantization step size

for each sample:
    // Quantize (assuming normalized float -1.0 to 1.0)
    quantized = floor(sample / step) * step;
    // Add slight rounding for centered levels
    quantized = floor(sample / step + 0.5) * step;

// 2. Sample rate reduction (optional)
if (target_rate < current_rate):
    // Use ma_resampler to downsample
    resample(buffer, target_rate);

    // Optionally upsample back to original rate
    // (keeps aliasing artifacts)
    if (preserve_original_rate):
        resample(buffer, original_rate);
```

**Parameters:**
- **TargetBits:** 1-16 (1 = extreme, 4-8 = classic lo-fi, 16 = no bit reduction)
- **TargetSampleRate:** Target rate or 0 to skip sample rate reduction
  - Common lo-fi rates: 8000, 11025, 22050 Hz
  - Extreme: 4000, 2000 Hz

**Variants:**
```prolog
% Just bit reduction
sampler_data_bit_crush(Source, 4, 0, Crushed).

% Just sample rate reduction (use resample instead)
sampler_data_resample(Source, 8000, Crushed).

% Full bit crush effect
sampler_data_bit_crush(Source, 8, 11025, Crushed).
```

**Use Cases:**
- Lo-fi aesthetics (8-bit, chiptune sounds)
- Vintage sampler emulation (12-bit)
- Extreme digital distortion (1-4 bits)
- Telephone/radio quality (8-bit, 8kHz)
- Granular texture variation (different grains at different bit depths)

**Testing Plan:**
- Test various bit depths (16, 8, 4, 2, 1), listen for quantization noise
- Verify extreme settings (1-bit) produce recognizable distortion
- Test sample rate reduction combined with bit reduction
- Compare against reference bit crusher plugins
- Verify multi-channel processing maintains phase relationships

### Step 6: Grain Lifecycle Management
**Goal:** Enable automatic management of short-lived grains without manual cleanup

**Functions to Implement:**

1. **Stop Time Scheduling**
   ```c
   sampler_sound_set_stop_time(+Handle, +DurationFrames)
   ```
   - Schedules sound to automatically stop after N frames from current engine time
   - Uses `ma_sound_set_stop_time_in_pcm_frames(sound, ma_engine_get_time_in_pcm_frames(engine) + duration)`
   - Essential for fixed-duration grains

2. **End Detection**
   ```c
   sampler_sound_at_end(+Handle)
   ```
   - Query predicate: succeeds if sound has reached the end
   - Uses `ma_sound_at_end(sound)`
   - Enables polling for finished grains

3. **Optional: End Callback Registration**
   ```c
   sampler_sound_set_end_callback(+Handle, +CallbackAtom)
   ```
   - Register Prolog predicate to call when sound finishes
   - **Challenge:** miniaudio callback fires from audio thread
   - **Solution:** Queue finished handles for Prolog thread to poll
   - Thread-safe queue of completed sound handles
   - Prolog calls `sampler_poll_finished_sounds(-Handles)` to get list
   - Deferred to avoid thread-safety complexity if polling sufficient

4. **Cleanup Strategy**
   - Option A: Manual - Prolog polls and unloads finished sounds
   - Option B: Automatic - C code marks slots as reusable when sound ends
   - Decide based on use patterns

**Testing Plan:**
- Create grain, set stop time, verify it stops and `at_end` succeeds
- Spawn multiple grains with different durations, verify independent operation
- Test with MAX_SOUNDS concurrent grains

### Step 7: ADBR Envelope Generator
**Goal:** Implement trigger-based envelope for grain amplitude shaping

**Design Decision:** ADBR (trigger-based) instead of ADSR (gate-based) because:
- Granular synthesis uses fixed-duration triggers, not held notes
- All stages time-based, no need for note-off detection
- Simpler state machine

**ADBR Envelope Stages:**
1. **Attack:** 0 → 1.0 over attack_time
2. **Decay:** 1.0 → break_level over decay_time
3. **Break:** hold at break_level for break_time
4. **Release:** break_level → 0 over release_time

**Per-Sound Envelope State:**
```c
typedef struct {
    // Parameters
    float attack_time;   // seconds
    float decay_time;
    float break_time;
    float break_level;   // 0.0 to 1.0
    float release_time;

    // State
    enum { ENV_OFF, ENV_ATTACK, ENV_DECAY, ENV_BREAK, ENV_RELEASE } stage;
    ma_uint64 stage_start_frame;
    float current_value;
    ma_bool32 enabled;
} envelope_state_t;
```

**Integration with Sound Playback:**
- Add envelope_state to sound_slot_t
- Hook into ma_sound via custom data source wrapper or post-processing
- **Challenge:** miniaudio ma_sound doesn't have per-frame processing hook
- **Solution Options:**
  1. Use ma_sound_group with custom effect node in graph
  2. Replace ma_sound with ma_data_source + manual node graph construction
  3. Use ma_sound_set_volume() and update from separate thread (coarse, not per-frame)
  4. Create custom data_source wrapper that applies envelope then feeds ma_sound

**Recommended Approach:** Custom data source wrapper
- Implement ma_data_source vtable
- Wraps ma_audio_buffer
- read_pcm_frames() applies envelope per-frame before returning
- Sound is created from custom source instead of raw buffer

**Functions to Implement:**
```c
sampler_sound_set_envelope(+Handle, +Attack, +Decay, +Break, +BreakLevel, +Release)
sampler_sound_enable_envelope(+Handle)
sampler_sound_disable_envelope(+Handle)
sampler_sound_get_envelope(+Handle, -Params)
sampler_sound_trigger_envelope(+Handle)  // restart envelope from beginning
```

**Optional: Simpler Envelopes First**
- Linear fade in/out using `ma_sound_set_fade_in_pcm_frames()`
- Trapezoid envelope (attack, sustain, release)
- Useful for testing before full ADBR implementation

**Curve Shaping (Future Enhancement):**
- Linear, exponential, logarithmic attack/release curves
- Match Clouds implementation: "shape can be independently adjusted from exponential, through linear, to logarithmic"

### Step 8: High-Level Grain Spawn API
**Goal:** Single predicate to spawn fully-configured grain

**Function:**
```prolog
sampler_grain_spawn(+DataHandle, +StartFrame, +DurationFrames, +Pitch, +Volume, +Pan, -GrainHandle)
```

**Implementation:**
```prolog
sampler_grain_spawn(Data, Start, Duration, Pitch, Vol, Pan, Handle) :-
    sampler_sound_create(Data, Handle),
    sampler_sound_seek(Handle, Start),
    sampler_sound_set_pitch(Handle, Pitch),
    sampler_sound_set_volume(Handle, Vol),
    sampler_sound_set_pan(Handle, Pan),
    sampler_sound_set_stop_time(Handle, Duration),
    sampler_sound_enable_envelope(Handle),  % if configured
    sampler_sound_start(Handle).
```

**Variant with Envelope:**
```prolog
sampler_grain_spawn_env(+DataHandle, +StartFrame, +DurationFrames,
                        +Pitch, +Volume, +Pan,
                        +EnvParams, -GrainHandle)
```

**Auto-Management Variant:**
```prolog
sampler_grain_fire(+DataHandle, +StartFrame, +DurationFrames, +Pitch, +Volume, +Pan)
```
- Spawns grain, returns immediately (no handle)
- Grain automatically cleaned up when finished
- For "fire and forget" grains

**Testing Plan:**
- Spawn single grain with all parameters
- Spawn 100 overlapping grains from same buffer
- Spawn 1000 grains sequentially (test slot reuse)
- Performance: measure grain spawn latency

### Step 9: Live Capture / Ring Buffer Support
**Goal:** Enable live audio capture and real-time granular processing

**Use Case:** Live granular synthesis - capture audio from input device, spawn grains from recent audio in real-time

**Design Decision: Snapshot Approach**
- Ring buffer continuously overwrites oldest data
- When spawning grain, copy (snapshot) frames from ring buffer to fixed data buffer
- Grain plays from stable snapshot, unaffected by ring buffer wrap-around
- Trade-off: Memory usage vs simplicity (acceptable for this use case)

**Ring Buffer Architecture:**
```c
typedef struct {
    ma_pcm_rb ring_buffer;          // miniaudio lock-free ring buffer
    ma_device capture_device;       // capture device
    ma_format format;
    ma_uint32 channels;
    ma_uint32 sample_rate;
    ma_uint64 buffer_size_frames;   // total ring buffer capacity
    ma_uint64 write_position;       // logical position (monotonic, for tracking)
    ma_bool32 active;
} capture_ring_buffer_t;
```

**Functions to Implement:**

1. **Start Capture**
   ```c
   sampler_capture_start(-CaptureHandle, +BufferSizeFrames)
   ```
   - Allocates ring buffer with specified size
   - Initializes capture device (ma_device_type_capture)
   - Starts recording audio into ring buffer
   - Returns handle for subsequent operations

2. **Stop Capture**
   ```c
   sampler_capture_stop(+CaptureHandle)
   ```
   - Stops capture device
   - Frees ring buffer resources
   - Does not affect already-extracted data buffers

3. **Extract Frames (Snapshot)**
   ```c
   sampler_capture_extract(+CaptureHandle, +StartFrame, +LengthFrames, -DataHandle)
   ```
   - Copies frames from ring buffer to new fixed data buffer
   - StartFrame is relative to current write position (0 = most recent, negative = go back in time)
   - Or: StartFrame is absolute from capture start (check if still available)
   - Returns standard data handle that can be used with sampler_sound_create
   - Handles wrap-around internally
   - Returns error if requested frames no longer available (overwritten)

4. **Query Status**
   ```c
   sampler_capture_get_info(+CaptureHandle, -Info)
   ```
   - Returns info(current_position, buffer_size, available_frames, format, channels, sample_rate)
   - Enables Prolog to know what frames are available for extraction

**Integration with Grain System:**
```prolog
% Start capture with 10 second buffer at 48kHz = 480000 frames
sampler_capture_start(Cap, 480000),

% Extract last 2 seconds of audio
sampler_capture_extract(Cap, -96000, 96000, Data),

% Spawn grains from extracted data as usual
sampler_grain_spawn(Data, StartFrame, Duration, Pitch, Vol, Pan, Grain),

% Clean up
sampler_data_unload(Data),
sampler_capture_stop(Cap).
```

**Thread Safety:**
- Ring buffer is lock-free (single producer = audio callback, single consumer = extract function)
- Capture device callback writes to ring buffer
- Extract function reads from ring buffer (safely)
- Prolog thread calls extract, not audio thread

**Implementation Details:**
```c
// Capture callback (runs in audio thread)
void capture_callback(ma_device* pDevice, void* pOutput, const void* pInput, ma_uint32 frameCount)
{
    capture_ring_buffer_t* capture = (capture_ring_buffer_t*)pDevice->pUserData;
    void* pWriteBuffer;
    ma_uint32 framesWritten;

    // Acquire write space in ring buffer
    ma_pcm_rb_acquire_write(&capture->ring_buffer, &frameCount, &pWriteBuffer);

    // Copy from input to ring buffer
    ma_copy_pcm_frames(pWriteBuffer, pInput, frameCount, capture->format, capture->channels);

    // Commit write
    ma_pcm_rb_commit_write(&capture->ring_buffer, frameCount);

    // Update logical position
    capture->write_position += frameCount;
}

// Extract function (called from Prolog thread)
// Copies frames from ring buffer to heap-allocated buffer
// Returns standard data handle
```

**Buffer Size Considerations:**
- Larger buffer = more audio history = more flexibility for grain extraction
- Typical: 5-30 seconds at sample rate (240k-1.44M frames at 48kHz)
- Must be larger than longest grain duration + extraction latency

**Error Handling:**
- Extraction request for frames no longer in buffer returns error
- Extraction request for future frames (not yet captured) returns error
- Ring buffer overflow: drop oldest data, log warning

**Testing Plan:**
- Start capture, let record for N seconds, verify buffer fills
- Extract frames, verify content matches what was captured
- Test wrap-around: let buffer wrap, extract across boundary
- Test overrun: request frames that were overwritten
- Spawn grains from live capture, verify playback correctness
- Performance: measure extraction latency

### Step 10: Reverb Integration
**Goal:** High-quality reverb effect for processed output

**Reverb Selection Criteria:**
- Open source, permissive license (MIT, BSD, public domain)
- Written in C or easily portable
- Reasonable CPU usage for real-time
- Good sound quality

**Candidate Algorithms:**

1. **FreeVerb (Jezar at Dreampoint)**
   - Public domain
   - Classic Schroeder reverb with Comb + Allpass filters
   - Simple, well-documented
   - C++ but straightforward to port

2. **Dattorro Plate Reverb**
   - Based on "Effect Design Part 1" paper
   - What Mutable Instruments Clouds uses
   - High quality, complex
   - Would need clean-room C implementation

3. **Simple Algorithmic Reverb**
   - Multiple comb filters + allpass
   - Implement from scratch based on textbook algorithm
   - Full control, no dependencies

4. **miniaudio Community Contributions**
   - Check if anyone has contributed reverb node implementations

**Recommended Starting Point:** FreeVerb port
- Well-understood algorithm
- Good balance of quality vs complexity
- Can always upgrade later

**Integration Architecture:**

Option A: Engine-Level Effect (Send/Return)
```c
// Process all sounds → reverb → output
ma_engine → [mix all sounds] → reverb_node → output_device
```

Option B: Per-Sound Effect
```c
// Each sound can have independent reverb
sound → reverb_node → engine
```

Recommend Option A for efficiency (one reverb instance)

**Implementation Plan:**
1. Port/implement reverb algorithm in C
2. Create miniaudio effect node interface
3. Integrate into engine node graph after mixer
4. Add control predicates:
   ```prolog
   sampler_reverb_enable/0
   sampler_reverb_disable/0
   sampler_reverb_set_wet/1      % 0.0 to 1.0
   sampler_reverb_set_room_size/1
   sampler_reverb_set_damping/1
   sampler_reverb_set_width/1     % stereo width
   ```

**Miniaudio Node Graph Integration:**
```c
// Custom reverb node
typedef struct {
    ma_node_base base;
    // reverb state
    float wet_mix;
    // comb/allpass filters
} reverb_node;

// Implement vtable
ma_node_vtable reverb_vtable = {
    reverb_process_pcm_frames,
    // ...
};

// Insert into graph
ma_node_attach_output_bus(&engine_endpoint, 0, &reverb_node, 0);
ma_node_attach_output_bus(&reverb_node, 0, &device, 0);
```

**Testing Plan:**
- Verify dry signal passes through unchanged at 0% wet
- Test with single grain, multiple grains, dense grain cloud
- Listen for artifacts, metallic ringing, or excessive CPU usage
- Compare to reference reverbs for quality

## Additional Considerations

### Step 11+: Future Enhancements
- **Musical Scales/Modes:**
  - Define musical scales (major, minor, pentatonic, chromatic, etc.)
  - Mode selection (Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian)
  - Root note/key selection
  - Quantize random pitch values to scale degrees
  - Support multiple simultaneous scales for polymodal generation
  - Enables musical/tonal grain clouds instead of arbitrary pitches
- **Delay Effect:** Implement delay line with feedback
- **Filter Effects:** Low-pass, high-pass, band-pass using ma_biquad
- **Granular-Specific Features:**
  - Random grain parameter variation (±pitch, ±start position)
  - Grain density control (grains per second)
  - Grain clouds (spawn multiple grains in burst)
  - Spatial distribution (random pan per grain)
- **Performance Monitoring:**
  - CPU usage tracking
  - Active grain count
  - Buffer pool utilization
- **Persistence:**
  - Save/load grain presets
  - Record grain sequences

### Performance Targets
- Support 100+ concurrent grains without glitches
- Grain spawn latency < 1ms
- CPU usage < 25% on modern hardware

### Testing Strategy
- **Unit Tests:** Each function in isolation
- **Integration Tests:** Multi-step workflows
- **Stress Tests:** MAX_SOUNDS concurrent grains, rapid spawning
- **Audio Quality:** Listen tests, no clicks/pops, smooth envelopes
- **Memory:** No leaks, verify refcounting

### Code Organization
```
promini/
├── include/
│   └── miniaudio.h           # v0.11.23
├── lib/
│   └── sampler.dylib         # compiled library
├── src/
│   ├── c/
│   │   ├── sampler.c         # main implementation
│   │   ├── envelope.c        # envelope generators (Step 7)
│   │   ├── capture.c         # live capture / ring buffer (Step 9)
│   │   └── reverb.c          # reverb implementation (Step 10)
│   └── prolog/
│       ├── sampler.pro       # API exports
│       └── granular.pro      # high-level grain utilities (Step 8)
├── audio/                    # test samples
├── examples/                 # example Prolog programs
└── Makefile
```

### Naming Conventions
- Underscores only, no camelCase
- Prolog predicates: `sampler_*`, `grain_*`
- C functions: `pl_sampler_*` (Prolog callable), `ma_*` (miniaudio API)
- Macros: UPPERCASE_WITH_UNDERSCORES

### Documentation Needs
- API reference with examples
- Granular synthesis primer
- Architecture diagrams
- Performance benchmarks
- Audio examples/demos

## Summary
Current state: Steps 1-4 complete (engine, playback, polyphony, sound parameters)
Remaining: Steps 5-10 (reverse playback, grain lifecycle, envelopes, high-level API, live capture, reverb)
End goal: Production-ready granular sampler with live input capabilities and rich sound processing
