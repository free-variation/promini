# Session State - Live Capture Complete

## Current Status

**Completed Steps:**
- Steps 1-8 of plan.md fully implemented
- All basic sampler functionality working
- Effects system complete: bitcrush, envelope with full management
- Live capture with circular buffer and snapshot extraction
- 41 tests passing

## Step 8: Live Capture with Ring Buffer (Completed)

**Architecture:**
- Circular buffer with continuous overwriting (no read pointer tracking)
- Lock-free audio thread callback writes captured audio
- Snapshot extraction copies frames to linear data buffers
- Supports up to 8 simultaneous capture devices (MAX_CAPTURE_DEVICES)

**Implementation:**
- `capture_data_callback()` - Audio thread writes to circular buffer, handles wraparound
- `pl_sampler_capture_start()` - Initialize device and allocate buffer memory
- `pl_sampler_capture_stop()` - Stop device and cleanup
- `pl_sampler_capture_get_info()` - Returns write_position, capacity, sample_rate
- `pl_sampler_capture_extract()` - Snapshot frames from buffer to new data buffer
- `GET_CAPTURE_DEVICE_FROM_HANDLE` macro - Validates capture handles

**Buffer Design:**
- Period-based sizing: user specifies seconds, function returns calculated frame count
- Simple circular buffer: `write_position % capacity` for wraparound
- No synchronization between audio/Prolog threads (accepted race for granular use)
- Extraction handles wraparound: copies in two memcpy calls if needed

**Prolog Interface:**
```prolog
sampler_capture_start(+DeviceName, +PeriodSeconds, -CaptureHandle, -BufferFrames)
sampler_capture_stop(+CaptureHandle)
sampler_capture_get_info(+CaptureHandle, -capture_info(WritePos, Capacity, SampleRate))
sampler_capture_extract(+CaptureHandle, +NegativeOffset, +Length, -DataHandle)
```

**Key Design Decisions:**
- Extraction over direct reading: Simpler, handles wraparound, freeze-frame semantics
- No ma_pcm_rb: Custom circular buffer, no read pointer, continuous overwrite
- Negative offsets: Extract N frames behind write head (e.g., -48000 = 1 second ago at 48kHz)
- Frames not time: Consistent with rest of API, user can convert from seconds

**Files Modified:**
- `src/c/sampler.c` - Lines ~170-185 (capture_slot_t structure), ~245-300 (slot management), ~1440-1470 (callback), capture functions, install/uninstall updates
- `src/prolog/sampler.pro` - Exported 4 capture predicates
- `test/sampler.pro` - Added sampler_capture test suite (4 tests)
- `CLAUDE.MD` - Added code style guidelines (naming conventions)

## Test Status

All 41 tests passing:
- sampler_init: 3 tests
- sampler_data: 5 tests
- sampler_sound: 3 tests
- sampler_playback: 4 tests
- sampler_parameters: 4 tests
- sampler_range: 1 test
- sampler_effects: 6 tests
- sampler_polyphony: 1 test
- effect_setters: 4 tests
- effect_detach: 4 tests
- effect_clear: 2 tests
- sampler_capture: 4 tests (start/stop, get_info, extract, extract_wraparound)

## Next Step: Step 9 - Expose Existing miniaudio Filters

Per plan.md lines 240-269:
- Wrap miniaudio's built-in filters as per-sound effect nodes
- Following bitcrush/envelope pattern
- Filters: biquad, low-pass, high-pass, band-pass, notch, peaking EQ, shelving
- Common parameters: cutoff/center frequencies, filter order, Q factor, gain
- Dynamic parameter updates via `sampler_effect_set_parameters`

## Technical Notes

**Effect Chain Architecture:**
- Linked list of effect_node_t structs per sound
- Each node contains: type enum, ma_node_base pointer, next pointer
- Audio graph routing: sound → effect1 → effect2 → ... → engine endpoint
- Detachment reconnects remaining chain automatically

**Capture Buffer Architecture:**
- Global array: `capture_slot_t g_capture_devices[MAX_CAPTURE_DEVICES]`
- Each slot: ma_device, buffer_data pointer, capacity_frames, write_position, in_use flag
- Audio callback: `write_pos = write_position % capacity`, memcpy with wraparound check
- Extraction: Calculate read position from negative offset, handle wraparound, create data buffer

**Memory Management:**
- Capture buffers: Allocated at start (period * sample_rate * bytes_per_frame), freed at stop
- Extracted data: Separate malloc, owned by data buffer system, refcounted
- Device cleanup: ma_device_uninit in free_capture_slot, called from stop or uninstall

**Race Condition Analysis:**
- Write position: No atomics (ma_uint64 read/write assumed atomic on x86-64/ARM64)
- Frame data: Torn reads possible if extraction overlaps write
- Impact: Brief glitch in one grain among hundreds, inaudible in granular context
- Decision: Accept race, no synchronization overhead
