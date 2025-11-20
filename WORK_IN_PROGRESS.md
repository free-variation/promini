# Work in Progress - ADBR Envelope Implementation

## Current Status

Implementing Step 7 from plan.md: ADBR Envelope Generator for per-sound amplitude envelopes.

### Completed

1. **Bitcrush effect** (Step 6) - COMPLETE
   - Structure: `bitcrush_node_t` with bit depth and sample rate reduction
   - NEON-optimized processing callback
   - Node graph integration via `attach_effect_node_to_sound()`
   - Prolog interface: `sampler_sound_attach_bitcrush/3`
   - Helper: `init_effect_node_base()` for common node initialization boilerplate
   - Tests: All unit tests passing, interactive demo working

2. **ADBR envelope structure** - DEFINED
   - Location: src/c/sampler.c lines 166-176
   - Type: `adbr_envelope_node_t`
   - Fields:
     - `float attack, decay, brek, release` - proportional times (not absolute)
     - `float break_level` - target amplitude for break stage (0.0-1.0)
     - `float duration_ms` - total envelope duration in milliseconds
     - `ma_uint32 stage` - current stage (0=attack, 1=decay, 2=break, 3=release)
     - `float stage_progress` - 0.0-1.0 progress through current stage

3. **ADBR vtable** - DECLARED
   - Location: src/c/sampler.c lines 178-185
   - Name: `adbr_envelope_vtable`
   - References `adbr_process_pcm_frames` (NOT YET IMPLEMENTED)

4. **Init function stub** - STARTED
   - Location: src/c/sampler.c lines 1357-1367
   - Function: `init_adbr_node()`
   - Uses `init_effect_node_base()` helper
   - INCOMPLETE: missing parameter assignments and return statement

### Next Steps

1. **Implement `adbr_process_pcm_frames()` callback**
   - Must be placed BEFORE vtable declaration (before line 178)
   - Calculate envelope value per frame based on stage and stage_progress
   - Multiply audio samples by envelope value (use NEON optimization)
   - Stage transitions:
     - Attack: 0.0 → 1.0
     - Decay: 1.0 → break_level
     - Break: hold at break_level
     - Release: break_level → 0.0
   - Progress calculation each frame: `increment = 1.0 / (duration_ms/1000 * proportion * sample_rate)`

2. **Complete `init_adbr_node()` function**
   - Assign parameters to node fields
   - Initialize stage = 0, stage_progress = 0.0
   - Return MA_SUCCESS

3. **Implement `attach_adbr_envelope_effect()` function**
   - Parse parameters from Prolog term list
   - Validate parameters
   - Allocate and initialize envelope node
   - Call `attach_effect_node_to_sound()`
   - Handle cleanup on error

4. **Add to `pl_sampler_sound_attach_effect()` dispatcher**
   - Add `else if (strcmp(type_str, "envelope") == 0)`
   - Call `attach_adbr_envelope_effect()`

5. **Prolog wrapper**
   - Add to sampler.pro exports
   - Create `sampler_sound_attach_envelope/6` wrapper

6. **Tests**
   - Unit tests in test/sampler.pro
   - Interactive demo for hearing envelope on grains

### Design Decisions

**Envelope shape representation:**
- Proportional values (attack/decay/break/release) define the shape
- `duration_ms` scales the entire envelope
- Inspired by Dreadbox Typhon time scaling (0.1x to 6x)
- No pre-computed frame counts - calculated per-frame from proportions
- No stored sample_rate - queried from engine when needed
- No stored current_value - computed locally in callback

**NEON optimization:**
- Envelope state updates: scalar (sequential, per-frame)
- Sample multiplication: NEON vectorized (broadcast envelope value, multiply 4 samples at once)

### Known Issues

1. Line 1360: Parameter name `delay` should be `decay`
2. `adbr_process_pcm_frames` referenced but not implemented
3. `init_adbr_node` incomplete

### Files Modified

- src/c/sampler.c - envelope structure, vtable, init stub
- src/prolog/sampler.pro - (not yet)
- test/sampler.pro - (not yet)
- examples/ - (not yet)
