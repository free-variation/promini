# promini
A highly motivated music synthesis system built on [miniaudio](https://miniaud.io/), with Prolog as a control language.
Still in very early development.

The intention is to make it easy to reason about patches and sounds. 

Existing or planned features (all in stereo):
- granular synthesis ✓
  - Beads-style position-based granulation ✓
  - live recording to ring buffer ✓
  - sound/sample loading ✓
  - tempo-synced grain triggering ✓
  - pitch quantization to scales/modes ✓
  - concatenative synthesis *(planned)*
- additive synthesis (including noise oscillators) ✓
- live audio capture ✓
- essential audio processors:
  - bitcrush ✓
  - ping-pong delay ✓
  - reverb (Dattorro with shimmer) ✓
  - panning ✓
  - VCA ✓
  - summing node ✓
  - crossfading *(planned)*
  - limiter ✓
  - 4-pole ladder filter (D'Angelo & Välimäki) ✓
  - plus miniaudio's effects:
    - delay ✓
    - various filter types ✓
- image-to-audio synthesis ✓
  - additive mode (rows as oscillators) ✓
  - waveform mode (row as wavetable) ✓
  - RGB stereo (R=left, G=center, B=right) ✓
- clock system ✓
  - BPM-based master clock ✓
  - routes to LFO, envelope, granular, delays ✓
  - pulse (trigger) and sync (tempo-lock) modes ✓
- comprehensive modulation system *(in progress)*
  - sources:
    - LFO ✓
    - looping envelope ✓
    - gamepad (axes, triggers, d-pad) ✓
    - noise *(planned)*
    - audio buffers *(planned)*
  - targets: many parameters across the system, with S&H *(in progress)*
- control interface using gamepad ✓, keyboard *(planned)*, trackpad *(planned)*
- patches, presets, and setups saved as transparent prolog clauses ✓
- MCP integration for AI-assisted patch design and composition *(planned)*

MIT License
