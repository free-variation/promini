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
  - reverb (Dattorro with shimmer, in-loop shimmer, freeze) ✓
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
- comprehensive modulation system ✓
  - sources:
    - LFO ✓
    - looping envelope ✓
    - gamepad axes (sticks, triggers, d-pad) ✓
    - gamepad buttons (cycling, momentary, trigger, toggle) ✓
    - noise (with sample & hold) ✓
    - keyboard (velocity, note) ✓
    - audio buffers *(planned)*
  - targets: oscillator, filter, VCA, pan, delay, granular, reverb ✓
- control interface using gamepad ✓, keyboard ✓ (polyphonic synth support with voice stealing), trackpad *(planned)*
- SDL visualizer ✓
  - waveform, spectrum, spectrogram modes ✓
  - attach to any audio source ✓
  - multiple windows simultaneously ✓
  - auto-ranging, theme support, FFT size selection ✓
- patches, presets, and setups saved as transparent prolog clauses ✓
- sequencers *(planned)*
  - Turing Machine (shift register pitch sequencer)
  - Euclidean (Bjorklund rhythm generator)
- MCP integration for AI-assisted patch design and composition *(planned)*

MIT License
