# promini
A highly motivated music synthesis system built on [miniaudio](https://miniaud.io/), with Prolog as a control language.
Still in very early development.

The intention is to make it easy to reason about patches and sounds. 

Existing or planned features (all in stereo):
- real-time or sample granulation *(planned)*
  - live audio capture ✓
  - sound/sample playback ✓
- additive synthesis (including noise oscillators) ✓
- essential audio processors:
  - bitcrush ✓
  - ping-pong delay ✓
  - reverb (Dattorro) ✓
  - panning ✓
  - VCA ✓
  - summing node ✓
  - crossfading *(planned)*
  - compressor *(planned)*
  - limiter *(planned)*
  - 4-pole ladder filter (D'Angelo & Välimäki) ✓
  - plus miniaudio's effects:
    - delay ✓
    - various filter types ✓
- image-to-audio additive synthesis *(planned)*
- comprehensive modulation system *(in progress)*
  - sources:
    - LFO ✓
    - looping envelope ✓
    - noise *(planned)*
    - audio buffers *(planned)*
    - images *(planned)*
  - targets: many parameters across the system, with S&H *(in progress)*
- control interface using gamepad, keyboard, trackpad *(planned)*
- patches, presets, and setups saved as transparent prolog clauses ✓
- MCP integration for AI-assisted patch design and composition *(planned)*

MIT License
