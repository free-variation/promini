# promini
A highly motivated music synthesis system built on [miniaudio](https://miniaud.io/), with Prolog as a control language.
Still in very early development.

The intention is to make it easy to reason about patches and sounds. 

Existing or planned features (all in stereo):
- real-time or sample granulation
- additive sythesis
- essential audio processors:
  - bitcrush
  - ping-pong delay
  - reverb (Dattorro)
  - panning
  - crossfading
  - 4-pole ladder filter (D'Angelo & Välimäki)
  - plus miniaudio's effects:
    - delay
    - various filter types
- image-to-audio additive sythesis
- comprehensive modulation system
  - sources:
    - LFO
    - looping envelope
    - noise
    - audio buffers
    - images
  - targets: many parameters across the system, with S&H
- control interface using gamepad, keyboard, trackpad
- patches, presets, and setups saved as transparent prolog queries.
