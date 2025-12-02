# promini
A motivated music synthesis system build on miniaudio, with Prolog as a control language.
Still in very early development.

Existing or planned features (all in stereo):
- real-time or sample granulation
- additive sythesis
- essential audio processors:
  - bitcrush
  - ping-pong delay
  - reverb
  - panning
  - crossfading
  - 4-pole ladder filter
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
