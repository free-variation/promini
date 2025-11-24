# Synthesizer Design Plan

## Overview
Additive synthesizer using sine waves for experimental music, integrated with the sampler system.

## Voice Architecture
- Each voice contains one sine wave oscillator
- Parameters: frequency, phase (0.0-1.0), pan (-1.0 to 1.0)
- Initial amplitude at maximum
- Clean, unfiltered output (no 3D HRTF spatialization)
- Uses simple stereo panning

## Modulation
### Amplitude
- Use existing ADBR envelope effect from effects.c
- Voices are ma_sound instances, so effects attach like sampler sounds

### Other Parameters
- Sample-and-hold modulator concept
- Can modulate: frequency, phase, pan
- Modulation sources:
  - Waveforms (LFOs)
  - Noise generators (white, pink, Brownian)
  - Sampled audio (audio as control signal)
- Quantization via hold intervals

## Integration
- Shares global ma_engine with sampler
- Voices use ma_sound for playback
- Compatible with existing effect chain system
- Sampler audio can modulate synth parameters (experimental cross-modulation)

## Design Philosophy
- Precise control over harmonic content
- No unwanted filtering or coloration
- Support for experimental techniques
- Modular architecture (voices + effects + modulators)
