# video-server

## What is it?

### as a short description

Software which lets you creatively play back, synthesise and manipulate video
material in real-time using a live-coding programming language and/or external
controllers (e.g. MIDI controllers).

When controlled using
[SuperCollider-AV](https://github.com/r-gr/supercollider-av), it forms a
powerful audiovisual composition system by extending SuperCollider with video
capabilities.

### by vague comparison

If SuperCollider is like Max/MSP but with textual rather than graphical
programming, then this is a bit like the video playback and processing parts of
Jitter.

### in technical terms

A standalone program which receives messages over IPC or TCP sockets telling the
program what video synthesis, video playback, or video transformation operations
to perform, which it outputs to a window. The parameters of these operations can
be received at a very high rate with low latency, meaning that audio signals (or
other high rate data sources) can be used to control the parameters.

It uses the FFmpeg library to read a large range of video formats and uses
OpenGL to utilise GPU hardware to perform video operations.

## Why is it?

It exists

- for live creative video processing, audiovisual composition, music and data
  visualisation;
- as an alternative to other software which exists in a similar space, such as
  parts of Cycling '74's Max/MSP/Jitter;
- as a fun, live, interactive alternative to the slow, offline workflows of
  non-real-time video editors;
- as an experiment in using a live-coding and textual programming interface for
  creative video processing.

## How is it?

It's doing well, thanks.

## What is it? The full technical description.

A real-time video synthesis and processing system with a conceptually equivalent
role to the SuperCollider platform's audio server (scsynth/supernova). It
follows SuperCollider's model of UGen graphs defining synthesis/transformation
operations and is designed to be controlled via SuperCollider's live-coding
programming language _sclang_, currently using a modified version of
SuperCollider, [SuperCollider-AV](https://github.com/r-gr/supercollider-av).

It does not depend on SuperCollider, however, and can receive messages in an
appropriate format from any source (over IPC or TCP sockets).

The video server receives messages defining video synthesis and transformation
operations as a graph of UGens. As with SuperCollider's audio processing UGens,
the parameters of the video UGens can be controlled by either static values or a
stream of values/signals, such as a control rate signal in SuperCollider. In
SuperCollider-AV, you can control audio synthesis parameters with, for example,
the same LFO you're using to control some video parameter.

The system has very low latency so there is a very tight perceptual link between
the output you hear and the output you see.

It has really solid performance, even for an unreasonable number of
transformations applied to a video. Granted, none of the currently implemented
transformations are particularly computationally expensive, but I haven't had
the output drop below 60fps on my laptop (Intel i7, integrated HD Graphics 520),
even with hundreds of audio streams controlling hundreds of video transformation
parameters.

The video server supports the playback of videos from disk in a number of
formats by utilising the FFmpeg library. It supports looping playback of videos
and playback at variable rates -- it can handle playback at very high or very
low rates but does not attempt to do any form of frame blending at low rates.

My favourite video playback feature is the optional ability to load video files
into memory, which allows a video's frames to be accessed randomly with a freely
controllable 'playback head'. Controlling the playback head with an increasing
ramp waveform results in normal (forwards, linear) playback, a decreasing ramp
gives reverse playback, and beyond that other waveforms can play the frames in
any way you want: playing the frames in a random order, speed ramps, etc.

The UGens are implemented as GLSL (OpenGL Shading Language) fragment shader
functions so the existing set of UGens can be easily extended. When using
SuperCollider-AV to control the video server, corresponding UGen classes exist
in sclang.
