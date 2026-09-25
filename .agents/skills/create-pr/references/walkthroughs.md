# Narrated PR walkthroughs

Create a short video with voice-over and synchronized subtitles when UI, CLI
or backend changes have meaningful behavior to demonstrate. Documentation and
skill changes need a video only when an actual workflow example helps explain
them. A scroll through the diff alone is usually insufficient. Reuse suitable
current recordings instead of repeating work.

## Discover capabilities on this machine

Check the OS, graphical or headless session, available tools and their actual
capabilities. Discover recording, speech synthesis, audio-duration inspection,
video assembly, subtitle rendering and attachment support separately. A screen
recorder alone cannot necessarily combine narration or render captions.

Use existing local tools or an already-authorized configured provider. Do not
assume a particular recorder, voice, model, cloud subscription or OS. Examples
to inspect, not mandatory dependencies:

| Host | Capture candidates | Narration candidates |
| --- | --- | --- |
| macOS | `screencapture`, installed recorder | `say`, installed speech engine |
| Linux | Installed recorder; supported X11 or Wayland capture | Installed eSpeak NG or another speech engine |
| Windows | Installed recorder; supported Windows capture | Available Windows speech engine or another installed synthesizer |

FFmpeg can capture and assemble video when its installed build exposes the
needed devices, codecs and filters. X11 capture does not imply Wayland support.
Browser screenshots are not video; an asciinema cast needs playback capture or
a supported renderer to become a video file. In a headless session, use an
available headless recorder or renderer without assuming a desktop exists.
Consult current help or primary documentation for the chosen tools:
[FFmpeg devices](https://www.ffmpeg.org/ffmpeg-devices.html),
[Wayland ScreenCast portal](https://flatpak.github.io/xdg-desktop-portal/docs/doc-org.freedesktop.portal.ScreenCast.html),
[Windows speech output](https://learn.microsoft.com/en-us/dotnet/api/system.speech.synthesis.speechsynthesizer.setoutputtowavefile),
[eSpeak NG](https://github.com/espeak-ng/espeak-ng/blob/master/docs/guide.md).

Record the host and capture backend actually exercised. [Wine](https://www.winehq.org/about)
can help check whether a Windows media tool runs on the current host, including
encoding or caption assembly from existing inputs. That result establishes
compatibility under Wine, not native Windows capture or speech support. Verify
[Windows capture](https://learn.microsoft.com/en-us/windows/apps/develop/media-authoring-processing/screen-capture)
in an actual Windows session with a supported graphics device; a Windows VM can
cover its virtual display, while physical capture devices need their own check.
Wayland capture needs a Wayland session and its capture backend or portal.

## Produce and verify the walkthrough

- Choose a representative, already-validated scenario showing the trigger,
  action and changed result. Use an authorized demonstration environment with
  safe example data. Capture the relevant application or region, excluding
  unrelated windows, notifications and credentials.
- Write brief narration segments explaining the demonstrated behavior. Use a
  suitable installed voice by default. Generate captions from the same script
  and time them to the measured audio and demonstrated actions; transcription
  is unnecessary for a scripted voice-over.
- Assemble the actual recording, narration and readable subtitles. Prefer
  subtitles rendered into the video for consistent playback; optional SRT or
  WebVTT files can accompany it. Rely on selectable tracks only when the target
  player supports them. Verify narration matches what the viewer sees. Do not
  present staged or synthetic examples as execution evidence or edited playback
  timing as a performance measurement.
- Review the finished playback for legible text, intelligible audio, accurate
  captions, synchronization and the claimed result. Check the actual exported
  file; an export command succeeding does not establish playback quality.
- Upload through the repository's supported attachment path and verify the
  resulting PR asset. Keep media out of source control. A locally created file
  is not an uploaded, reviewer-accessible walkthrough.

## Report incomplete requirements without blocking the PR

If capture, voice-over, subtitles, assembly, playback verification or upload
cannot be completed, continue the otherwise-authorized PR workflow. Existing
review, behavior and CI requirements remain in force; a media tooling gap alone
does not keep a PR draft.

In the final user handoff, name each incomplete requirement, the observed
reason and the concrete remedy for this OS, such as a supported tool to install,
a permission to enable or an upload capability to configure. Verify suggested
setup instructions against current tool documentation. Include local artifact
paths and completed stages so the user can resume without repeating work.
Identify missing media briefly in the PR where a comparison or walkthrough was
expected. Do not silently substitute a silent clip for a narrated, subtitled
video or describe incomplete media as finished. Avoid repeated failed setup
attempts; unsupported tooling is a reported gap, not a new implementation task.
