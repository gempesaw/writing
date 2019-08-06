# Uninstalling Mersive's Solstice Audio Driver from MacOS Mojave

This was a minor annoyance, but since my Google searches were
particularly unfruitful, I figured I'd make a note of it. Our WeWork
uses the Mersive Solstice client for broadcasting our devices to the
meeting room televisions. It has the option to install its own audio
driver to try to mirror the audio on its own.

The client's built-in audio driver is called "Desktop Audio Streaming
Device" and it shows up as an output and an input device. It also
completely failed to work at all. The only way I managed to get sound
on to the TV was by using AirPlay after connecting with the Solstice
client - basically using it as little as possible.

Uninstalling audio drivers is apparently something weird in OS X - the
device was listed in the `Audio Midi Setup` built-in macOS app, but
couldn't be uninstalled from the app. Some wild googling directed me
to `/Library/Audio/Plug-Ins/HAL/`, or `/Library/Preferences`, or their
`~/Library` counterparts. In the end, it was a `kext` in
`/System/Library/Extensions`: `SolsticeAudioDevice.kext`. Trashing
that file and restarting the computer fixed the issue.
