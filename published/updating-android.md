Humm, so my nexus 6 has a persistent notification about wanting me to
upgrade to 5.1.1. I was lazy this time and didn't bother doing it at
the end of May when the stock images came out, but my phone doesn't
know how to do it on its own since it's rooted and unencrypted and/or
it's using TWRP recovery instead of the stock recovery. I don't do
this frequently enough to remember, but too infrequently to want to
write a script for it (especially since Wug's toolkit already
exists). Anyway, here's my pretty straightforward steps to getting my
N6 to 5.1.1, generic enough to apply to any update[^1]:

0. start [downloading whatever factory image][dl] that needs flashing
1. on the phone, make a backup via TWRP
2. copy the backup to a desktop, just in case of lightning strikes!
3. whenever the DL finishes, unzip the `.tgz` and go in that folder
4. in that folder unzip the `.zip`, cuz it has `boot.img` and `system.img`
5. wire the phone to a computer (probably already done in 2)
6. reboot to bootloader (power off, hold vol down & power)
7. use `fastboot` to do some things:


        fastboot flash bootloader bootloader.img
        fastboot flash radio radio.img
        fastboot reboot-bootloader
        fastboot flash boot boot.img
        fastboot flash system system.img
        fastboot reboot-bootloader

8. head into TWRP recovery, supposedly I should wipe cache & Dalvik,
   but I forgot and it seems fine. anyway, reboot to system and it'll
   prompt about installing SU for us since we lost root (yay TWRP).

Make sure not to flash the recovery from the download since we'd like
to keep TWRP, not the stock recovery. TWRP's reboot into system will
spin the "optimizing app M of N" for a while, and then it should be
gravy!

[dl]: https://developers.google.com/android/nexus/images?hl=en

[^1]: I used to go to theSiteThatShallNotBeNamed for this, but after
seeing their recent behaviors and reading the thoughts of some
industry thought leaders (heh, heh, thought leaders is a funny
concept - but here I am following their thoughts...), I figure I'd
ought to make a reference for myself so I can stop giving them
traffic.
