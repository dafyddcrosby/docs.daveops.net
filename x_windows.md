---
title: X Windows
---

# X Windows

* [X and NeWS history](http://minnie.tuhs.org/pipermail/tuhs/2017-September/010471.html)



[XTerm](./terminal-emulation.md#xterm)



# xrandr
@X_Windows

Make PAL card output NTSC
-------------------------
After the first two lines in ``/etc/gdm/Init/Default`` , put:

	xrandr --output S-video --set “tv standard” ntsc

