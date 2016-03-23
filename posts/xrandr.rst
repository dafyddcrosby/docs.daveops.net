xrandr
======
:tags: X Windows

Make PAL card output NTSC
-------------------------
After the first two lines in ``/etc/gdm/Init/Default`` , put:

::

  xrandr --output S-video --set “tv standard” ntsc

