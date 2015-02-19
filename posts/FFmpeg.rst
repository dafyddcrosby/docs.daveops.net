FFmpeg
======
:date: 2015-02-19

Encode a video from WAV and PNG
-------------------------------
::

 ffmpeg -loop 1 -r 2 -i input.png -i audio.ogg -c:v libx264 -preset medium -tune stillimage -crf 18 -c:a copy -shortest -pix_fmt yuv420p output.mkv
