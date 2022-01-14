# FFmpeg

## Encode a video from WAV and PNG

```bash
ffmpeg -loop 1 -r 2 -i input.png -i audio.ogg -c:v libx264 -preset medium -tune stillimage -crf 18 -c:a copy -shortest -pix_fmt yuv420p output.mkv
```

## Convert MOV to GIF

```bash
ffmpeg -i file.mov -pix_fmt rgb24 output.gif
```

## Do a screencast

```bash
ffmpeg -f x11grab -r 25 -s 1280x800 -i :0.0 /tmp/outputFile.mpg
```

## Extract audio from a video file

```bash
ffmpeg -i video.mp4 -vn -acodec copy audio.aac
```
