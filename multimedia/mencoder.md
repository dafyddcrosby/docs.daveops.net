# mencoder
## Join multiple videos

```
cat movie1.avi movie2.avi > movie.avi
mencoder -forceidx -oac copy -ovc copy movie.avi -o movie_fixed.avi
```

## Get specific time
Use -ss to seek to a specific part of the file you want.

```
-ss 00:30:00 -endpos 00:00:05
```


## Get codecs

```bash
# audio codecs
mencoder -oac help
# video codecs
mencoder -ovc help
```

## Subtitles

```
 -sub FILE
 -subdelay
```

