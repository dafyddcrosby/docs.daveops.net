========
mencoder
========


Join multiple videos
==============================
{{{
cat movie1.avi movie2.avi > movie.avi
mencoder -forceidx -oac copy -ovc copy movie.avi -o movie_fixed.avi
}}}

