---
title: PHP Insecurity
---

PHP has more than a few security pitfalls, this is just a quick list of ways it
can bite you.

## $FILES tmp_name usage

tmp_name is untrusted input and should be sanitized before doing any file
operations (like `move_uploaded_file`)

## exif_imagetype is not validation

If you're testing a file to ensure it's an image, exif_imagetype alone is
inadequate, as it can be easily bypassed with a magic string like "BM" (for
bitmap)
