---
title: ImageMagick
---

## Create a PDF from image files

```bash
# single image per page
convert a.jpg b.jpg ab.pdf
# 2x2 tiled, landscape
montage *jpg -adjoin -border 0 -tile 2x2 -rotate 90 -page letter -geometry +2+2 example.pdf
```
