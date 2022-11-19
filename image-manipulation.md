# Image Manipulation


# ImageMagick


## Create a PDF from image files

```shell
# single image per page
convert a.jpg b.jpg ab.pdf
# 2x2 tiled, landscape
montage *jpg -adjoin -border 0 -tile 2x2 -rotate 90 -page letter -geometry +2+2 example.pdf
```


# Exiftool


## Remove metadata from JPEG

```shell
exiftool -all= example.jpg
```


## Modify PDF metadata

```shell
exiftool file.pdf -Title="Animal Farm" -Author="George Orwell"
```


# ASCII art


## Convert JPG to ASCII art

```shell
jp2a -b --colors --fill
```