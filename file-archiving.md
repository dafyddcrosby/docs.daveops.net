# File Archiving

# Tar

## Clean a tar-bombed directory

```
tar -tf <file.tar.gz> | xargs rm -r
```

## Remove VCS junk

```
tar --exclude-vcs -cf src.tar src/
```

## Creating a gzipped tarball

```
# Zip a directory
tar czfv tobemade.tar.gz tobezipped/
```

## Extracting tarballs

```
# tar.gz
tar zxvf [file]

# tar.bz2
tar jxvf [file]
```

## Extract to a directory

```
tar ... -C /send/to/
```


# cpio
Not POSIX 1996, but 2001
* predates tar!


flag | description
---  | ---
-i   | input
-o   | output
-p   | pass-through

```
# Copy-out mode
