# File Archiving


# Tar

```shell
# Clean a tar-bombed directory
tar -tf <file.tar.gz> | xargs rm -r

# Remove VCS junk
tar --exclude-vcs -cf src.tar src/

# Creating a gzipped tarball
tar czfv tobemade.tar.gz tobezipped/

# Extract tar.gz
tar zxvf [file]

# Extract tar.bz2
tar jxvf [file]

# Extract to a directory
tar ... -C /send/to/
```


# cpio

- Not POSIX 1996, but 2001
- predates tar!

| flag | description  |
|---- |------------ |
| -i   | input        |
| -o   | output       |
| -p   | pass-through |

[Diffoscope home page](https://diffoscope.org/)

[Restic docs](https://restic.readthedocs.io/en/stable/index.html)

```shell
# Initialize the repo
restic init -r $REPO_PATH

# Run a backup
restic backup -r $REPO_PATH $PATH_TO_BACKUP

# List snapshots
restic snapshots -r $REPO_PATH
```
