# File Transfer

# xmodem

## Using xmodem with macOS

```bash
brew install lrzsz
```

using GNU Screen, press ctrl-a, :, then type `exec !! lsz -X thing.bin`
# Rsync

## Use sudo on remote end

```
rsync -a -e "ssh" --rsync-path="/usr/bin/sudo /usr/bin/rsync" user@example.com:/remote_files/ local_dir/ 
```
