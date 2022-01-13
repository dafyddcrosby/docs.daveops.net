# Rsync

## Use sudo on remote end

```
rsync -a -e "ssh" --rsync-path="/usr/bin/sudo /usr/bin/rsync" user@example.com:/remote_files/ local_dir/ 
```
