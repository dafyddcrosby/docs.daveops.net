# Mount

```bash
# Mounting an ISO
mount -o loop disk1.iso /mnt/disk

# Remount a filesystem (change options without unmounting)
mount /home -o remount, noatime

# Create a RAM disk
mount -t tmpfs -o size=1g tmpfs /mnt
```

## Lazy unmount of a partition

Linux 2.4.11+

```bash
umount -l <mount>
```

## fuser

```bash
# See what processes are using /mnt
fuser -v /mnt

# Kill processes using /mnt
fuser -k -KILL /mnt
```

## UUID

```bash
# find a filesystem
findfs UUID=...
# list filesystems
blkid
```
