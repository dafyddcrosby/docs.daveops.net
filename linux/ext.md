---
title: ext filesystem
slug: ext
---

* ext2 - no journaling, but good for small solid-state drives
* ext3 - journaling - can be upgraded from ext2 with no data loss
* ext4 - support for large disks/file sizes

Superblock at the start, has info on filesystem. Groupblock holds information on subsections of the space, as well as superblock backups. Inode tables hold the file metadata.

```bash
# create label for the filesystem
e2label /dev/sda2 mylabel
# see label for filesystem
e2label /dev/sda2

# Add journal to ext2 filesystem
tune2fs -j /dev/sda2
# specify percentage that's reserved by root (by default 5%)
tune2fs -m1 /dev/sda2
# Disable automatic filesystem checking
tune2fs -c0 -i0 /dev/sda2

# Get superblock info
dumpe2fs /dev/sda2

# Debug an ext2 filesystem
```
