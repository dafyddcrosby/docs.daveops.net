---
title: btrfs
tags: filesystems
---

## snapshots

```bash
# Create a read-only snapshot
btrfs subvolume snapshot -r <mount> <dest>

# Send snapshot to external drive
btrfs send <snap> | btrfs receive <device>
```
