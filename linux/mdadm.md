---
title: mdadm
---

## Get details of RAID setup

```bash
mdadm --detail /dev/md0
cat /proc/mdstat
```

## Adjust the array

```bash
# Drop a disk from the array
/sbin/mdadm /dev/md0 --fail /dev/sda1 --remove /dev/sda1

# Add a disk to the array
/sbin/mdadm /dev/md0 --add /dev/sda1
```
