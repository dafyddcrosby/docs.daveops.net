# LUKS

```bash
# Encrypt a partition
cryptsetup luksFormat /dev/sda2

# Mount partition
cryptsetup open /dev/sda2/ mapping_name
mount /dev/mapper/mapping_name /mount/dir
```
