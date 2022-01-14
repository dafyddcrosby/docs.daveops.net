# LVM

## Cheatsheet

```bash
# Initialize a disk for LVM
pvcreate PHYSICAL_VOLUME

# Create a volume group
vgcreate GROUP_NAME DISKS...

# List groups
vgs

# Create a logical volume with 100% free space
lvcreate -l 100%free -n VOLUME_NAME GROUP_NAME
```

## Links

- <https://access.redhat.com/documentation/en-US/Red_Hat_Enterprise_Linux/7/html/Logical_Volume_Manager_Administration/LVM_examples.html>
