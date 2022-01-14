# swap

Rule of thumb is usually twice physical RAM, but not strictly necessary. You do get a benefit of the kernel putting crash dumps into it.

```bash
# convert partition to swap space
mkswap /dev/sda2

# enable the partition swap
swapon /dev/sda2

# show current swap space
swapon -s
```
