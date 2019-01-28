---
title: GRUB
---

Except where noted, this is for GRUB 2+

# Console

```bash
# Turn on the pager in console mode
set pager=1
# See what the variables are
set
# Find devices available
ls
```

# Grub grub.cfg creation

```bash
# RHEL-ish
grub2-mkconfig -o /boot/grub2/grub.cfg
# Deb-ish
update-grub
```

# GRUB 1

* `/boot/grub/grub.conf`

