---
title: systemd/hostnamectl
---

```bash
# Set hostname
hostnamectl set-hostname HOSTNAME
# F32 and below to restart multicast DNS:
systemctl restart avahi-daemon.service
```
