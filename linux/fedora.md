---
title: Fedora
---

## Upgrading to new Fedora versions

```bash
sudo dnf upgrade --refresh
sudo dnf install dnf-plugin-system-upgrade
sudo dnf system-upgrade download --refresh --releasever=30
```

https://fedoraproject.org/wiki/DNF_system_upgrade
