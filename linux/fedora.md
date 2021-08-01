---
title: Fedora
---

## Upgrading to new Fedora versions

```bash
sudo dnf upgrade --refresh
sudo dnf install dnf-plugin-system-upgrade
sudo dnf system-upgrade download --refresh --releasever=33
```

https://fedoraproject.org/wiki/DNF_system_upgrade

## Building RPMs from spec files

```bash
dnf install fedora-packager fedora-review
usermod -a -G mock yourusername
```
https://docs.fedoraproject.org/en-US/quick-docs/creating-rpm-packages/
