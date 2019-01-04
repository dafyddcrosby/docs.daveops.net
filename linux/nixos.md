---
title: NixOS
---

Uses /etc/nixos/configuration.nix for machine config

nixos-rebuild to use it

nix-channel handles the repositories

## /etc/nixos/configuration.nix

```
# auto upgrade system
system.autoUpgrade.enable = true;
```

## NixOS containers

As of 2019-01-04, "NixOS containers are not perfectly isolated from the host
system. This means that a user with root access to the container can do things
that affect the host. So you should not give container root access to untrusted
users." from <https://nixos.org/nixos/manual/index.html#ch-containers>

```bash
# Create a container
nixos-container create foo
# Run a container
nixos-container start foo
```
