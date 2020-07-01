---
title: podman
---

## Add varlink interface

https://podman.io/blogs/2019/01/16/podman-varlink.html

## Change to cgroups v1

```bash
sudo grubby --update-kernel=ALL --args="systemd.unified_cgroup_hierarchy=0"
```
