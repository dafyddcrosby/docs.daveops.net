---
title: Test Kitchen
tags: ["Chef"]
---

## Setting RAM on Vagrant

```yaml
driver:
  name: vagrant
  customize:
    memory: 2048
```

## Setting environment in chef-zero

```yaml
driver:
  name: vagrant

provisioner:
  name: chef_zero
  environments_path: path/to/environments
  client_rb:
    environment: production
```

## Links

* [Test Kitchen website](https://kitchen.ci)
* <https://github.com/someara/kitchen-dokken>

