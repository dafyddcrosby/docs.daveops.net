---
title: Lokkit
tags: RedHat, firewalls
---

## Get list of services

```bash
lokkit --list-services
```

## Open port

```bash
lokkit --selinux=disabled --update --enabled -p [port]:[tcp|udp]
# or to open a service
lokkit -s [service]
```