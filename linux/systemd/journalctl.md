---
title: SystemD/journalctl
tags: init, Linux, RedHat
---

## journalctl

```bash
# Jump to the end of the log
journalctl -e
# Reverse to get newest messages first
journalctl -r
# Use the message catalog for explanatory text
journalctl -x
# Show kernel logs of previous boot
journalctl -k -b -1
# Tail the log of a service
journalctl -u SERVICENAME -f
```

## Message catalog

Catalogs in `/usr/lib/systemd/catalog/*.catalog`

- [Message catalog format](https://www.freedesktop.org/wiki/Software/systemd/catalog/)
