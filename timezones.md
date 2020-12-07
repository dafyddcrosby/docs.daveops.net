---
title: Timezones
---

## Creating a timezone

```bash
echo "Zone MEST -6:00 - MEST" > MEST.zone
zic -d ~/.zoneinfo MEST.zone
export TZDIR=~/.zoneinfo TZ=MEST
```

## Setting timezone (systemd)

```bash
sudo timedatectl set-timezone TIMEZONE
```
