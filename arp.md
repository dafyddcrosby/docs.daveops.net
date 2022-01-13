---
title: ARP
---

## Running arpwatch

```bash
arpwatch -i <interface> -u <non-root username>
```

## Use a static ARP table

```bash
# Single address:
arp -s <ip> <mac>

# File:
arp -f <filepath>
```
