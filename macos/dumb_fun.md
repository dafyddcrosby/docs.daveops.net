---
title: Dumb Fun
---

# boot in text console mode

Uncomment the /usr/libexec/getty console line in ``/etc/ttys``

# Universal binaries

```
arch
lipo
```

# Record a terminal session

```bash
script -r
# do whatever, then exit
script -p typescript
```
