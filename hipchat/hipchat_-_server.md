---
title: HipChat - server
---

## XMPP

```bash
# See if XMPP enabled
hipchat network --show

# Enable XMPP
hipchat network --enable-xmpp-ports
```

## Export

```bash
# Run an export job
hipchat export --export -p PASSPHRASE # passphrase decrypts the package

# Check status of the export job
hipchat export --check
```

* https://confluence.atlassian.com/hc/exporting-and-importing-your-hipchat-data-688882302.html

## Links
[Admin Guide](https://confluence.atlassian.com/hc/administering-hipchat-server-622985653.html)
