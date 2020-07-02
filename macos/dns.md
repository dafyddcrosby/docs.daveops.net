---
title: dns
---

| path                                                       | description                   |
| ---------------------------------------------------------- | ----------------------------- |
| /etc/resolv.conf                                           | not used                      |
| /private/etc/hosts                                         | force resolution              |
| /Library/Preferences/SystemConfiguration/preferences.plist | preferences                   |
| /etc/resolver/                                             | domain nameserver overrides   |


## Flush DNS cache

```bash
# On Yosemite+
sudo discoveryutil mdnsflushcache
# 10.10.4+
sudo dscacheutil -flushcache; sudo killall -HUP mDNSResponder
```

## Resolution

```bash
# Use Mac resolution service
dns-sd -G v4 example.com

# Query via DirectoryService
dscacheutil -q host -a name example.org

# Don't use Mac resolution
nslookup example.com
```

## DNS settings

``scutil --dns``


## mDNSResponder

listens port 5353

## helpful man pages

resolver(5)

