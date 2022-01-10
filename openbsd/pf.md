---
title: pf
tags:
  - OpenBSD
  - firewalls
---

## pfctl

flag       | command
---        | ---
-e         | Enable pf
-d         | Disable pf
-nf <file> | parse file, don't load
-f <file>  | load pf.conf file
-sr        | show rulesets
-ss        | show state table
-si        | show filter stats+counters
-sl        | show label counters
-sa        | show everything

## General rule syntax

```
action [direction] [log] [quick] [on interface] [af] [proto protocol] \
[from src_addr [port src_port]] [to dst_addr [port dst_port]] \
[flags tcp_flags] [state] 
```

action              | pass/block
---                 | ---
direction           | in/out
quick               | *if packet matches rule, do action and skip rest of rules*
af (address family) | inet/inet6
protocol            | udp/tcp/icmp

($ext_if) is shorthand for "use the IP address for the rule" (handy with NAT)

## Default deny

```
block in  all
block out all 
```

## Table containing all IP addresses to firewall

```
table <firewall> const { self }
```

## Resources

* https://www.openbsd.org/faq/pf/


# pfsync

## sysctl

 net.inet.carp.preempt=1

## ifconfig

 ifconfig em1 10.10.10.2 netmask 255.255.255.0
 ifconfig pfsync0 syncdev em1
 ifconfig pfsync0 up

