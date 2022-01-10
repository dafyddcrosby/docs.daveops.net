---
title: OpenBSD
---

## List PCI devices

 pcidump -v

## Install package

 pkg_add <package>

## Add an IP alias to a network interface

 ifconfig carp0 inet alias 192.0.1.2 netmask 255.255.255.255

## Delete an IP alias

 ifconfig carp0 delete 192.0.1.2

## Download ports

 cd /usr
 cvs get ports

## USB snapshots

  dd if=install*.fs of=/dev/sd1c bs=1m

## Updates

```bash
# See installed patches
syspatch -l
# See uninstalled patches
syspatch -c
# Install any uninstalled patches
syspatch
```

## Securelevels

Edit /etc/rc.securelevel or `sysctl kern.securelevel`

num | desc
--- | ---
-1  | permanently insecure
0   | insecure
1   | secure
2   | highly secure


## Building from source

See `man 8 release`

# OpenBSD ld.so

```
# get documentation
man ld.so
# get information about what is getting loaded at run-time
LD_DEBUG=1 ./a.out
```


# OpenBSD - trunk

## Setup

 ifconfig bge0 up
 ifconfig bge1 up
 ifconfig trunk0 trunkport <proto> bge0 trunkport bge1 \
 192.168.1.1 netmask 255.255.255.0

| proto       | desc                                                              |
|-------------|-------------------------------------------------------------------|
| failover    | Fails over to next link                                           |
| lacp        | increases link speed and redundancy, requires lacp-capable switch |
| loadbalance | load-balancing                                                    |
| roundrobin  | Use round-robin scheduler to distribute traffic                   |


