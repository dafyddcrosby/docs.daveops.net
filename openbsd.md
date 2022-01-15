# OpenBSD

## List PCI devices

 pcidump -v

## Install package
```
 pkg_add <package>
```

## Add an IP alias to a network interface

 ifconfig carp0 inet alias 192.0.1.2 netmask 255.255.255.255

## Delete an IP alias

 ifconfig carp0 delete 192.0.1.2

## Download ports

 cd /usr
 cvs get ports

## USB snapshots

```bash
dd if=install*.fs of=/dev/sd1c bs=1m
```

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


# trunk

## Setup
```
 ifconfig bge0 up
 ifconfig bge1 up
 ifconfig trunk0 trunkport <proto> bge0 trunkport bge1 \
 192.168.1.1 netmask 255.255.255.0
```

proto       | desc
---         | ---
failover    | Fails over to next link
lacp        | increases link speed and redundancy, requires lacp-capable switch
loadbalance | load-balancing
roundrobin  | Use round-robin scheduler to distribute traffic


# CARP

master advertises on port 112

## ifconfig syntax
```
 ifconfig carpN create
 ifconfig carpN [advbase n] [advskew n] [balancing mode]   \
 [carpnodes vhid:advskew,vhid:advskew,...] [carpdev iface] \
 [[-]carppeer peer_address] [pass passphrase] [state state] [vhid host-id]
```

## Try to become master

```bash
# Force master to give up control
ifconfig carp0 down

# Allow preemption
sysctl net.inet.carp.preempt=1
```


# pf

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

