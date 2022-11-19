# Operating Systems


# Writing operating systems

<http://wiki.osdev.org>


# POSIX


## Standards

- [POSIX 1003.1 2013](http://pubs.opengroup.org/onlinepubs/9699919799/)
- [What could have been IEEE 1003.1e/2c (AKA capabilities)](http://wt.tuxomania.net/publications/posix.1e/download.html)
    - <http://www.trustedbsd.org/privileges.html>


# Minix

- <http://www.minix3.org/>


# Android


## Keystore

```shell
# Create a keystore
keytool -genkey -v -keystore /path/to/example-key.keystore -keyalg RSA -keysize 2048 -alias alias_name -validity 10000
# Get expiration dates in the store
keytool -list -v -keystore keystore.jks
```


## Auto-sign release

Add to ant.properties

```
key.store=/path/to/example-key.keystore
key.alias=alias_name
```


# Solaris

This includes Solaris derivatives as well, such as OpenSolaris and the Illumos project <http://www.brendangregg.com/blog/2017-09-05/solaris-to-linux-2017.html>


# Solaris


## SMF (Service Management Facility)


## Clear maintenance mode and restart

```shell
svcadm clear <FMRI>
```


## List services

```shell
svcs -a
```


## Logs

/var/svc/log


## Links

- <http://bnsmb.de/solaris/My_Little_SMF_FAQ.html>
- [Ben Rockwood's cheatsheet](http://www.cuddletech.com/blog/pivot/entry.php?id=182)


## Sun OpenBoot


## Boot from CDROM

```
boot cdrom
```


## Test hardware

```
test-all
```


## Show banner

```
banner
```


## Get ethernet address

```
.enet-addr
```


## Show hardware devices

```
show-devs
```


# SmartOS

- vmadm - start, stop, etc virtual machines
- imgadm - find, download, install images


## SmartDataCenter


## Find VM by alias

sdc-vmapi *vms | json -H -c "this.alias && this.alias.match(/riak*)"


## Output fwapi rules

sdc-fwapi /rules


## Update resolvers

sdc-vmapi *vms*?action=update -d '{ "resolvers": ["8.8.8.8", "8.8.4.4"] }'


# OmniOS

<https://omnios.org/>


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

```shell
cd /usr
cvs get ports
```


## USB snapshots

```shell
dd if=install*.fs of=/dev/sd1c bs=1m
```


## Updates

```shell
# See installed patches
syspatch -l
# See uninstalled patches
syspatch -c
# Install any uninstalled patches
syspatch
```


## Securelevels

Edit /etc/rc.securelevel or `sysctl kern.securelevel`

| num | desc                 |
|--- |-------------------- |
| -1  | permanently insecure |
| 0   | insecure             |
| 1   | secure               |
| 2   | highly secure        |


## Building from source

See `man 8 release`


## OpenBSD ld.so

```
# get documentation
man ld.so
# get information about what is getting loaded at run-time
LD_DEBUG=1 ./a.out
```


## trunk


### Setup

```
ifconfig bge0 up
ifconfig bge1 up
ifconfig trunk0 trunkport <proto> bge0 trunkport bge1 \
192.168.1.1 netmask 255.255.255.0
```

| proto       | desc                                                              |
|----------- |----------------------------------------------------------------- |
| failover    | Fails over to next link                                           |
| lacp        | increases link speed and redundancy, requires lacp-capable switch |
| loadbalance | load-balancing                                                    |
| roundrobin  | Use round-robin scheduler to distribute traffic                   |


## CARP

master advertises on port 112


### ifconfig syntax

```
ifconfig carpN create
ifconfig carpN [advbase n] [advskew n] [balancing mode]   \
[carpnodes vhid:advskew,vhid:advskew,...] [carpdev iface] \
[[-]carppeer peer_address] [pass passphrase] [state state] [vhid host-id]
```


### Try to become master

```shell
# Force master to give up control
ifconfig carp0 down

# Allow preemption
sysctl net.inet.carp.preempt=1
```


## pf


### pfctl

| flag | command                    |
|---- |-------------------------- |
| -e   | Enable pf                  |
| -d   | Disable pf                 |
| -nf  | parse file, don't load     |
| -f   | load pf.conf file          |
| -sr  | show rulesets              |
| -ss  | show state table           |
| -si  | show filter stats+counters |
| -sl  | show label counters        |
| -sa  | show everything            |


### General rule syntax

```
action [direction] [log] [quick] [on interface] [af] [proto protocol] \
[from src_addr [port src_port]] [to dst_addr [port dst_port]] \
[flags tcp_flags] [state] 
```

| action              | pass/block                                               |
|------------------- |-------------------------------------------------------- |
| direction           | in/out                                                   |
| quick               | if packet matches rule, do action and skip rest of rules |
| af (address family) | inet/inet6                                               |
| protocol            | udp/tcp/icmp                                             |

(`$ext_if`) is shorthand for "use the IP address for the rule" (handy with NAT)


### Default deny

```
block in  all
block out all 
```


### Table containing all IP addresses to firewall

```
table <firewall> const { self }
```


### Resources

- <https://www.openbsd.org/faq/pf/>


## pfsync


### sysctl

```
net.inet.carp.preempt=1
```


### ifconfig

```shell
ifconfig em1 10.10.10.2 netmask 255.255.255.0
ifconfig pfsync0 syncdev em1
ifconfig pfsync0 up
```


## Conferences

- [AsiaBSDCon](http://asiabsdcon.org)
- BSDCan


### View lifecycle

- viewDidLoad
- viewWillAppear
- viewDidAppear
- vieWillDisappear
- viewDidDisappear


# iOS web programming


## Web Inspector


### On Mac

Safari -> Preferences -> Advanced -> Show Develop in menu bar


### On iPhone

Settings -> Safari -> Advanced -> Web Inspector


## Application Name

```
<title>WebApp</title>
```


## Launcher icon (iOS 1.1.3+)

Rounded corners, no added shiny (iOS 4.2):

```
<link rel="apple-touch-icon-precomposed" href="apple-touch-icon-precomposed.png"/>
```

Added shiny:

```html
<link rel="apple-touch-icon" href="touch-icon-iphone.png" />
<link rel="apple-touch-icon" sizes="72x72" href="touch-icon-ipad.png" />
<link rel="apple-touch-icon" sizes="114x114" href="touch-icon-iphone4.png" />
```

With no sizes the default is 57x57 px

(Multiple sizes iOS 4.2+)


## Startup Image (iOS 3.0+)

```
<link rel="apple-touch-startup-image" href="/startup.png">
```

320x460 px, portrait


## Have standalone look

```
<meta name="apple-mobile-web-app-capable" content="yes" />
```

> You might think it's an innocent meta tag, but in fact it's a powerful and dangerous meta tag if you add it irresponsibly. You must provide a single page application solution offering back buttons inside the UI -or use location.href instead of normal <a> links if you don't want them to be opened in the browser instead of your app's container-. <https://medium.com/@firt/dont-use-ios-web-app-meta-tag-irresponsibly-in-your-progressive-web-apps-85d70f4438cb>

> That will involve: a) adding back navigation everywhere, b) create a SPA experience or use location.href instead of links for internal navigation, c) if the load process is done in fullscreen mode (navigator.standalone==true), always load the home screen not matter what the stored URL says and please d) don't suggest me to "download an app" if I'm already inside an app-like experience.


## Hide top status bar

NB - must have standalone mode on.

```
<meta name="apple-mobile-web-app-status-bar-style" content="black" />
```


## Prevent zooming

```
<meta name="viewport" content="initial-scale=1.0">
<meta name="viewport" content="maximum-scale=1.0">
<meta name="viewport" content="user-scalable=no">
<meta name="viewport" content="width=device-width">
```


## Links

<https://developer.apple.com/library/archive/documentation/AppleApplications/Reference/SafariWebContent/ConfiguringWebApplications/ConfiguringWebApplications.html>


# watchOS

watchOS app bundled in iOS app ![img](https://developer.apple.com/library/content/documentation/General/Conceptual/WatchKitProgrammingGuide/Art/architecture_compared_2x.png)


## Complications

The visual element on the watchface

- useful for frequently used info
- app stays in memory
- app receives more time to execute background tasks
- Aople recommends creating a complication, even if it is only a button to launch the app


## Notifications

short look - glanceable version of notification content if the wrist remains raised, goes to long look


## UI


### Key color

- The title string in the status bar
- App name in short-look notifications

stored in Global Tint property of an app's storyboad


### Page-based navigation

Add interface controllers


## Links

- [WatchKitProgrammingGuide](https://developer.apple.com/library/content/documentation/General/Conceptual/WatchKitProgrammingGuide/)
- [Human Interface Guidelines](https://developer.apple.com/watchos/human-interface-guidelines/)


# tvOS

<https://developer.apple.com/documentation/tvservices> <https://developer.apple.com/documentation/avkit> <https://developer.apple.com/documentation/mediaplayer> <https://developer.apple.com/documentation/tvmljs>


# osquery


## Get processes where the path no longer exists

```sql
select pid,path,uid from processes where on_disk=0;
```

- [Schema](https://osquery.io/schema/)


# Plan 9 from Bell Labs

- [Plan 9 Programmer's Manual](http://doc.cat-v.org/plan_9/1st_edition/manual.pdf)


# Fuchsia

<https://fuchsia.googlesource.com/fuchsia/+/refs/heads/main>


# PRIMOS OS

built in FORTRAN

- [(WP) PRIMOS](https://en.wikipedia.org/wiki/PRIMOS)
- <ftp://ftp.lysator.liu.se/pub/primos/>
- <http://bitsavers.informatik.uni-stuttgart.de/bits/Prime/>