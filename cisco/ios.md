---
title: Cisco IOS
---

Mode          | Description
---           | ---
User          | generally no breaking changes allowed
Enable        | Allows privileged access to the equipment
Configuration | Allows privileged access to the equipment

Command                            | Description
---                                | ---
[command] ?                        | help mode
enable                             | enter Enable (privileged) mode
configure terminal                 | Move to global configuration mode
reload                             | reboot IOS
show running-config                | Get current config
show startup-config                | Get startup config
copy running-config startup-config | Save running config as startup config
erase startup-config               | erase startup-config file
show mac address-table [dynamic]   | show the MAC address table
show mac address-table count       | get MAC address space available

Command     | Description
---         | ---
show ip ssh | show SSH configuration
show ssh    | show active SSH sessions

## BGP

Command             | Desc
---                 | ---
show ip bgp summary | get quick info on sessions

## User access

```ios
! Set an enable mode secret
enable secret <PASSWORD>

! Only use version 2
ip ssh version 2

! Ask for a password with console login
line console 0
  login
  password <PASSWORD>

! Ask for a username on a network connection (good god don't use telnet)
line vty 0 15
  login local
  username bob password burger
  transport input ssh
 ```

### SSH

```
! Enable SSH
hostname foo
ip domain-name example.com
crypto key generate rsa

! Remove a key
ip ssh pubkey-chain
username NAME
key-hash ssh-rsa HASH KEYNAME
```

## SFP

Get DOM
```
show hw-module subslot 0/1 transceiver 0 status
```

## factory reset switch

Note - check `flash:` for any residual crap

```
erase nvram:
delete flash:vlan.dat
reload
```

also `write erase`

## Reset port to default config

in config mode:
```
default interface FastEthernet 1/0/1
```

## file system commands

* dir
* copy
* show file information
* show file systems
* more
* delete
* pwd
* cd
* mkdir
* rmdir
