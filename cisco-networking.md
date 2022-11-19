# Cisco


# Firepower Threat Defense - Firepower Management Center


## Enabling the REST API

System>Configuration>REST API Preferences>Enable REST API


## CLI Modes

Regular Firepower Threat Defense CLI:

```
>
```

To enter Diagnostic Mode, type `system support diagnostic-cli`

user exec:

```
firepower>
```

privileged exec:

```
firepower#
```

To enter Expert Mode, type `expert`

```
admin@firepower:$
```


## CA certs

```
# get CAs for a trustpoint
show crypto ca certificates [trustpointname]
# show CA trustpoints
show crypto ca trustpoints [trustpointname]
```

<https://www.cisco.com/c/en/us/td/docs/security/firepower/command_ref/b_Command_Reference_for_Firepower_Threat_Defense/s_3.html#wp1813115769>


# Cisco NX-OS

Get inventory of pluggables:

```
show inventory
```

Get details of installed SFP/SFP+'s:

```
show interface transceiver details
```


# Cisco IOS

| Mode          | Description                               |
|------------- |----------------------------------------- |
| User          | generally no breaking changes allowed     |
| Enable        | Allows privileged access to the equipment |
| Configuration | Allows privileged access to the equipment |

| Command                            | Description                           |
|---------------------------------- |------------------------------------- |
| [command] ?                        | help mode                             |
| enable                             | enter Enable (privileged) mode        |
| configure terminal                 | Move to global configuration mode     |
| reload                             | reboot IOS                            |
| show running-config                | Get current config                    |
| show startup-config                | Get startup config                    |
| copy running-config startup-config | Save running config as startup config |
| erase startup-config               | erase startup-config file             |
| show mac address-table [dynamic]   | show the MAC address table            |
| show mac address-table count       | get MAC address space available       |

| Command             | Description                    |
|------------------- |------------------------------ |
| show ip ssh         | show SSH configuration         |
| show ssh            | show active SSH sessions       |
| show ip bgp summary | get quick info on BGP sessions |


## User access

```
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

- dir
- copy
- show file information
- show file systems
- more
- delete
- pwd
- cd
- mkdir
- rmdir


# CIMC

[CIMC CLI config guide](http://www.cisco.com/c/en/us/td/docs/unified_computing/ucs/c/sw/cli/config/guide/b_Cisco_CIMC_CLI_Configuration_Guide.pdf)


# ASA

A higher security-level can always talk to a lower security-level, but not vice-versa. If you have an internet-facing ASA, the inside port is 100, a DMZ is 50, internet-facing is 0.


# Common serial configuration defaults

- 9600 baud
- no parity
- 8 data bits
- 1 stop bit
- no flow control


# RANCID

<https://shrubbery.net/rancid/>