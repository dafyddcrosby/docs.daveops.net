---
title: Cisco IOS
---

| Mode          | Description                               |
| -----------   | ----------------------------------------- |
| User          | generally no breaking changes allowed     |
| Enable        | Allows privileged access to the equipment |
| Configuration | Allows privileged access to the equipment |

| Command                          | Description                       |
| --                               | --                                |
| [command] ?                      | help mode                         |
| enable                           | enter Enable (privileged) mode    |
| configure terminal               | Move to global configuration mode |
| reload                           | reboot IOS                        |
| show running-config              | Get current config                |
| show startup-config              | Get startup config                |
| erase startup-config             | erase startup-config file         |
| show mac address-table [dynamic] | show the MAC address table        |
| show mac address-table count     | get MAC address space available   |

| Command     | Description              |
| --------    | ----------               |
| show ip ssh | show SSH configuration   |
| show ssh    | show active SSH sessions |

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

### Enable SSH
```ios
hostname foo
ip domain-name example.com
crypto key generate rsa
```
