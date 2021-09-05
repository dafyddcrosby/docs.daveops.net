---
title: SystemD
tags: init, Linux, RedHat
---

## systemctl

Command                        | Notes
---                            | ---
systemctl                      | List services
start SERVICE                  | Used to start a service (not reboot persistent)
stop SERVICE                   | Used to stop a service (not reboot persistent)
restart SERVICE                | Used to stop and then start a service
reload SERVICE                 | When supported, reloads the config file without interrupting pending operations
condrestart SERVICE            | Restarts if the service is already running
status SERVICE                 | Tells whether a service is currently running
enable SERVICE                 | Turn the service on, for start at next boot, or other trigger
disable SERVICE                | Turn the service off for the next reboot, or any other trigger
is-enabled SERVICE             | Used to check whether a service is configured to start or not in the current environment
list-unit-files --type=service | Print a table of services that lists which runlevels each is configured on or off
daemon-reload                  | Used when you create a new service file or modify any configuration
list-dependencies              | Show dependency tree of a target

## journalctl

```bash
# Jump to the end of the log
journalctl -e
# Use the message catalog for explanatory text
journalctl -x
# Show kernel logs of previous boot
journalctl -k -b -1
# Tail the log of a service
journalctl -u SERVICENAME -f
```

## hostnamectl

```bash
# Set hostname
hostnamectl set-hostname HOSTNAME
# F32 and below to restart multicast DNS:
systemctl restart avahi-daemon.service
```

## Directories

* /etc/systemd/system/\*.wants/SERVICE.service - Used to list what levels this
  service is configured on or off

## Runlevels

To change the runlevel at boot, add the following to the kernel arguments, e.g.
`systemd.unit=rescue.target`

To change the runlevel in a running system, `systemctl isolate rescue.target`

target            | desc
---               | ---
poweroff.target   | halt/shut off system
rescue.target     | single user mode
multi-user.target | normal startup of system
graphical.target  | graphical startup
reboot.target     | restart system

## Units

key         | value
---         | ---
Environment | Space separated key-value pairs for environment variables

* [Directives](https://www.freedesktop.org/software/systemd/man/systemd.directives.html)
* [systemd unit configuration](https://www.freedesktop.org/software/systemd/man/systemd.unit.html)

## Running user-level services

Put unit into `~/.config/systemd/user/NAME.service`

Run `systemctl` commands with `--user` flag

## Mountpoints

* <https://www.freedesktop.org/software/systemd/man/systemd.mount.html>

## Analyze boot time

```bash
systemd-analyze
```

## Links

* <https://cgit.freedesktop.org/systemd/systemd/>
* <https://www.freedesktop.org/software/systemd/man/index.html>
* [Design documentation](http://0pointer.de/blog/projects/systemd.html)

