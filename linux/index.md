---
title: Linux
tags: ["Linux"]
---

## force filesystem check on next boot

```bash
touch /forcefsck
```

## Socket programming with /dev/tcp

```bash
exec 3<>/dev/tcp/www.google.com/80
echo -e "GET / HTTP/1.1\n\n" >&3
cat <&3
```

## See what services are using a particular port
Run as root:

```bash
lsof -w -n -i (tcp|udp):<port>
```

or

```bash
netstat -luntp
```

## See if hard drive is on its last legs

```bash
# Get all health info
smartctl -a /dev/sda
# Run tests that take ~10m
smartctl -t short /dev/sda
```

## Get reboot/shutdown history

```bash
last -x
```

## Date utility

```bash
# Get the date from a timestamp
date -d @$TIMESTAMP
# Get the current time as a timestamp
date +%s
```

## Find all files with a setuid/setgid bit set

```bash
find / -perm +6000 -type f -exec ls -ld {} \; > setuid.txt &
```

## Burn an ISO from the command prompt

```bash
cdrecord -v -data image.iso
```

## Fixing missing shared library

- Create a .conf file in `/etc/ld.so.conf.d/` and put the library's directory in it.
- Run `ldconfig` to reload the system paths

## Find files changed in the past day

```bash
find . -ctime -1 -type f
```

## Disable caps lock

```bash
setxkbmap -option ctrl:nocaps
```

## Set time on machine that doesn't have NTP

```bash
date --set="$(ssh user@server date)"
```

## Inter-user communication

```bash
# Get list of logged in users
who
# Send message to all users
wall [message]
# Send message to another user's terminal
write user [ttyname]
# Enable/disable terminal message
mesg [n|y]
```

## Assembly

System call table located at `/usr/include/asm/unistd.h`

Red Hat syscall man pages installed with ``man-pages`` RPM. ``man 2 syscalls`` for a list, ``man 2 <syscall>`` for the syscall.

Put syscall (32-bit int) in EAX, put arguments in other ExX registers, `int 0x80`, result usually in EAX

## ip command

ifconfig is deprecated, ip was added in Linux 2.2

```bash
# Get IP address
ip addr
# Get network interface stats
ip link
# Get network interface packet stats
ip -s link

# Enable interface
ip link set eth0 up
# Set IP address
ip address add 192.168.1.23 dev eth0

# Show routing table
ip route show
```

## Sneaking around the open file limit
<https://www.youtube.com/watch?v=_XgXCVULj0o>

Open a pair of domain sockets (with socketpair) that connect to the same
process. Throw the FD in one end, close the FD, then read it out of the other
end. Recursively add the ring buffers...

## PipeFS, SockFS, DebugFS, SecurityFS
<https://www.linux.org/threads/pipefs-sockfs-debugfs-and-securityfs.9638/>

## setuid/setgid on executables

The kernel doesn't execute setuid scripts, only binaries. See the [UNIX
FAQ](http://www.faqs.org/faqs/unix-faq/faq/part4/section-7.html) for reasons
why.

## Change the ulimits of a running process

```bash
prlimit --pid $PID --nofile=8192
```

## Kernel resources

- <https://kernel.org>
- <https://lwn.net/>
- <https://kernelnewbies.org/>
- [Kernel syscalls](https://syscalls.kernelgrok.com/)


# Timezones

## Creating a timezone

```bash
echo "Zone MEST -6:00 - MEST" > MEST.zone
zic -d ~/.zoneinfo MEST.zone
export TZDIR=~/.zoneinfo TZ=MEST
```

## Setting timezone (systemd)

```bash
sudo timedatectl set-timezone TIMEZONE
```



# WONTFIX

There's a lot of crazy behaviour in Linux, that for whatever reason will not be fixed.

- [unintentional TCP self-connects](http://lkml.iu.edu/hypermail/linux/kernel/9909.3/0510.html)
- fingerprinting via UDP packets
