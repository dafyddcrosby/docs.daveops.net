# Linux


## force filesystem check on next boot

```shell
touch /forcefsck
```


## Socket programming with /dev/tcp

```shell
exec 3<>/dev/tcp/www.google.com/80
echo -e "GET / HTTP/1.1\n\n" >&3
cat <&3
```


## See what services are using a particular port

Run as root:

```shell
lsof -w -n -i (tcp|udp):<port>
```

or

```shell
netstat -luntp
```


## See if hard drive is on its last legs

```shell
# Get all health info
smartctl -a /dev/sda
# Run tests that take ~10m
smartctl -t short /dev/sda
```


## Get reboot/shutdown history

```shell
last -x
```


## Date utility

```shell
# Get the date from a timestamp
date -d @$TIMESTAMP
# Get the current time as a timestamp
date +%s
```


## Find all files with a setuid/setgid bit set

```shell
find / -perm +6000 -type f -exec ls -ld {} \; > setuid.txt &
```


## Burn an ISO from the command prompt

```shell
cdrecord -v -data image.iso
```


## Fixing missing shared library

- Create a .conf file in `/etc/ld.so.conf.d/` and put the library's directory in it.
- Run `ldconfig` to reload the system paths


## Find files changed in the past day

```shell
find . -ctime -1 -type f
```


## Disable caps lock

```shell
setxkbmap -option ctrl:nocaps
```


## Set time on machine that doesn't have NTP

```shell
date --set="$(ssh user@server date)"
```


## Inter-user communication

```shell
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

Red Hat syscall man pages installed with `man-pages` RPM. `man 2 syscalls` for a list, `man 2 <syscall>` for the syscall.

Put syscall (32-bit int) in EAX, put arguments in other ExX registers, `int 0x80`, result usually in EAX


## ip command

ifconfig is deprecated, ip was added in Linux 2.2

```shell
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

Open a pair of domain sockets (with socketpair) that connect to the same process. Throw the FD in one end, close the FD, then read it out of the other end. Recursively add the ring buffers...


## PipeFS, SockFS, DebugFS, SecurityFS

<https://www.linux.org/threads/pipefs-sockfs-debugfs-and-securityfs.9638/>


## setuid/setgid on executables

The kernel doesn't execute setuid scripts, only binaries. See the [UNIX FAQ](http://www.faqs.org/faqs/unix-faq/faq/part4/section-7.html) for reasons why.


## Change the ulimits of a running process

```shell
prlimit --pid $PID --nofile=8192
```


## Kernel resources

- <https://kernel.org>
- <https://lwn.net/>
- <https://kernelnewbies.org/>
- [Kernel syscalls](https://syscalls.kernelgrok.com/)


# Timezones


## Creating a timezone

```shell
echo "Zone MEST -6:00 - MEST" > MEST.zone
zic -d ~/.zoneinfo MEST.zone
export TZDIR=~/.zoneinfo TZ=MEST
```


## Setting timezone (systemd)

```shell
sudo timedatectl set-timezone TIMEZONE
```


# WONTFIX

There's a lot of crazy behaviour in Linux, that for whatever reason will not be fixed.

- [unintentional TCP self-connects](http://lkml.iu.edu/hypermail/linux/kernel/9909.3/0510.html)
- fingerprinting via UDP packets


# Linux - ELF

```shell
# Get a program's headers
readelf -l ./program
```


# fdisk

```shell
# show partition table from CLI
fdisk -l <disk>
```

| description            | command |
|---------------------- |------- |
| show menu              | m       |
| show partition table   | p       |
| create a new partition | n       |
| change filesystem type | t       |
| list filesystem types  | l       |
| write partition table  | w       |

Use `sfdisk` to script disk formatting # User Management


## Users

```shell
# change user info
chfn

# Delete user, their home directory, and their mailbox
userdel -r [user]

# Add user, home directory
useradd -m [user]

# Create system user
useradd -r [user]

# See password policies for user
chage -l [user]
```

Only superusers can change ownership of a file

Executable scripts require read and execute bits


## umask

Octal mask to deny permissions by default

Files can't have execution at creation, but directories do. Set your octal mask to deal with the executable

```shell
# Get umask
umask

# Set a umask that denies any other-user access
umask 077
```


## Groups

- newgrp - logs into a new shell with a new primary group
- chgrp - change the group for files
- groupadd - create a group
- usermod - add users to the group

Sticky bit - t or T in the mode line


# Disk quotas

Add usrquota/grpquota to the mount options of the drive

```shell
# Create quota database
quotacheck -cugm /

# Turn quota on for all disks
quotaon -a

# Get quota usage for a user
quota USER

# Create a report of user quota usage
repquota /

# Change a quota for a user
edquota USER
# Copy a quota setting to another user
edquota -p USER1 USER2
# Change grace period
edquota -t
# Set disk quotas
setquota USER block-soft block-hard inode-soft inode-hard mount-point
```


# swap

Rule of thumb is usually twice physical RAM, but not strictly necessary. You do get a benefit of the kernel putting crash dumps into it.

```shell
# convert partition to swap space
mkswap /dev/sda2

# enable the partition swap
swapon /dev/sda2

# show current swap space
swapon -s
```


# iptables


## Set up default DROP rule for eth0

```shell
iptables -P INPUT DROP
```


## Allow existing connections to continue

```shell
iptables -A INPUT -i eth0 -m state --state ESTABLISHED,RELATED -j ACCEPT
```


## Accept everything from the 192.168.1.x network

```shell
iptables -A INPUT -i eth0 -s 192.168.1.0/24 -j ACCEPT
```


## Drop a single host

```shell
iptables -I INPUT -s 192.168.1.100 -j DROP
```


## Allow connections from this host to 192.168.2.10

```shell
iptables -A OUTPUT -o eth0 -d 192.168.2.10 -j ACCEPT
```


## Flush table

```shell
iptables --flush
```


## Allow incoming HTTP

```shell
iptables -A INPUT -i eth0 -p tcp --dport 80 -m state --state NEW,ESTABLISHED -j ACCEPT
iptables -A OUTPUT -o eth0 -p tcp --sport 80 -m state --state ESTABLISHED -j ACCEPT
```


## Limit connections

```shell
iptables -A INPUT -p tcp --dport 80 -m limit --limit 25/minute --limit-burst 100 -j ACCEPT
```


## Simple IP masquerading

```shell
echo "1" > /proc/sys/net/ipv4/ip_forward
iptables -t nat -A POSTROUTING -o $EXT_IFACE -j MASQUERADE
```


## Backup, reload iptables rules

```shell
iptables-save > ./file
iptables-restore < ./file
```


## File location

- /etc/sysconfig/iptables


# firewalld

```shell
# is firewalld running?
firewall-cmd --state
# permanently open HTTPS port
firewall-cmd --permanent --add-service=https
```


# Linux/ld


# The GNU linker

- `/etc/ld.so.conf` configures directories to search
- `/etc/ld.so.cache` is the binary cache used by ld.so

```shell
# Reload ld cache
ldconfig
# See which libraries are utilized
ldconfig -v

# See what an executable links to
ldd ./executablefile

# Specify an alternate library path
export LD_LIBRARY_PATH=/path/to/dir ...

# Link object file into an executable file
ld -o example example.o
# Strip all binary symbol information
ld -s ...
# Strip debug binary symbol information
ld -S ...
# Mark stack as non executable
ld -z noexecstack
```


# Lokkit


## Get list of services

```shell
lokkit --list-services
```


## Open port

```shell
lokkit --selinux=disabled --update --enabled -p [port]:[tcp|udp]
# or to open a service
lokkit -s [service]
```


# LVM


## Cheatsheet

```shell
# Initialize a disk for LVM
pvcreate PHYSICAL_VOLUME

# Create a volume group
vgcreate GROUP_NAME DISKS...

# List groups
vgs

# Create a logical volume with 100% free space
lvcreate -l 100%free -n VOLUME_NAME GROUP_NAME
```


## Links

- <https://access.redhat.com/documentation/en-US/Red_Hat_Enterprise_Linux/7/html/Logical_Volume_Manager_Administration/LVM_examples.html>


# kickstart

- Press ESC

- `boot: linux ks=nfs:192.168.75.132:/srv/nfs/ks.cfg`


## Links

<https://www.centos.org/docs/5/html/Installation_Guide-en-US/s1-kickstart2-options.html>


# genisoimage

```shell
genisoimage -o cd.iso $DIRECTORY
```


# LUKS

```shell
# Encrypt a partition
cryptsetup luksFormat /dev/sda2

# Mount partition
cryptsetup open /dev/sda2/ mapping_name
mount /dev/mapper/mapping_name /mount/dir
```


# mdadm


## Get details of RAID setup

```shell
mdadm --detail /dev/md0
cat /proc/mdstat
```


## Adjust the array

```shell
# Drop a disk from the array
/sbin/mdadm /dev/md0 --fail /dev/sda1 --remove /dev/sda1

# Add a disk to the array
/sbin/mdadm /dev/md0 --add /dev/sda1
```


# Partitioning

`/dev/sd*` - Drives that start with sd are either SATA, SCSI, or USB drives.

`/dev/hd*` - Drives that start with hd are PATA, also known as IDE drives.


## GUID vs. MBR

GUID Partition Table supports disks up to 9ZB, 128 partitions per-disk.


# proc

- <http://man7.org/linux/man-pages/man5/proc.5.html>


## Get filesystems kernel can use

```shell
# Get filesystems kernel can use
cat /proc/filesystems
# Get mounted file systems
cat /proc/self/mounts
# Get arguments to kernel from bootloader
cat /proc/cmdline
```

- <https://perf.wiki.kernel.org/index.php/Tutorial>


## Get kernel command line arguments

```
cat /proc/cmdline
```


# Mount

```shell
# Mounting an ISO
mount -o loop disk1.iso /mnt/disk

# Remount a filesystem (change options without unmounting)
mount /home -o remount, noatime

# Create a RAM disk
mount -t tmpfs -o size=1g tmpfs /mnt
```


## Lazy unmount of a partition

Linux 2.4.11+

```shell
umount -l <mount>
```


## fuser

```shell
# See what processes are using /mnt
fuser -v /mnt

# Kill processes using /mnt
fuser -k -KILL /mnt
```


## UUID

```shell
# find a filesystem
findfs UUID=...
# list filesystems
blkid
```


# systemd


## systemctl

| Command                        | Notes                                                                                    |
|------------------------------ |---------------------------------------------------------------------------------------- |
| systemctl                      | List services                                                                            |
| start SERVICE                  | Used to start a service (not reboot persistent)                                          |
| stop SERVICE                   | Used to stop a service (not reboot persistent)                                           |
| restart SERVICE                | Used to stop and then start a service                                                    |
| reload SERVICE                 | When supported, reloads the config file without interrupting pending operations          |
| condrestart SERVICE            | Restarts if the service is already running                                               |
| status SERVICE                 | Tells whether a service is currently running                                             |
| enable SERVICE                 | Turn the service on, for start at next boot, or other trigger                            |
| disable SERVICE                | Turn the service off for the next reboot, or any other trigger                           |
| is-enabled SERVICE             | Used to check whether a service is configured to start or not in the current environment |
| list-unit-files --type=service | Print a table of services that lists which runlevels each is configured on or off        |
| daemon-reload                  | Used when you create a new service file or modify any configuration                      |
| list-dependencies              | Show dependency tree of a target                                                         |


## Directories

- /etc/systemd/system/\*.wants/SERVICE.service - Used to list what levels this service is configured on or off


## Runlevels

To change the runlevel at boot, add the following to the kernel arguments, e.g. `systemd.unit=rescue.target`

To change the runlevel in a running system, `systemctl isolate rescue.target`

| target            | desc                     |
|----------------- |------------------------ |
| poweroff.target   | halt/shut off system     |
| rescue.target     | single user mode         |
| multi-user.target | normal startup of system |
| graphical.target  | graphical startup        |
| reboot.target     | restart system           |


## Units

| key         | value                                                     |
|----------- |--------------------------------------------------------- |
| Environment | Space separated key-value pairs for environment variables |

- [Directives](https://www.freedesktop.org/software/systemd/man/systemd.directives.html)
- [systemd unit configuration](https://www.freedesktop.org/software/systemd/man/systemd.unit.html)


## Running user-level services

Put unit into `~/.config/systemd/user/NAME.service`

Run `systemctl` commands with `--user` flag


## Mountpoints

- <https://www.freedesktop.org/software/systemd/man/systemd.mount.html>


## Analyze boot time

```shell
systemd-analyze
```


## See also

- [journalctl](../../log-management.md#journalctl)
- [hostnamectl](hostnamectl.md)


## Links

- [homepage](https://systemd.io/)
- <https://cgit.freedesktop.org/systemd/systemd/>
- <https://www.freedesktop.org/software/systemd/man/index.html>
- [Design documentation](http://0pointer.de/blog/projects/systemd.html)


# systemd/hostnamectl

```shell
# Set hostname
hostnamectl set-hostname HOSTNAME
# F32 and below to restart multicast DNS:
systemctl restart avahi-daemon.service
```