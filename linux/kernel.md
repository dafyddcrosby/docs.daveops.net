# Linux kernel

# Remove older kernels

```bash
# (RHEL) Install yum-utils and run:
package-cleanup --oldkernels --count=1
```

# Kernel Analysis
* [Linux Kernel Analysis](http://www.faqs.org/docs/Linux-HOWTO/KernelAnalysis-HOWTO.html)

# Hardware

```bash
# Get cpu info
lscpu
cat /proc/cpuinfo
```



# kexec

Using systemctl

```bash
systemctl kexec
```


# SELinux

Security Enhanced Linux

The cache for SELinux messages is known as the Access Vector Cache (AVC)

SELinux messages can be found in - /var/log/messages or /var/log/audit/audit.log

```bash
ls -Z
id -Z
ps -Z
netstat -Z
# and even more!
```

## Labels

```
user:role:type (and optionally :level)
```

Generally speaking, you're concerned with type enforcement

## Files

```bash
# Change the context of a file
chcon ...
# Copy the security context from another file
chcon --reference RFILE TARGET...
# Restore files to default SELinux security context
restorecon
# manage login, user, port, interface, module, node, file context, boolean, permissive state, dontaudit
semanage
# Get the context type for a file
secon -t --file FILE
# Copy a file while preserving context
cp --preserve=context
# Get file contexts of directory
ls -Za
# List of default file contexts
semanage fcontext -l
```

## Booleans

Booleans allow you to enable/disable privileges for when you hit edge-cases or need non-default configuration

```bash
# Show available SELinux booleans
getsebool -a
# For ~CentOS 6:
semanage boolean -l
# Set a boolean value (add -P to make it permanent)
setsebool [boolean] [0|1]
```

/etc/selinux/targeted/modules/active/booleans.local 

## Users

```bash
# List users
semanage user -l
```

## Modes

```bash
# get the mode
sestatus
# change the mode (does not persist through reboot)
setenforce [ Enforcing | Permissive | 1 | 0 ]
```

The modes used by SELinux:

- Enforcing
- Permissive
- Disabled


## semodule

```bash
# Get available SE modules
semodule -l

# Install a policy module
semodule -i MOD_PKG
```

## audit2allow

RH RPM: policycoreutils-python

```bash
# Figure out why something is failing

grep httpd_t audit.log | audit2allow -M newmod
```

## setools-console

RH RPM: setools-console

```bash
# List contexts
seinfo -t

# Query policies
sesearch ...

# Search for files with a particular context
findcon /etc/selinux/targeted/contexts/files/file_contexts -t shadow_t
```

## Policies

policy name | description
---         | ---
targeted    | type enforcement rules, some RBAC
strict      | Full protection. TE, RBAC, much more aggressive
mls         | Multi-Level Security (more labels, more rules)

## Networking

```bash
# See ports and services
semanage port -l
# Add a port rule
semanage port -a -t http_port_t -p tcp 81
```

## Get the default SELinux context

matchpathcon

## Resources

* <https://github.com/SELinuxProject>
* <http://www.selinuxproject.org>
* [CentOS SELinux guide](https://wiki.centos.org/HowTos/SELinux)
* [Debian SELinux guide](https://wiki.debian.org/SELinux/Setup)
* [Fedora SELinux guide](https://fedoraproject.org/wiki/SELinux)
* [Arch SELinux wiki](https://wiki.archlinux.org/index.php/SELinux)

### TODO
what context is needed for the PID to access the file


# Transparent Huge Pages

Files in CentOS:

```bash
cat /sys/kernel/mm/transparent_hugepage/enabled
cat /sys/kernel/mm/transparent_hugepage/defrag
```

To disable:

```bash
echo 'never' > /sys/kernel/mm/transparent_hugepage/enabled
echo 'never' > /sys/kernel/mm/transparent_hugepage/defrag
```

* [THP kernel doc](https://www.kernel.org/doc/Documentation/vm/transhuge.txt)
* [hugetlbpage kernel doc](https://www.kernel.org/doc/Documentation/vm/hugetlbpage.txt)


# filesystems
# btrfs

## snapshots

```bash
# Create a read-only snapshot
btrfs subvolume snapshot -r <mount> <dest>

# Send snapshot to external drive
btrfs send <snap> | btrfs receive <device>
```


# ext filesystem

* ext2 - no journaling, but good for small solid-state drives
* ext3 - journaling - can be upgraded from ext2 with no data loss
* ext4 - support for large disks/file sizes

Superblock at the start, has info on filesystem. Groupblock holds information on subsections of the space, as well as superblock backups. Inode tables hold the file metadata.

```bash
# create label for the filesystem
e2label /dev/sda2 mylabel
# see label for filesystem
e2label /dev/sda2

# Add journal to ext2 filesystem
tune2fs -j /dev/sda2
# specify percentage that's reserved by root (by default 5%)
tune2fs -m1 /dev/sda2
# Disable automatic filesystem checking
tune2fs -c0 -i0 /dev/sda2
# Run filesystem check after 100 days
tune2fs -i100 /dev/sda2

# Get superblock, groupblock info
dumpe2fs /dev/sda2
dumpe2fs -h /dev/sda2 # superblock only

# Debug an ext2 filesystem
debugfs
```


# XFS

Default filesystem of CentOS 7

Can't be shrunk

```bash
xfs_admin -L mylabel /dev/sda2
```


# bcachefs

[Architecture](https://bcachefs.org/Architecture/)

# kernel modules

## basic commands

desc                           | command
---                            | ---
list modules                   | `lsmod`
get info about a module        | `modinfo MODULE`
load a module                  | `modprobe MODULE`
load kernel module by filename | `insmod FILE`
unload a module                | `modprobe -r MODULE` (or `rmmod MODULE` in a pinch)

## Resources

* [Linux Loadable Kernel Module](http://www.tldp.org/HOWTO/Module-HOWTO/)
* [Linux Kernel Module Programming](http://www.tldp.org/LDP/lkmpg/2.6/html/)



# UDP

## UDP fingerprinting
> In addition to his post, I heard a very good explanation of why they _intentionally_ coded it this way. The simplest way to create a packet is to simply set aside a section of memory for the packet and start filling in the necessary fields. For something like a UDP packet, there are some spaces in the packet that just aren't normally used. So if the field is left alne, is simply contains some data from whatever was stored in that section of memory previously. So instead of leaving those sections alone, they intentionally zeroed out the unused fields (such as the IP identification field) so that when the packet gets sent out, it doesn't give out any information that may have been laying around in memory.
> — from <http://www.antionline.com/showthread.php?221887.html>



#  DKMS

Dynamic Kernel Module System



# Building Kernels

```bash
dnf install fedpkg fedora-packager rpmdevtools ncurses-devel pesign bison kernel-devel glibc-static
```

TODO - add bison flex to <https://fedoraproject.org/wiki/Building_a_custom_kernel>

### Building a user-mode linux kernel

```
# TODO add a saner config
mini.config
CONFIG_BINFMT_ELF=y
CONFIG_HOSTFS=y
CONFIG_LBD=y
CONFIG_BLK_DEV=y
CONFIG_BLK_DEV_LOOP=y
CONFIG_STDERR_CONSOLE=y
CONFIG_UNIX98_PTYS=y
CONFIG_EXT2_FS=y
```

```bash
# build the config
make ARCH=um allnoconfig KCONFIG_ALLCONFIG=mini.config
# make the kernel
make ARCH=um
```

if missing gnu/stubs-32.h, means you need glibc-devel.i686



# cgroups

## Links

- <https://www.kernel.org/doc/Documentation/cgroup-v1/>
- <https://www.kernel.org/doc/Documentation/cgroup-v2.txt>
- <https://www.freedesktop.org/wiki/Software/systemd/ControlGroupInterface/>

# drgn

## Links

- [Meta blog post](https://developers.facebook.com/blog/post/2021/12/09/drgn-how-linux-kernel-team-meta-debugs-kernel-scale/?_fb_noscript=1)
- [Github](https://github.com/osandov/drgn)
