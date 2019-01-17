---
title: SELinux
tags: ["Linux", "security"]
---

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

* Enforcing
* Permissive
* Disabled


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

| policy name | desc |
|-------------|------|
| targeted | type enforcement rules, some RBAC |
| strict | Full protection. TE, RBAC, much more aggressive |
| mls | Multi-Level Security (more labels, more rules) |

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

# TODO
what context is needed for the PID to access the file
