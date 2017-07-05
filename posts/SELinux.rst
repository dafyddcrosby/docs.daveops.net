SELinux
=======
:tags: Linux, security
:date: 2017-06-15
:modified: 2017-07-05

Security Enhanced Linux

The cache for SELinux messages is known as the Access Vector Cache (AVC)

SELinux messages can be found in - /var/log/messages or /var/log/audit/audit.log

Labels
------
user:role:type (and optionally :level)

generally speaking, you're concerned with type enforcement

::

  # Change the context of a file
  chcon ...
  # Copy the security context from another file
  chcon --reference RFILE TARGET...
  # Restore files to default SELinux security context
  restorecon
  # manage login, user, port, interface, module, node, file context, boolean, permissive state, dontaudit
  semanage

::

  ls -Z
  id -Z
  ps -Z
  netstat -Z
  # and even more!

Booleans
--------

Booleans allow you to enable/disable privileges for when you hit edge-cases or need non-default configuration

::

 # Show available SELinux booleans
 getsebool -a
 # For ~CentOS 6:
 semanage boolean -l
 # Set a boolean value (add -P to make it permanent)
 setsebool [boolean] [0|1]

/etc/selinux/targeted/modules/active/booleans.local 

Modes
-----
::

  # get the mode
  sestatus
  # change the mode (does not persist through reboot)
  setenforce [ Enforcing | Permissive | 1 | 0 ]

The modes used by SELinux:

- Enforcing
- Permissive
- Disabled

semodule
--------
::

  # Get available SE modules
  semodule -l

audit2allow
-----------
RH RPM: policycoreutils-python

::

  grep httpd_t audit.log | audit2allow -M newmod

seinfo
------

RH RPM: setools-console

::

  # List contexts
  seinfo -t


Get the default SELinux context
-------------------------------

matchpathcon

Resources
---------
- https://github.com/SELinuxProject
- http://www.selinuxproject.org
- `CentOS SELinux wiki <https://wiki.centos.org/HowTos/SELinux>`_
- `Debian SELinux wiki <https://wiki.debian.org/SELinux/Setup>`_
- `Fedora SELinux wiki <https://fedoraproject.org/wiki/SELinux>`_
- `Arch SELinux wiki <https://wiki.archlinux.org/index.php/SELinux>`_
