SELinux
=======
:tags: Linux, security
:date: 2017-06-15

Security Enhanced Linux

The cache for SELinux messages is known as the Access Vector Cache (AVC)

SELinux messages can be found in - /var/log/messages or /var/log/audit/audit.log

Get the mode
------------
::

  sestatus

Show available SELinux booleans
-------------------------------
::

 getsebool -a
 # For ~CentOS 6:
 semanage boolean -l

Modes
-----
- Enforcing
- Permissive
- Disabled

Change the context of a file
----------------------------
::

  chcon
  restorecon
