SELinux
=======
:tags: Linux, security

Security Enhanced Linux

The cache for SELinux messages is known as the Access Vector Cache (AVC)

SELinux messages can be found in - /var/log/messages or /var/log/audit/audit.log


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
