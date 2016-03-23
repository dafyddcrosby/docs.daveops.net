pfsync
======
:tags: OpenBSD, firewalls

sysctl:
-------
::

 net.inet.carp.preempt=1

ifconfig:
---------
::

 ifconfig em1 10.10.10.2 netmask 255.255.255.0
 ifconfig pfsync0 syncdev em1
 ifconfig pfsync0 up
