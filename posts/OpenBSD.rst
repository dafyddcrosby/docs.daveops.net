OpenBSD
-------

List PCI devices
================
::

 pcidump -v

Install package
===============
::

 pkg_add <package>

Add an IP alias to a network interface
======================================
::

 ifconfig carp0 inet alias 192.0.1.2 netmask 255.255.255.255

Delete an IP alias
==============================
::

 ifconfig carp0 delete 192.0.1.2

Download ports
==============
::

 cd /usr
 cvs get ports
