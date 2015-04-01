DNS
---
:date:

Reverse lookup
==============================
``dig -x [ip addr]``

Query name server for IP addresses
==================================
``nslookup [name] [dns server]``

Get nameserver glue records
===========================
::

 # get root servers
 dig NS com
 # get glue records
 dig NS example.com @b.gtld-servers.net
