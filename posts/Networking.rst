==========
Networking
==========


traceroute/ping
==============================
(as root)
{{{mtr google.com}}}

list listening ports
==============================
::

 netstat -plunt

Alarm when ping is successful
==============================
{{{ping -i 60 -a IP_address}}}

Get external IP
===============
::

 curl ipecho.net/plain
