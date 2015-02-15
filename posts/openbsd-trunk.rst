OpenBSD - trunk
===============
:tags: openbsd 


Setup
-----
::

 ifconfig bge0 up
 ifconfig bge1 up
 ifconfig trunk0 trunkport <proto> bge0 trunkport bge1 \
 192.168.1.1 netmask 255.255.255.0

+-------------+-------------------------------------------------------------------+
| proto       | desc                                                              |
+=============+===================================================================+
| failover    | Fails over to next link                                           |
+-------------+-------------------------------------------------------------------+
| lacp        | increases link speed and redundancy, requires lacp-capable switch |
+-------------+-------------------------------------------------------------------+
| loadbalance | load-balancing                                                    |
+-------------+-------------------------------------------------------------------+
| roundrobin  | Use round-robin scheduler to distribute traffic                   |
+-------------+-------------------------------------------------------------------+
