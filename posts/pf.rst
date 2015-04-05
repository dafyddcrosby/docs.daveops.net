==
pf
==
:date: 2015-04-05
:tags: openbsd

pfctl
=====

+------------+----------------------------+
| !flag      | !command                   |
+============+============================+
| -e         | Enable pf                  |
+------------+----------------------------+
| -d         | Disable pf                 |
+------------+----------------------------+
| -nf <file> | parse file, don't load     |
+------------+----------------------------+
| -f <file>  | load pf.conf file          |
+------------+----------------------------+
| -sr        | show rulesets              |
+------------+----------------------------+
| -ss        | show state table           |
+------------+----------------------------+
| -si        | show filter stats+counters |
+------------+----------------------------+
| -sa        | show everything            |
+------------+----------------------------+

General rule syntax
==============================
::

 action [direction] [log] [quick] [on interface] [af] [proto protocol] \
 [from src_addr [port src_port]] [to dst_addr [port dst_port]] \
 [flags tcp_flags] [state] 

+---------------------+------------------------------------------------------------+
| action              | pass/block                                                 |
+=====================+============================================================+
| direction           | in/out                                                     |
+---------------------+------------------------------------------------------------+
| quick               | *if packet matches rule, do action and skip rest of rules* |
+---------------------+------------------------------------------------------------+
| af (address family) | inet/inet6                                                 |
+---------------------+------------------------------------------------------------+
| protocol            | udp/tcp/icmp                                               |
+---------------------+------------------------------------------------------------+

Default deny
==============================
::

 block in  all
 block out all 

Table containing all IP addresses to firewall
=============================================
::

 table <firewall> const { self }
