--------
Mac OS X
--------
:date: 2015-04-28

Keyboard shortcuts
==================

+-----------------------+-----------------------------------------+
| Shortcut              | Desc                                    |
+=======================+=========================================+
| cmd + option + escape | bring up 'force quit applications' menu |
+-----------------------+-----------------------------------------+

kernel extensions
=================
::

 # list kernel extensions
 kextstat -l
 # unload kernel extensions
  kextunload -b <id>

Update software
===============
::

 softwareupdate -h

Search help
===========
apple key + ? , search for the help menu

Remove launch agents
====================
::

 # get launch list
 launchctl list
 # remove item
 launchctl remove <svc>

Use particular nameservers for a domain
---------------------------------------
Create hosts-style file in `/etc/resolver/<domain>`

See `man 5 resolver`

Flush DNS cache
---------------
::

 # On Yosemite+
 sudo discoveryutil mdnsflushcache

Burn ISO
--------
::

 hdiutil burn <image>

List disks
----------
::

 diskutil list

Create a RAM disk
-----------------
::

 # Replace XXXXX with MB * 2048 (eg a 4 gig is 8388608 (4096 * 2048))
 diskutil erasevolume HFS+ 'RAM Disk' `hdiutil attach -nomount ram://XXXXX`
