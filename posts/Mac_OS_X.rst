Mac OS X
========
:date: 2016-03-04
:modified: 2017-01-19

Keyboard shortcuts
------------------

+-----------------------+-----------------------------------------+
| Shortcut              | Desc                                    |
+=======================+=========================================+
| cmd + option + escape | bring up 'force quit applications' menu |
+-----------------------+-----------------------------------------+
| cmd + shift + 3       | take screenshot of all screens          |
+-----------------------+-----------------------------------------+
| cmd + shift + 4       | take a partial screenshot               |
+-----------------------+-----------------------------------------+
| cmd + shift + eject   | lock screen                             | 
+-----------------------+-----------------------------------------+

kernel extensions
-----------------
::

 # list kernel extensions
 kextstat -l
 # unload kernel extensions
  kextunload -b <id>

Update software
---------------
::

 softwareupdate -h

Search help
-----------
apple key + ? , search for the help menu

Increase maxfiles for session
-----------------------------
::

  ulimit -n 4096

Remove launch agents
--------------------
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

Get linked libraries/object files
---------------------------------
::

 # List shared libraries
 otool -L <executable>

.. TODO look more into otool's operations

Create a RAM disk
-----------------
::

 # Replace XXXXX with MB * 2048 (eg a 4 gig is 8388608 (4096 * 2048))
 diskutil erasevolume HFS+ 'RAM Disk' `hdiutil attach -nomount ram://XXXXX`

Boot Options
------------

| keypress | action |
| Cmd + r | Recovery Mode |
| Cmd + v | Verbose Mode |
| Cmd + s | Single-user Mode |
| Shift | Safe Mode |
| D | Apple Diagnostics / Hardware Test |
| C | Boot removable device |
| N | Boot from network |
| Option | Startup Manager |
| Cmd + Option + P + R | Reset NVRAM |

Links
-----

https://opensource.apple.com/
