Mac OS X
--------

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
