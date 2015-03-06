logrotate
=========
:date: 2015-03-04

Syntax
------
::

 /path/to/file.log {
     rotate 30
     daily
     compress
     missingok
     copytruncate
     dateext
 }
