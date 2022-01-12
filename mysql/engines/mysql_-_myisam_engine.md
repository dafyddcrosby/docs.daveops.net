# MySQL - MyISAM engine

<!-- TODO
   more technical details
   links section
-->

* uses table-level locking and lacks transactions, but has low overhead and is platform neutral
* excellent for read-only tables


Tools
-----


 myisam_ftdump
 A utility that displays information about full-text indexes in MyISAM tables. See Section 4.6.2, “myisam_ftdump — Display Full-Text Index information”.
 myisamchk
 A utility to describe, check, optimize, and repair MyISAM tables. See Section 4.6.3, “myisamchk —
 MyISAM Table-Maintenance Utility”. myisamlog
 A utility that processes the contents of a MyISAM log file. See Section 4.6.4, “myisamlog — Display MyISAM Log File Contents”.
 myisampack
 A utility that compresses MyISAM tables to produce smaller read-only tables. See Section 4.6.5,
 “myisampack — Generate Compressed, Read-Only MyISAM Tables”
 mysqlhotcopy
 A utility that quickly makes backups of MyISAM tables while the server is running. See Section 4.6.10, “mysqlhotcopy — A Database Backup Program”..

