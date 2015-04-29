ZFS
===
:date: 2015-04-28

Compression
-----------
caveat - compression is *not* retroactive, so you should set it on the pool
immediately after creation

::

 # See compression settings
 zfs get compression
 # See compression efficiency
 zfs get compressratio
 # Set compression (types: lzjb, lz4, gzip-[1-9])
 zfs set compression=<type> <pool>

References
----------

* http://www.solarisinternals.com/wiki/index.php/ZFS_Evil_Tuning_Guide
