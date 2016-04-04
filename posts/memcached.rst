memcached
=========
:date: 2016-03-30

Get a list of keys
------------------
Note - this is an undocumented interface in the C memcached server

::
  
  # get slab info
  stats items
  # get dump of slab
  stats cachedump SLAB LIMIT

Using ASCII protocol through telnet
-----------------------------------
::

 telnet host 11211


+--------------------------------------------------------------------------------------------------------+-------------------+
| stats                                                                                                  | gets buncha stats |
+========================================================================================================+===================+
| set <key> <flags> <exptime> <bytes> [noreply]\r\n                                                      | set a value       |
+--------------------------------------------------------------------------------------------------------+-------------------+
| \*:<bytes> is the number of bytes in the data block to follow, *not* including the delimiting \r\n     |                   |
| <bytes> may be zero (in which case it's followed by an empty data block).                              |                   |
+--------------------------------------------------------------------------------------------------------+-------------------+
| get <key> - returns: VALUE <key> <flags> <bytes> [<cas unique>]\r\n                                    |                   |
+--------------------------------------------------------------------------------------------------------+-------------------+
| <data block>\r\n                                                                                       |                   |
+--------------------------------------------------------------------------------------------------------+-------------------+

Resources
---------
- `memcached site <http://memcached.org/>`_
- `libmemcached site <http://libmemcached.org/>`_
- `memdump <http://docs.libmemcached.org/bin/memdump.html>`_
