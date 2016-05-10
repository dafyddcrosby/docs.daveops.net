MySQL - ZFS
===========
:date: 2016-4-29

Set the ZFS recordsizes to match InnoDB page size
-------------------------------------------------

zfs set recordsize=16k  tank/db
zfs set recordsize=128k tank/log

Further reading
---------------
- `MySQL InnoDB ZFS Best Practices <https://blogs.oracle.com/realneel/entry/mysql_innodb_zfs_best_practices>`_
