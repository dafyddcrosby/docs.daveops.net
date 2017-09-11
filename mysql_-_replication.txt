MySQL - Replication
===================
:date: 2016-01-13
:date: 2016-05-16
:tags: MySQL, databases

Set the replica binlog coordinates
----------------------------------
::

  CHANGE MASTER TO
    MASTER_LOG_FILE='mysqld-bin.123456',
    MASTER_LOG_POS=123456;

Check the binlog in a human readable way
----------------------------------------
::

  mysqlbinlog <binlog file>

.. todo
   https://dev.mysql.com/doc/refman/5.6/en/replication-gtids-concepts.html

