-----
MySQL
-----

Monitor queries
===============
::

 watch -n 1 mysqladmin --user=<user> --password=<password> processlist

List all databases
==================
.. code-block:: mysql

 show databases;

List all tables
===============
.. code-block:: mysql

 show tables;

Describe table contents
=======================
.. code-block:: mysql

 -- Column names
 describe <table>;
 -- Schema
 show create table <table>;

Create a database
=================
.. code-block:: mysql
   
 create database <databasename>;

Import database
===============
::

 mysql -u username -p<password> database < filename.sql

Export database
===============
::

 mysqldump -u username -p<password> database > filename.sql 

Delete database
===============
.. code-block:: mysql

 drop database <databasename>;

Create a user
=============
.. code-block:: mysql

 CREATE USER 'example_user'@'localhost' IDENTIFIED BY 'example_pass';

Resetting root password
=======================
::

 /etc/init.d/mysql stop
 /usr/bin/mysqld_safe --skip-grant-tables &
 mysql --user=root mysql

.. code-block:: mysql
   
 update user set Password=PASSWORD('new-password-here') WHERE User='root';
 flush privileges;
 exit

::

 fg
 # (ctrl-c to kill mysql)
 service mysql start

Create prefix index
==============================
.. code-block:: mysql

 alter table TABLENAME.COLUMN
 add key (COLUMN(n));

See what engine the table uses
==============================
.. code-block:: mysql

 show table status
 like 'table_name' \G

Isolation Levels
================

+------------------+-----------------------+-------------------------------+-------------------------+----------------+
| !Isolation level | !dirty reads possible | !nonrepeatable reads possible | !phantom reads possible | !locking reads |
+==================+=======================+===============================+=========================+================+
| READ UNCOMMITTED | t                     | t                             | t                       | f              |
+------------------+-----------------------+-------------------------------+-------------------------+----------------+
| READ COMMITTED   | f                     | t                             | t                       | f              |
+------------------+-----------------------+-------------------------------+-------------------------+----------------+
| REPEATABLE READ  | f                     | f                             | t                       | f              |
+------------------+-----------------------+-------------------------------+-------------------------+----------------+
| SERIALIZABLE     | f                     | f                             | f                       | t              |
+------------------+-----------------------+-------------------------------+-------------------------+----------------+

.. code-block:: mysql

 SET SESSION TRANSACTION ISOLATION LEVEL [level];

Autocommit
==========
.. code-block:: mysql

 SHOW VARIABLES LIKE 'AUTOCOMMIT';
 SET AUTOCOMMIT=[0|1]

Storage engines
==============================
MyISAM
-----------------------------------
* uses table-level locking and lacks transactions, but has low overhead and is platform neutral
* excellent for read-only tables

Repairing
~~~~~~~~~
.. code-block:: mysql

 check table [tablename]
 repair table [tablename]

InnoDB
------
has high overhead, but row-level locking with multiversion concurrency control (MVCC)

Memory engine
-------------
uses table-locking, but is speedy

Archive engine
--------------
* Only uses INSERT and SELECT
* Compresses each new row with zlib
* Low disk I/O
* Ideal for logging

CSV engine
----------
* Reads, writes to CSV files

Federated engine
-----------------------------------
(need to look into)

Blackhole engine
----------------
* No storage mechanism at all
* Useful for replication setups and audit logging

NDB Cluster Engine
-----------------------------------
* Consists of data nodes, management nodes, and SQL nodes
* Real-time performance with redunancy and load-balancing capabilities
* Complex joins are slow, but single table lookups can be fast

Falcon Engine
-------------
* Uses MVCC, tries to keep transactions in memory
* (need to see where it's development is at now)

soliddb engine
--------------
* similar to InnoDB

PBXT (Primebase XT) engine
-----------------------------------
* Has high write concurrency

Maria
-----
* (needs to be looked into)

Good ways to benchmark
==============================
* Use a query log to come up with a realistic workload that covers peek time and when batch jobs are run
* Use fresh snapshots between benchmarks
* Full stack tools:

  * ab
  * http_load

    * ``http_load -rate [requests_per_sec] -parallel [num_processes] -seconds [time] [url_file]``

  * JMeter

* Single component tools:

  * mysqlslap
  * Database Test Suite (made by OSDL)
  * sql-bench
  * Super Smack

Optimizing
==========
* Avoid NULL when possible
* ``optimize table``

indexing
--------
* Isolate the query column
* Try to simplify any math, and use literals when possible
* When indexing char colums, try using just a few letters
  * good target is ``count(distinct name) / count(*)``

Check the slow query log
------------------------
::

 log-slow-queries = file_name

Run profiling
-----------------------------------
::

 set profiling = 1;
 * run query *
 show profile;
