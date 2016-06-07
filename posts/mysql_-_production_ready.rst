MySQL - Production Ready
========================
:date: 2016-5-16

Remove all anonymous accounts
-----------------------------
::

  DROP USER ''@'localhost';
  DROP USER ''@'hostname';

Remove all non-localhost root users
-----------------------------------

Remove test databases
---------------------
::

  DELETE FROM mysql.db WHERE Db LIKE 'test%';
  FLUSH PRIVILEGES;
  DROP DATABASE test;

Set timezone to UTC
-------------------
