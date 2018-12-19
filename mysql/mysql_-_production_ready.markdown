# MySQL - Production Ready
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

For safe measure, don't just set the time zone in MySQL, but also set the system time zone to UTC

::

  default-time-zone=UTC

Change the default prompt
-------------------------

::

  [mysql]
  prompt="(\\U) [\d] > "

<https://dev.mysql.com/doc/refman/5.6/en/mysql-commands.html>

