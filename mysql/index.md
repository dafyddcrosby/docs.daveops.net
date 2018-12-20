# MySQL
@databases, @MySQL

Monitor queries
---------------
	watch -n 1 mysqladmin --user=<user> --password=<password> processlist


List all databases
------------------
	show databases;


List all tables
---------------

	show tables;


Describe table contents
-----------------------

	-- Column names
	describe TABLE_NAME;
	-- Schema
	show create table TABLE_NAME;
	-- Indexes
	show index from TABLE_NAME;


Dump the database schema
------------------------

	mysqldump --all-databases --no-data
	# --skip-add-drop-table 
	# --skip-comments


Server-side help
----------------

	-- contents
	help contents


Check if a server is up
-----------------------
::

 mysqladmin ping


Create a database
-----------------

	create database DATABASE_NAME;


Import database
---------------

::

 mysql -u username -p<password> database < filename.sql

Export database
---------------
::

 mysqldump -u username -p<password> database > filename.sql 

Delete database
---------------

	drop database DATABASE_NAME;


User management
---------------

	-- Create a user
	CREATE USER 'example_user'@'localhost' IDENTIFIED BY 'example_pass';
	-- Revoke permissions for user
	REVOKE ALL PRIVILEGES, GRANT OPTION FROM 'example_user'@'localhost';
	-- Delete a user
	DROP USER 'example_user'@'localhost';


Show grants
-----------

	-- show grants for current user
	show grants;
	-- show grants for particular user
	show grants for 'user'@'example.com';


Variables
---------

	-- session variables
	SHOW SESSION VARIABLES;
	SET SESSION sort_buffer_size=1000000;
	-- global variables
	SHOW GLOBAL VARIABLES;
	SET GLOBAL sort_buffer_size=1000000;


Resetting root password
-----------------------
::

 /etc/init.d/mysql stop
 /usr/bin/mysqld_safe --skip-grant-tables &
 mysql --user=root mysql

	update user set Password=PASSWORD('new-password-here') WHERE User='root';
	flush privileges;


	 fg
	 # (ctrl-c to kill mysql)
	 service mysql start


Create prefix index
-------------------

	alter table TABLENAME.COLUMN
	add key (COLUMN(n));


See what engine the table uses
------------------------------

	show table status
	like 'table_name' \G


See running processes
---------------------

	-- Quick glance
	show processlist ;
	-- sort by user
	select * from information_schema.processlist where user='foobar';


Isolation Levels
----------------

| Isolation level  | dirty reads possible | nonrepeatable reads possible | phantom reads possible | locking reads |
|------------------|----------------------|------------------------------|------------------------|---------------|
| READ UNCOMMITTED | t                    | t                            | t                      | f             |
| READ COMMITTED   | f                    | t                            | t                      | f             |
| REPEATABLE READ  | f                    | f                            | t                      | f             |
| SERIALIZABLE     | f                    | f                            | f                      | t             |

	SET SESSION TRANSACTION ISOLATION LEVEL [level];


Autocommit
----------

	SHOW VARIABLES LIKE 'AUTOCOMMIT';
	SET AUTOCOMMIT=[0|1]


Repairing
---------

	check table [tablename]
	repair table [tablename]


InnoDB engine
-------------
has high overhead, but row-level locking with multiversion concurrency control (MVCC)

Memory engine
-------------
uses table-locking, but is speedy

Archive engine
--------------

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


NDB Cluster Engine
------------------

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
--------------------------

* Has high write concurrency


indexing
--------


* similar to InnoDB


PBXT (Primebase XT) engine
--------------------------


* Has high write concurrency


Maria
-----


* (needs to be looked into)


Optimizing
----------


* Avoid NULL when possible
* ``optimize table``


indexing
--------


* Isolate the query column
* Try to simplify any math, and use literals when possible
* When indexing char colums, try using just a few letters
	* good target is ``count(distinct name) / count(*)``


Timezones
---------

	-- see what time zones are in use
	SELECT @@global.time_zone, @@session.time_zone;
	-- set global time zone
	SET GLOBAL time_zone = <timezone>;
	-- set session time zone
	SET time_zone = <timezone>;


mysqladmin
----------
mysqladmin COMMAND

| command    | desc        |
|------------|-------------|
| flush-logs | rotate logs |
| version    | get version |


Search for foreign key use
--------------------------
	select * from KEY_COLUMN_USAGE where REFERENCED_TABLE_NAME = 'tbl_name';


Disable super_read_only
-----------------------
	set global super_read_only=0

super_read_only <https://www.percona.com/blog/2016/09/27/using-the-super_read_only-system-variable/>

Good books
----------

* High Performance MySQL by Baron Schwartz, Perter Zaitsev, Vadim Tkachenko


