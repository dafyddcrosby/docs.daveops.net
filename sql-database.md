# SQL


## To add a row

```sql
INSERT INTO table_name (column1, column2, column3,...)
VALUES (value1, value2, value3,...)
```


## To update a row

```sql
UPDATE table_name
SET column1=value, column2=value2,...
WHERE some_column=some_value
```


## To sort a table

```sql
SELECT row1, row2
FROM table
ORDER BY row2 (ASC|DESC)
```


## Delete rows

```sql
DELETE FROM table_name
WHERE some_column=some_value
```


## Search in fields

```sql
SELECT *
FROM Persons
WHERE City LIKE '%ville'
```


## Conditional statements

```sql
CASE WHEN condition THEN result
[WHEN ...]
[ELSE result]
END
```


## Format a date

```sql
SELECT DATE_FORMAT(`date`,'%Y-%m-%d') AS showdate 
FROM table
```


## Retrieve records within 90 days of stamp

```sql
FROM stockserialitems
WHERE expirationdate < utc_timestamp() + interval 90 day
```


## Check for duplicate rows

```sql
SELECT a, b, count(*) cnt 
FROM table
GROUP BY a, b 
HAVING cnt > 1
ORDER BY cnt asc;
```


## Standards

- 1992
- 1999
- 2003
- 2008
- new ones?


# SQLite


## Show all tables

```
.tables
```


## Show table schema

```

.schema tablename
```


## See if table exists

```sql
SELECT name
FROM sqlite_master
WHERE type='table'
```


## Export tables

```
sqlite3 my.db .dump
```


## Get table sizes

```sqlite
SELECT SUM("pgsize") FROM "dbstat" WHERE name='TABLENAME';
```


## Datatypes

- NULL. The value is a NULL value.
- INTEGER. The value is a signed integer, stored in 1, 2, 3, 4, 6, or 8 bytes depending on the magnitude of the value.
- REAL. The value is a floating point value, stored as an 8-byte IEEE floating point number.
- TEXT. The value is a text string, stored using the database encoding (UTF-8, UTF-16BE or UTF-16LE).
- BLOB. The value is a blob of data, stored exactly as it was input.


# PostgreSQL


## Connect to a shell

psql


## List databases

```shell
$ psql -l

# in psql shell \l
```


## Configuration

/var/lib/pgsql/data/postgresql.conf


## View current queries

```sql
SELECT * FROM pg_stat_activity ;
```


## Killing long running queries

Don't use SIGKILL, it will shut down the rest of PostgreSQL, and require a replay

```
pg_cancel_backend(pid int) 
pg_terminate_backend(pid int)
```


## Tuning

| config                 | set                                          |
|---------------------- |-------------------------------------------- |
| `shared_buffers`       | About 1/4 of the memory in the system        |
| `effective_cache_size` | should be ~ 2/3 of *available* RAM           |
| `work_mem`             | Depends on max<sub>connections</sub>         |
| `maintenance_work_mem` | Used for vacuum, etc. "256 MB is reasonable" |

- <https://wiki.postgresql.org/wiki/Tuning_Your_PostgreSQL_Server>


## Dealing with table bloat

When you update a table in PostgreSQL, deleting or updating a row leaves behind old rows that were part of the transaction. To reclaim the space and prevent table bloat, you'll need to vacuum the database.

From 8.1 on, there's an autovacuum daemon which can be tuned. Have it aggressively vacuum high-frequency tables.

For really bad table bloat, use CLUSTER (not VACUUM FULL). It requires space for the in-use data while it runs, but is faster and prevents bloated indexes.


## Dump database to tar file

```shell
pg_dump -O -x -F t -b -f [filename.tar] [db name]
```


## Restore database from tar file

```shell
pg_restore -F t -d [db name] [filename.tar]
```


## Create database

```sql
create database
```


## Reindexing

```sql
REINDEX { INDEX | TABLE | DATABASE | SYSTEM } name
```


## Handy URLs

- <http://www.postgresql.org/docs/9.2/static/functions-admin.html>
- <http://www.postgresql.org/docs/9.2/static/monitoring-stats.html>


# MySQL


## Monitor queries

```shell
watch -n 1 mysqladmin --user=<user> --password=<password> processlist
```


## List all databases

```sql
show databases;
```


## List all tables

```sql
show tables;
```


## Describe table contents

```sql
-- Column names
describe TABLE_NAME;
-- Schema
show create table TABLE_NAME;
-- Indexes
show index from TABLE_NAME;
```


## Dump the database schema

```shell
mysqldump --all-databases --no-data
# --skip-add-drop-table
# --skip-comments
```


## Server-side help

```
-- contents
help contents
```


## Check if a server is up

```shell
mysqladmin ping
```


## Create a database

```sql
create database DATABASE_NAME;
```


## Import database

```shell
mysql -u username -p<password> database < filename.sql
```


## Export database

```shell
mysqldump -u username -p<password> database > filename.sql
```


## Delete database

```shell
drop database DATABASE_NAME;
```


## User management

```sql
-- Create a user
CREATE USER 'example_user'@'localhost' IDENTIFIED BY 'example_pass';
-- Revoke permissions for user
REVOKE ALL PRIVILEGES, GRANT OPTION FROM 'example_user'@'localhost';
-- Delete a user
DROP USER 'example_user'@'localhost';
```


## Show grants

```
-- show grants for current user
show grants;
-- show grants for particular user
show grants for 'user'@'example.com';
```


## Variables

```
-- session variables
SHOW SESSION VARIABLES;
SET SESSION sort_buffer_size=1000000;
-- global variables
SHOW GLOBAL VARIABLES;
SET GLOBAL sort_buffer_size=1000000;
```


## Resetting root password

```shell
/etc/init.d/mysql stop
/usr/bin/mysqld_safe --skip-grant-tables &
mysql --user=root mysql
```

```
update user set Password=PASSWORD('new-password-here') WHERE User='root';
flush privileges;
```

```shell
fg
# (ctrl-c to kill mysql)
service mysql start
```


## Create prefix index

```
alter table TABLENAME.COLUMN
add key (COLUMN(n));
```


## See what engine the table uses

```
show table status
like 'table_name' \G
```


## See running processes

```
-- Quick glance
show processlist ;
-- sort by user
select * from information_schema.processlist where user='foobar';
```


## Isolation Levels

| Isolation level  | dirty reads possible | nonrepeatable reads possible | phantom reads possible | locking reads |
|---------------- |-------------------- |---------------------------- |---------------------- |------------- |
| READ UNCOMMITTED | t                    | t                            | t                      | f             |
| READ COMMITTED   | f                    | t                            | t                      | f             |
| REPEATABLE READ  | f                    | f                            | t                      | f             |
| SERIALIZABLE     | f                    | f                            | f                      | t             |

```
SET SESSION TRANSACTION ISOLATION LEVEL [level];
```


## Autocommit

```
SHOW VARIABLES LIKE 'AUTOCOMMIT';
SET AUTOCOMMIT=[0|1]
```


## Repairing

```
check table [tablename]
repair table [tablename]
```


## InnoDB engine

has high overhead, but row-level locking with multiversion concurrency control (MVCC)


## Memory engine

uses table-locking, but is speedy


## Archive engine

- Only uses INSERT and SELECT
- Compresses each new row with zlib
- Low disk I/O
- Ideal for logging


## NDB Cluster Engine

- Consists of data nodes, management nodes, and SQL nodes
- Real-time performance with redunancy and load-balancing capabilities
- Complex joins are slow, but single table lookups can be fast


## Falcon Engine

- Uses MVCC, tries to keep transactions in memory
- (need to see where it's development is at now)


## soliddb engine

- similar to InnoDB


## PBXT (Primebase XT) engine

- Has high write concurrency


## Maria

- (needs to be looked into)


## Optimizing

- Avoid NULL when possible
- `optimize table`


## indexing

- Isolate the query column
- Try to simplify any math, and use literals when possible
- When indexing char columns, try using just a few letters
    - good target is `count(distinct name) / count(*)`


## Timezones

```
-- see what time zones are in use
SELECT @@global.time_zone, @@session.time_zone;
-- set global time zone
SET GLOBAL time_zone = <timezone>;
-- set session time zone
SET time_zone = <timezone>;
```


## mysqladmin

mysqladmin COMMAND

| command    | desc        |
|---------- |----------- |
| flush-logs | rotate logs |
| version    | get version |


## Search for foreign key use

```sql
select * from KEY_COLUMN_USAGE where REFERENCED_TABLE_NAME = 'tbl_name';
```


## Disable super<sub>read</sub><sub>only</sub>

```
set global super_read_only=0
```

super<sub>read</sub><sub>only</sub> <https://www.percona.com/blog/2016/09/27/using-the-super_read_only-system-variable/>


## Good books

- High Performance MySQL by Baron Schwartz, Perter Zaitsev, Vadim Tkachenko


## Links

- [MySQL Docker image](https://hub.docker.com/_/mysql)
- [Percona Docker image](https://hub.docker.com/_/percona)


## MySQL - Configuration


### Variables

```sql
-- session variables
SHOW SESSION VARIABLES;
SET SESSION sort_buffer_size=1000000;
-- global variables
SHOW GLOBAL VARIABLES;
SET GLOBAL sort_buffer_size=1000000;
```


### Get timezone config

```sql
SELECT @@global.time_zone, @@session.time_zone;
```


## network

Port 3306 Plaintext protocol SSL is negotiated in-stream


## performance


### indexing

- Isolate the query column
- Try to simplify any math, and use literals when possible
- When indexing char columns, try using just a few letters
    - good target is `count(distinct name) / count(*)`


### Check the slow query log

Enable this for debugging, don't leave it running if you don't need it.

```
set global slow_query_log = ON|OFF
set global slow_query_log_file = file_name
```

toolkit (<https://www.percona.com/software/database-tools/percona-toolkit>) pt-query-digest /var/log/mysql/mysql-slow.log


### Dealing with fragmentation

```
-- size in MB
select ENGINE, TABLE_NAME, Round(DATA_LENGTH/1024/1024) as data_length, round(INDEX_LENGTH/1024/1024) as index_length, round(DATA_FREE/1024/1024) as data_free, (data_free/(index_length+data_length)) as frag_ratio from information_schema.tables where DATA_FREE > 0 order by frag_ratio desc;
```


### Optimize table

```
optimize table <tbl>;
```

<https://dev.mysql.com/doc/refman/5.7/en/optimize-table.html>


### Good ways to benchmark

- Use a query log to come up with a realistic workload that covers peek time and when batch jobs are run
- Use fresh snapshots between benchmarks
- Full stack tools:
    - ab
    - http<sub>load</sub>
        - `http_load -rate [requests_per_sec] -parallel [num_processes] -seconds [time] [url_file]`
    
    - JMeter

- Single component tools:
    - Database Test Suite (made by OSDL)
    - sql-bench
    - Super Smack


### mysqlslap

Emulates client load


### Run profiling

```
set profiling = 1;
* run query *
show profile;
```


### Resources

- High Performance MySQL by Baron Schwartz, Perter Zaitsev, Vadim Tkachenko


## Production Ready


### Remove all anonymous accounts

```sql
DROP USER ''@'localhost';
DROP USER ''@'hostname';
```


### Remove all non-localhost root users


### Remove test databases

```sql
DELETE FROM mysql.db WHERE Db LIKE 'test%';
FLUSH PRIVILEGES;
DROP DATABASE test;
```


### Set timezone to UTC

For safe measure, don't just set the time zone in MySQL, but also set the system time zone to UTC

```
default-time-zone=UTC
```


### Change the default prompt

```
[mysql]
prompt="(\U) [\d] >"
```

<https://dev.mysql.com/doc/refman/5.6/en/mysql-commands.html>


## replication


### Set the replica binlog coordinates

```sql
CHANGE MASTER TO
  MASTER_HOST='example.com',
  MASTER_LOG_FILE='mysqld-bin.123456',
  MASTER_LOG_POS=123456;
```


### Check the binlog in a human readable way

```shell
mysqlbinlog $BINLOG_FILE
```


### Links

<https://dev.mysql.com/doc/refman/5.6/en/replication-gtids-concepts.html>


## security


### Don't place DB on same instance as application

If the application is vulnerable to a file disclosure attack, it could allow attacker to download the DB files directly. This can be mitigated by proper file permissions, but a locked down ACL on a networked MySQL server is just as good (and makes the application more scalable as well).


## Upgrading

If using InnoDB:

```
set global innodb_fast_shutdown=0
```

> With a slow shutdown, InnoDB performs a full purge and change buffer merge before shutting down, which ensures that data files are fully prepared in case of file format differences between releases.

Run mysql<sub>upgrade</sub>

> mysql<sub>upgrade</sub> should not be used when the server is running with --gtid-mode=ON. See GTID mode and mysql<sub>upgrade</sub> for more information.


## ZFS


### Set the ZFS recordsizes to match InnoDB page size

```
zfs set recordsize=16k  tank/db
zfs set recordsize=128k tank/log
```


### Further reading

- [MySQL InnoDB ZFS Best Practices](https://blogs.oracle.com/realneel/entry/mysql_innodb_zfs_best_practices)


## Engines


### MariaDB - Aria Engine

<https://en.wikipedia.org/wiki/Aria_%28storage_engine%29>


### MySQL - Blackhole Engine

- No storage mechanism at all
- Useful for replication setups and audit logging


### MySQL - CSV Engine

Reads, writes to CSV files


### MySQL - Federated Tables

<http://archive.oreilly.com/pub/a/databases/2006/08/10/mysql-federated-tables.html>


### MySQL - InnoDB Engine

innochecksum - offline file checksum utility


### MySQL - MyISAM engine

- uses table-level locking and lacks transactions, but has low overhead and is platform neutral
- excellent for read-only tables


### Tools

myisam<sub>ftdump</sub> A utility that displays information about full-text indexes in MyISAM tables. See Section 4.6.2, "myisam<sub>ftdump</sub> --- Display Full-Text Index information".

myisamchk A utility to describe, check, optimize, and repair MyISAM tables. See Section 4.6.3, "myisamchk --- MyISAM Table-Maintenance Utility".

myisamlog A utility that processes the contents of a MyISAM log file. See Section 4.6.4, "myisamlog --- Display MyISAM Log File Contents".

myisampack A utility that compresses MyISAM tables to produce smaller read-only tables. See Section 4.6.5, "myisampack --- Generate Compressed, Read-Only MyISAM Tables"

mysqlhotcopy A utility that quickly makes backups of MyISAM tables while the server is running. See Section 4.6.10, "mysqlhotcopy --- A Database Backup Program"..


### MySQL - XtraDB Engine

drop-in replacement for InnoDB binary compatibility with InnoDB database files
