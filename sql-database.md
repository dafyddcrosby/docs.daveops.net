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

* 1992
* 1999
* 2003
* 2008
* new ones?



# SQLite

## Show all tables

```sqlite3
.tables
```

## Show table schema

```sqlite3

 .schema tablename
```

## See if table exists

```sqlite3

 SELECT name 
 FROM sqlite_master
 WHERE type='table'
```

## Export tables

```bash
sqlite3 my.db .dump
```

## Get table sizes

```sqlite
SELECT SUM("pgsize") FROM "dbstat" WHERE name='TABLENAME';
```

## Datatypes


* NULL. The value is a NULL value.
* INTEGER. The value is a signed integer, stored in 1, 2, 3, 4, 6, or 8 bytes depending on the magnitude of the value.
* REAL. The value is a floating point value, stored as an 8-byte IEEE floating point number.
* TEXT. The value is a text string, stored using the database encoding (UTF-8, UTF-16BE or UTF-16LE).
* BLOB. The value is a blob of data, stored exactly as it was input.




# PostgreSQL

## Connect to a shell



 psql

## List databases



 $ psql -l

 # in psql shell
 \l

## Configuration


 
 /var/lib/pgsql/data/postgresql.conf

## View current queries

```sql
SELECT * FROM pg_stat_activity ;
```

## Killing long running queries

Don't use SIGKILL, it will shut down the rest of PostgreSQL, and require a replay

	
	   pg_cancel_backend(pid int) 
	   pg_terminate_backend(pid int)
## Tuning

config               | set
---                  | ---
shared_buffers       | About 1/4 of the memory in the system
effective_cache_size | should be ~ 2/3 of *available* RAM
work_mem             | Depends on max_connections
maintenance_work_mem | Used for vacuum, etc. "256 MB is reasonable"


* <https://wiki.postgresql.org/wiki/Tuning_Your_PostgreSQL_Server>


## Dealing with table bloat

When you update a table in PostgreSQL, deleting or updating a row leaves behind old rows that were part of the transaction. To reclaim the space and prevent table bloat, you'll need to vacuum the database.
From 8.1 on, there's an autovacuum daemon which can be tuned. Have it aggressively vacuum high-frequency tables.

For really bad table bloat, use CLUSTER (not VACUUM FULL). It requires space for the in-use data while it runs, but is faster and prevents bloated indexes.

## Dump database to tar file



 pg_dump -O -x -F t -b -f [filename.tar] [db name]

## Restore database from tar file



 pg_restore -F t -d [db name] [filename.tar]

## Create database


 create database <db_name>

## Reindexing



 REINDEX { INDEX | TABLE | DATABASE | SYSTEM } name

## Handy URLs


* <http://www.postgresql.org/docs/9.2/static/functions-admin.html>
* <http://www.postgresql.org/docs/9.2/static/monitoring-stats.html>



