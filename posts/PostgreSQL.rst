PostgreSQL
----------


Configuration
==============================
{{{/var/lib/pgsql/data/postgresql.conf}}}

Tuning
==============================

+----------------------+----------------------------------------------+
| config               | set                                          |
+======================+==============================================+
| shared_buffers       | About 1/4 of the memory in the system        |
+----------------------+----------------------------------------------+
| effective_cache_size | should be ~ 2/3 of //available// RAM         |
+----------------------+----------------------------------------------+
| work_mem             | Depends on max_connections                   |
+----------------------+----------------------------------------------+
| maintenance_work_mem | Used for vacuum, etc. "256 MB is reasonable" |
+----------------------+----------------------------------------------+

Sources:
* https://wiki.postgresql.org/wiki/Tuning_Your_PostgreSQL_Server

Dealing with table bloat
==============================
When you update a table in PostgreSQL, deleting or updating a row leaves behind old rows that were part of the transaction. To reclaim the space and prevent table bloat, you'll need to vacuum the database.
From 8.1 on, there's an autovacuum daemon which can be tuned. Have it aggressively vacuum high-frequency tables.

For really bad table bloat, use CLUSTER (not VACUUM FULL). It requires space for the in-use data while it runs, but is faster and prevents bloated indexes.

Dump database to tar file
==============================
{{{
pg_dump -O -x -F t -b -f [filename.tar] [db name]
}}}
Restore database from tar file
==============================
{{{
pg_restore -F t -d [db name] [filename.tar]
}}}
List databases
==============================
{{{
psql -l
}}}

