---
title: performance
tags: ["MySQL"]
---

<!-- TODO Avoid NULL when possible (forget why) -->

indexing
--------

* Isolate the query column
* Try to simplify any math, and use literals when possible
* When indexing char columns, try using just a few letters
  * good target is ``count(distinct name) / count(*)``


Check the slow query log
------------------------
Enable this for debugging, don't leave it running if you don't need it.

```mysql
set global slow_query_log = ON|OFF
set global slow_query_log_file = file_name
```



  # Plain-jane MySQL
  mysqldumpslow /var/log/mysql/mysql-slow.log
  # Percona toolkit (<https://www.percona.com/software/database-tools/percona-toolkit>)
  pt-query-digest /var/log/mysql/mysql-slow.log

Dealing with fragmentation
--------------------------

```mysql
-- size in MB
select ENGINE, TABLE_NAME, Round(DATA_LENGTH/1024/1024) as data_length, round(INDEX_LENGTH/1024/1024) as index_length, round(DATA_FREE/1024/1024) as data_free, (data_free/(index_length+data_length)) as frag_ratio from information_schema.tables where DATA_FREE > 0 order by frag_ratio desc;
```

Optimize table
--------------

```mysql
optimize table <tbl>;
```

<https://dev.mysql.com/doc/refman/5.7/en/optimize-table.html>

Good ways to benchmark
----------------------
<!-- TODO - cleanup -->


* Use a query log to come up with a realistic workload that covers peek time and when batch jobs are run
* Use fresh snapshots between benchmarks
* Full stack tools:
  * ab
  * http_load
    * ``http_load -rate [requests_per_sec] -parallel [num_processes] -seconds [time] [url_file]``
  * JMeter
* Single component tools:
  * Database Test Suite (made by OSDL)
  * sql-bench
  * Super Smack


mysqlslap
---------

Emulates client load

Run profiling
-------------

```
set profiling = 1;
* run query *
show profile;
```

Resources
---------

* High Performance MySQL by Baron Schwartz, Perter Zaitsev, Vadim Tkachenko

