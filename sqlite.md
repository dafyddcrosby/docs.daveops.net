---
title: SQLite
tags: ["databases"]
---

Show all tables
---------------

```sqlite3
.tables
```

Show table schema
-----------------

```sqlite3

 .schema tablename
```

See if table exists
-------------------

```sqlite3

 SELECT name 
 FROM sqlite_master
 WHERE type='table'
```

Export tables
-------------



  sqlite3 my.db .dump

Datatypes
---------


* NULL. The value is a NULL value.
* INTEGER. The value is a signed integer, stored in 1, 2, 3, 4, 6, or 8 bytes depending on the magnitude of the value.
* REAL. The value is a floating point value, stored as an 8-byte IEEE floating point number.
* TEXT. The value is a text string, stored using the database encoding (UTF-8, UTF-16BE or UTF-16LE).
* BLOB. The value is a blob of data, stored exactly as it was input.


