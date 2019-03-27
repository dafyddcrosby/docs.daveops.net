---
title: replication
tags: ["MySQL", "databases"]
---

Set the replica binlog coordinates
----------------------------------
```sql
CHANGE MASTER TO
  MASTER_HOST='example.com',
  MASTER_LOG_FILE='mysqld-bin.123456',
  MASTER_LOG_POS=123456;
```

Check the binlog in a human readable way
----------------------------------------


  mysqlbinlog <binlog file>


Links
-----
<https://dev.mysql.com/doc/refman/5.6/en/replication-gtids-concepts.html>

