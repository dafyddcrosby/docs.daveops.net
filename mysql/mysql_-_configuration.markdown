---
title: MySQL - Configuration
tags: ["MySQL"]
---

Variables
---------

```sql
-- session variables
SHOW SESSION VARIABLES;
SET SESSION sort_buffer_size=1000000;
-- global variables
SHOW GLOBAL VARIABLES;
SET GLOBAL sort_buffer_size=1000000;
```

Get timezone config
-------------------

```sql
SELECT @@global.time_zone, @@session.time_zone;
```
