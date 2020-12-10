---
title: osquery
---

## Get processes where the path no longer exists

```sql
select pid,path,uid from processes where on_disk=0;
```

* [Schema](https://osquery.io/schema/)
