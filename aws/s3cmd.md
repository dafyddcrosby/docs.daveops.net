---
title: s3cmd
---

## set new ACL

```bash
s3cmd setacl s3://BUCKET/OBJECT --acl-grant=[read|write|read_acp|write_acp|full_control|all]:USER_CANONICAL_ID [--recursive]
```
