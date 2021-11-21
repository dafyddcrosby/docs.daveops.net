---
title: lsof
---

```bash
# List open files by user
lsof -u $USER
# List open files by process name
lsof -c $NAME
# Use a logical AND for search
lsof -a # ...
# List all network connections
lsof -i
# List all Unix sockets
lsof -U
```
