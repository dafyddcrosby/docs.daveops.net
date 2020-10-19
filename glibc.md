---
title: glibc
---

## Get version

```bash
/lib/libc.so.6
```

or

```c
#include <stdio.h>
#include <gnu/libc-version.h>
int main (void) { puts (gnu_get_libc_version ()); return 0; }
```
