---
title: ld
---

# The GNU linker

* `/etc/ld.so.conf` configures directories to search
* `/etc/ld.so.cache` is the binary cache used by ld.so

```bash
# Reload ld cache
ldconfig
# See which libraries are utilized
ldconfig -v

# See what an executable links to
ldd ./executablefile

# Specify an alternate library path
export LD_LIBRARY_PATH=/path/to/dir ...

# Link object file into an executable file
ld -o example example.o
# Strip all binary symbol information
ld -s ...
# Strip debug binary symbol information
ld -S ...
# Mark stack as non executable
ld -z noexecstack
```
