# Fuzzing
# American Fuzzy Lop

<http://lcamtuf.coredump.cx/afl/>

```
CC=/path/to/afl/afl-gcc ./configure
afl-fuzz -i testcase_dir -o findings_dir /path/to/program [...params...]
```
