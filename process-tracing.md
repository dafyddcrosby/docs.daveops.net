# Process Tracing

# strace

flag          | description
---           | ---
-f            | trace child processes
-e trace=file | show only file operations
-i            | print instruction pointer at time of syscall
-y            | print paths associated with file descriptor args

# dtrace

```bash
# list probes
dtrace -l
# list syscall
dtrace -l -n syscall:::entry
# get all syscalls occurring
dtrace -n syscall:::'{ trace(execname) ;}'
```

## Links

* <http://www.brendangregg.com/DTrace/dtrace_oneliners.txt>
# lsof

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
