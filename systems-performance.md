# Systems Performance
# "Systems Performance by Brendan Gregg"

## 60 second Linux perf troubleshooting

<https://netflixtechblog.com/linux-performance-analysis-in-60-000-milliseconds-accc10403c55>

```bash
uptime
dmesg | tail
vmstat 1
mpstat -P ALL 1
pidstat 1
iostat -xz 1
free -m
sar -n DEV 1
sar -n TCP,ETCP 1
top
```

## Performance tuning

Most to least effective

* Don’t do it
* Do it, but don’t do it again
* Do it less
* Do it later
* Do it when they’re not looking
* Do it concurrently
* Do it more cheaply

# sysbench

## benchmark CPU

```bash
sysbench --test=cpu --cpu-max-prime=20000 run
```

## benchmark I/O
 
```bash
sysbench --test=fileio --file-total-size=10G prepare
sysbench --test=fileio --file-total-size=10G --file-test-mode=rndrw --init-rnd=on --max-time=300 --max_requests=0 run
sysbench --test=fileio --file-total-size=10G cleanup
```

# ftrace

## trace-cmd

```bash
# list available plugins/events
trace-cmd list
```
# perf

<https://perf.wiki.kernel.org/index.php/Main_Page>

# benchmarking

* Don't run off battery power (use mains)
* Disable things like TurboBoost (which temporarily increases CPU speed)
* Disable background processes (like backups)
* Run many times to get a stable measurement
* It might not hurt to reboot and try again

Be aware of subtle floating point rounding errors that can occur from code path
changes (eg hitting the CPU registers vs main memory)

# eBPF

- kprobe - a probe that fires on kernel function entry
- uprobe - a probe that fires on user-level program function entry
- USDT (user-level statically defined tracing) - a designated trace point for operations to allow for function name changes/inlining
- tracepoint - a kernel-level USDT

## bpftrace

```bash
# list all syscall tracepoints
bpftrace -l 'tracepoint:syscalls:*'

# run a bpftrace program
bpftrace -e 'tracepoint:syscalls:sys_enter_openat {printf "%s\n", comm}'

# get BPF instructions
bpftrace -v program.bt
```

```
probe /filter/ { action }
```

builtins:

var         | desc
---         | ---
pid         | process id
tid         | thread id
uid         | user id
username    | username
comm        | process or command name
curtask     | current task_struct as u64
nsecs       | current time in nanoseconds
elapsed     | time in nanoseconds since bpftrace start
kstack      | kernel stack trace
ustack      | user-level stack trace
arg0...argn | function arguments
args        | tracepoint arguments
retval      | function return value
func        | function name
probe       | full probe name

types:

var        | desc
---        | ---
@name      | global
@name[key] | hash (map)
@name[tid] | thread-local
$name      | scratch

- [bpftrace reference guide](https://github.com/iovisor/bpftrace/blob/master/docs/reference_guide.md)
- [Brendan Gregg bpftrace cheatsheet](https://brendangregg.com/BPF/bpftrace-cheat-sheet.html)

## bpftool

```bash
# show loaded bpf programs
bpftool prog show

# dump BPF instructions of a program (here 123)
bpftool prog dump xlated id 123
```
