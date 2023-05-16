
# Systems Performance

# "Systems Performance by Brendan Gregg"

[60 second Linux perf troubleshooting](https://netflixtechblog.com/linux-performance-analysis-in-60-000-milliseconds-accc10403c55)

```shell
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

- Don't do it
- Do it, but don't do it again
- Do it less
- Do it later
- Do it when they're not looking
- Do it concurrently
- Do it more cheaply


# sysbench


## benchmark CPU

```shell
sysbench --test=cpu --cpu-max-prime=20000 run
```


## benchmark I/O

```shell
sysbench --test=fileio --file-total-size=10G prepare
sysbench --test=fileio --file-total-size=10G --file-test-mode=rndrw --init-rnd=on --max-time=300 --max_requests=0 run
sysbench --test=fileio --file-total-size=10G cleanup
```


# ftrace


## trace-cmd

```shell
# list available plugins/events
trace-cmd list
```


# perf

<https://perf.wiki.kernel.org/index.php/Main_Page>


# benchmarking

- Don't run off battery power (use mains)
- Disable things like TurboBoost (which temporarily increases CPU speed)
- Disable background processes (like backups)
- Run many times to get a stable measurement
- It might not hurt to reboot and try again

Be aware of subtle floating point rounding errors that can occur from code path changes (eg hitting the CPU registers vs main memory)


# eBPF

- kprobe - a probe that fires on kernel function entry
- uprobe - a probe that fires on user-level program function entry
- USDT (user-level statically defined tracing) - a designated trace point for operations to allow for function name changes/inlining
- tracepoint - a kernel-level USDT


## bpftrace

```shell
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

| var         | desc                                     |
|----------- |---------------------------------------- |
| pid         | process id                               |
| tid         | thread id                                |
| uid         | user id                                  |
| username    | username                                 |
| comm        | process or command name                  |
| curtask     | current task<sub>struct</sub> as u64     |
| nsecs       | current time in nanoseconds              |
| elapsed     | time in nanoseconds since bpftrace start |
| kstack      | kernel stack trace                       |
| ustack      | user-level stack trace                   |
| arg0...argn | function arguments                       |
| args        | tracepoint arguments                     |
| retval      | function return value                    |
| func        | function name                            |
| probe       | full probe name                          |

types:

| var         | desc         |
|----------- |------------ |
| @name       | global       |
| @name [key] | hash (map)   |
| @name [tid] | thread-local |
| $name       | scratch      |

- [bpftrace reference guide](https://github.com/iovisor/bpftrace/blob/master/docs/reference_guide.md)
- [Brendan Gregg bpftrace cheatsheet](https://brendangregg.com/BPF/bpftrace-cheat-sheet.html)


## bpftool

```shell
# show loaded bpf programs
bpftool prog show

# dump BPF instructions of a program (here 123)
bpftool prog dump xlated id 123
```


# USE methodology

- Utilization - The percentage of resources used before performance is impacted
- Saturation - The threshold where performance drops due to resource contention, etc.
- Errors - The threshold where errors begin to surface.

100% utilization isn't a problem if there's no saturation/errors. When looking for performance bottlenecks, look for saturation/errors.


# Don't make changes until you've profiled

Assuming code performance is a power law, a small percentage of LOC will actually affect the over runtime of the program. If you aren't profiling your code, you have a small percentage chance of affecting the runtime performance.


# Using `time`

| desc                   | field |
|---------------------- |----- |
| time spent in kernel   | sys   |
| time spent in userland | user  |
| stopwatch time         | real  |

note that sys and user combined don't necessarily equal real (CPU has other processes to deal with, etc)


# Latency numbers


## Latency Comparison Numbers (Jeff Dean ~2012)

| L1 cache reference                 | 0.5 ns           |            |        |                             |
| Branch mispredict                  | 5   ns           |            |        |                             |
| L2 cache reference                 | 7   ns           |            |        | 14x L1 cache                |
| Mutex lock/unlock                  | 25   ns          |            |        |                             |
| Main memory reference              | 100   ns         |            |        | 20x L2 cache, 200x L1 cache |
| Compress 1K bytes with Zippy       | 3,000   ns       | 3 us       |        |                             |
| Send 1K bytes over 1 Gbps network  | 10,000   ns      | 10 us      |        |                             |
| Read 4K randomly from SSD\*        | 150,000   ns     | 150 us     |        | ~1GB/sec SSD                |
| Read 1 MB sequentially from memory | 250,000   ns     | 250 us     |        |                             |
| Round trip within same datacenter  | 500,000   ns     | 500 us     |        |                             |
| Read 1 MB sequentially from SSD\*  | 1,000,000   ns   | 1,000 us   | 1 ms   | ~1GB/sec SSD, 4X memory     |
| Disk seek                          | 10,000,000   ns  | 10,000 us  | 10 ms  | 20x datacenter roundtrip    |
| Read 1 MB sequentially from disk   | 20,000,000   ns  | 20,000 us  | 20 ms  | 80x memory, 20X SSD         |
| Send packet CA->Netherlands->CA    | 150,000,000   ns | 150,000 us | 150 ms |                             |