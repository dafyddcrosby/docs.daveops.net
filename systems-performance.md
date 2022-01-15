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

* Don’t do it.
* Do it, but don’t do it again.
* Do it less.
* Do it later.
* Do it when they’re not looking.
* Do it concurrently.
* Do it more cheaply.
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
