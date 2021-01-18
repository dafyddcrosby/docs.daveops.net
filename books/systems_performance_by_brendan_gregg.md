---
title: "Systems Performance by Brendan Gregg"
---

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
