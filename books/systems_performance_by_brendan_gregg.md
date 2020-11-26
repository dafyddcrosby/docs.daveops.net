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
