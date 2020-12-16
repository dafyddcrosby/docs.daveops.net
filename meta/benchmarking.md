---
title: benchmarking
---

* Don't run off battery power (use mains)
* Disable things like TurboBoost (which temporarily increases CPU speed)
* Disable background processes (like backups)
* Run many times to get a stable measurement
* It might not hurt to reboot and try again

Be aware of subtle floating point rounding errors that can occur from code path
changes (eg hitting the CPU registers vs main memory)
