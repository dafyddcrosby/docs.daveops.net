---
title: kernel modules
tags: ["Linux"]
---

## basic commands

| desc                           | command         |
|--------------------------------|-----------------|
| list modules                   | `lsmod`           |
| get info about a module        | `modinfo MODULE`  |
| load a module                  | `modprobe MODULE` |
| load kernel module by filename | `insmod FILE`     |
| unload a module                | `modprobe -r MODULE` (or `rmmod MODULE` in a pinch) |

## Resources

* [Linux Loadable Kernel Module](http://www.tldp.org/HOWTO/Module-HOWTO/)
* [Linux Kernel Module Programming](http://www.tldp.org/LDP/lkmpg/2.6/html/)
