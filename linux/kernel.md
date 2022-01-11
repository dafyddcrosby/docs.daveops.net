---
title: kernel
---

## Remove older kernels

```bash
# (RHEL) Install yum-utils and run:
package-cleanup --oldkernels --count=1
```

## Links
* [Linux Kernel Analysis](http://www.faqs.org/docs/Linux-HOWTO/KernelAnalysis-HOWTO.html)

# Hardware

```bash
# Get cpu info
lscpu
cat /proc/cpuinfo
```



# kexec

## Using systemctl

```bash
systemctl kexec
```
