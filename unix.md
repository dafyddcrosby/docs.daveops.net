---
title: UNIX
---

[The UNIX Heritage Society](http://www.tuhs.org)

Signals
-------

| signal  | # | description             |
|---------|---|-------------------------|
| SIGSTOP |   | Stop the process.       |
| SIGCONT |   | Start a stopped process |

14 character filename limit
---------------------------
* <http://languagelog.ldc.upenn.edu/nll/?p=3496>
* <http://minnie.tuhs.org/cgi-bin/utree.pl?file=V5/usr/sys/user.h>
* <http://minnie.tuhs.org/cgi-bin/utree.pl?file=V5/usr/sys/param.h>

```c
#define	DIRSIZ	14
```

# Processes

## nice

Lowest priority is 20, highest is -19

Only super-user can increase nice value below 0

```bash
# Run at lowest priority
nice 20 <pid>
```


## Filenames

The UNIX time-sharing system (1974)
<http://minnie.tuhs.org/cgi-bin/utree.pl?file=V5/usr/sys>
<https://9p.io/7thEdMan/v7vol2a.pdf> (p44)
<http://languagelog.ldc.upenn.edu/nll/?p=3496>
<http://minnie.tuhs.org/cgi-bin/utree.pl?file=V5/usr/sys/user.h> see u_name
DIRSIZ is defined here: <http://minnie.tuhs.org/cgi-bin/utree.pl?file=V5/usr/sys/param.h>