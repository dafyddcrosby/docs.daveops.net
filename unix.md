# UNIX

[The UNIX Heritage Society](http://www.tuhs.org)

## Signals

signal  | description
---     | ---
SIGSTOP | Stop the process.
SIGCONT | Start a stopped process

## 14 character filename limit
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


## Unix

- <https://www.bell-labs.com/usr/dmr/www/odd.html>
- [shasm](http://lists.gnu.org/archive/html/bug-bash/2001-02/msg00054.html)# sudo


# sudo

Syntax: 
```
<user(group)> <host(group)> = (<list of users to run as|ALL>) <PASSWD|EXEC|SETENV|LOG_INPUT|LOG_OUTPUT>: <commands|ALL>
```

# Systrace

http://www.citi.umich.edu/u/provos/systrace/

Used to limit system calls

## Automatically generate systrace profile
```bash
systrace -A /path/to/executable
```
# procfs
The process information pseudo-file system

## Linux

man 5 proc


* /proc/partitions

  list of partitions

