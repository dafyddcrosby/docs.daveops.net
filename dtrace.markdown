# dtrace
::

  # list probes
  dtrace -l
  # list syscall
  dtrace -l -n syscall:::entry
  # get all syscalls occurring
  dtrace -n syscall:::'{ trace(execname) ;}'

Links
-----


<http://www.brendangregg.com/DTrace/dtrace_oneliners.txt>

