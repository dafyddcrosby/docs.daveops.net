dtrace
======
:date: 2016-12-14

::

  # list probes
  dtrace -l
  # list syscall
  dtrace -l -n syscall:::entry
  # get all syscalls occurring
  dtrace -n syscall:::'{ trace(execname) ;}'


