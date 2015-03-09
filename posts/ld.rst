ld
==
:date: 2015-03-09

The GNU linker

::

 # Link object file into an executable file
 ld -o example example.o
 # Strip all binary symbol information
 ld -s ...
 # Strip debug binary symbol information
 ld -S ...
 # Mark stack as non executable
 ld -z noexecstack
