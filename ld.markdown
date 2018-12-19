# ld
The GNU linker
--------------
::

 # Link object file into an executable file
 ld -o example example.o
 # Strip all binary symbol information
 ld -s ...
 # Strip debug binary symbol information
 ld -S ...
 # Mark stack as non executable
 ld -z noexecstack

OpenBSD ld.so
-------------
::

 # get documentation
 man ld.so
 # get information about what is getting loaded at run-time
 LD_DEBUG=1 ./a.out

