---
title: GDB
---

cmd                        | desc
---                        | ---
!controlling execution     |
r ARG1 ...                 | run program
s                          | step through program
si                         | step one instruction exactly
c                          | continue running program
!breakpoints               |
b FUNCNAME (or line)       | set breakpoint at function
! ...                      |
bt                         | backtrace
bt full                    | backtrace (with locals for each frame)
l                          | list surrounding source
info locals                | show local variables
info args                  | show arguments
up/down N                  | go up or down n frames in the stack
disassemble                | disassemble section of memory
p/x VAR                    | print variable in hexadecimal
x ADDR                     | Examine memory at addr
whatis VAR                 | show variable type
set var VAR=VAL            | Set variable to value
show env                   | Show environment
set env FOO = bar          | Set environment variable FOO to bar
set env VAR=VAL            | Set environment variable to value
attach PID                 | Attach gdb to already running process
set print pretty on        | pretty print structures
set print array on         | readable array output
set print array-indexes on | show array indices
set print demangle on      | demangle C++ names
info registers             | show registers (excluding floating point)
macro expand MACRO         | expands a macro (requires -gdwarf-2 -g3 compiler flags)

code | print type
---  | ---
x    | hexadecimal
u    | unsigned int
d    | signed int
o    | octal
u    | unsigned decimal
t    | binary
f    | floating point
a    | address
c    | char
s    | string


## Check a core dump

```bash
gdb executable coredump
```

## Extensions

<http://rr-project.org/>

