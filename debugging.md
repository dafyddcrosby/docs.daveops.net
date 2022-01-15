# Debugging

## Bryan Cantrill

[Debugging Under Fire (YouTube)](https://www.youtube.com/watch?v=30jNsCVLpAE)

> Debugging is the process by which we understand pathological behavior in a software system

> The essence of debugging is asking and answering questions - and the craft of writing debuggable software is allowing the software to be able to answer questions about itself.

> Recovery in lieu of understanding normalizes broken software.

## Mike Perham

[The Three Best Debugging Tools](https://www.mikeperham.com/2013/09/12/the-three-best-debugging-tools/)

1. Your coworker
2. Your 'creative side'
3. Your Mental Model of the System

I agree a lot with #3 - often when I'm helping someone debugging, it's helping them establish this mental model.

# GDB

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

# LLDB

[LLDB <-> GDB Cheatsheet](https://lldb.llvm.org/lldb-gdb.html)

command        | description
---            | ---
r ARGS         | run program (with arguments)
s              | step
env FOO=BAR    | set environment variable
frame variable | list local variables
bt             | backtrace
disassemble -f | disassemble frame
