# Debugging


# GDB

Execution

| full             | short | desc                                                      |
|---------------- |----- |--------------------------------------------------------- |
| run ARG1         | r     | run program                                               |
| attach PID       |       | attach gdb to already running process                     |
| next             | n     | step to next source line (ie don't step in to subroutine) |
| step             | s     | step through program (steps into subroutines)             |
| stepi            | si    | step one instruction exactly                              |
| continue         | c     | continue running program                                  |
| break LOC        | b     | set breakpoint at function/line                           |
| tbreak LOC       |       | set breakpoint that breaks only once                      |
| watch EXPR       |       | stop execution when a variable changes                    |
| rwatch EXPR      |       | stop execution when a variable is read                    |
| info breakpoints | i b   | list of breakpoints                                       |
| disable NUM      | dis   | disable a breakpoint                                      |
| kill             |       | stop execution of program                                 |

Stack info

| full           | short   | desc                                   |
|-------------- |------- |-------------------------------------- |
| backtrace      | bt      | backtrace                              |
| backtrace full | bt full | backtrace (with locals for each frame) |
| list           | l       | list surrounding source                |
| info locals    | i lo    | show local variables                   |
| info args      | i ar    | show arguments                         |
| info frame     | i f     | show info about stack frame            |
| finish         | fin     | return from function                   |
| call           |         | call function linked into program      |
| up N           |         | go up n frames in the stack            |
| down N         |         | go down n frames in the stack          |

Examine memory:

| cmd               | desc                                      |
|----------------- |----------------------------------------- |
| disassemble       | disassemble section of memory             |
| p/x VAR           | print variable in hexadecimal             |
| x ADDR            | Examine memory at addr                    |
| whatis VAR        | show variable type                        |
| set var VAR=VAL   | Set variable to value                     |
| info registers    | show registers (excluding floating point) |
| show env          | Show environment                          |
| set env FOO = bar | Set environment variable FOO to bar       |
| set env VAR=VAL   | Set environment variable to value         |

| cmd                        | desc                                                    |
|-------------------------- |------------------------------------------------------- |
| set print pretty on        | pretty print structures                                 |
| set print array on         | readable array output                                   |
| set print array-indexes on | show array indices                                      |
| set print demangle on      | demangle C++ names                                      |
| macro expand MACRO         | expands a macro (requires -gdwarf-2 -g3 compiler flags) |
| source FILE                | source a GDB macro file                                 |

| code | print type       |
|---- |---------------- |
| x    | hexadecimal      |
| u    | unsigned int     |
| d    | signed int       |
| o    | octal            |
| u    | unsigned decimal |
| t    | binary           |
| f    | floating point   |
| a    | address          |
| c    | char             |
| s    | string           |

Examine a core dump:

```shell
gdb executable coredump
```


## Extensions

<http://rr-project.org/>


# LLDB

[LLDB <-> GDB Cheatsheet](https://lldb.llvm.org/lldb-gdb.html)

| command        | description                  |
|-------------- |---------------------------- |
| r ARGS         | run program (with arguments) |
| s              | step                         |
| env FOO=BAR    | set environment variable     |
| frame variable | list local variables         |
| bt             | backtrace                    |
| disassemble -f | disassemble frame            |


# Bryan Cantrill

[Debugging Under Fire (YouTube)](https://www.youtube.com/watch?v=30jNsCVLpAE)

> Debugging is the process by which we understand pathological behavior in a software system

> The essence of debugging is asking and answering questions - and the craft of writing debuggable software is allowing the software to be able to answer questions about itself.

> Recovery in lieu of understanding normalizes broken software.


# Mike Perham

[The Three Best Debugging Tools](https://www.mikeperham.com/2013/09/12/the-three-best-debugging-tools/)

1. Your coworker
2. Your 'creative side'
3. Your Mental Model of the System

I agree a lot with #3 - often when I'm helping someone debugging, it's helping them establish this mental model.