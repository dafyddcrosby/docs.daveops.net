
# Compilers

# GCC

| flag                  | desc                                                                            |
|--------------------- |------------------------------------------------------------------------------- |
| -S                    | get assembly code                                                               |
| -g                    | compile with debug symbols (for GDB)                                            |
| -pedantic             | warnings for strict ISO C                                                       |
| -std=c99              | use C99 standard                                                                |
| -Wall                 | show warnings of questionable practices                                         |
| -Wextra               | warnings not covered by -Wall                                                   |
| -Wformat-y2k          | look for strftime code that uses double-digit year                              |
| -Wunreachable-code    | warn for unreachable code                                                       |
| -Wduplicated-cond     | warn about duplicated condition in if-else-if chains                            |
| -Wduplicated-branches | warn about duplicate branches in if-else-if chains                              |
| -Wlogical-op          | warn where a bitwise operation may have been intended                           |
| -Wnull-dereference    | warn where compiler detects path that dereferences a null pointer               |
| -Wjump-misses-init    | warn where a goto jump misses variable initialization                           |
| -Wshadow              | warn when a local variable shadows another variable/parameter/type/class member |
| -Wformat=2            | aggressively check for format errors, warn about possible security bugs         |

- <http://gcc.gnu.org/onlinedocs/gcc/Code-Gen-Options.html>


# LLVM

[Github](https://github.com/llvm/llvm-project)


## Intermediate Representation


## List available targets

```shell
llc -version
```


## Modules

<http://clang.llvm.org/docs/Modules.html>


## Playing with AST, clang-query and clang-tidy

- <https://clang.llvm.org/docs/IntroductionToTheClangAST.html>

Note - when compiling from source:

```shell
-DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra"
```


### Dump AST of file

```shell
clang -Xclang -ast-dump -fsyntax-only test.cpp
```


### clang-tidy

Adding a new check

```shell
cd llvm/clang-tools-extra/clang-tidy
python add_new_check.py misc my-first-check
```

- <https://devblogs.microsoft.com/cppblog/exploring-clang-tooling-part-1-extending-clang-tidy/>
- <https://devblogs.microsoft.com/cppblog/exploring-clang-tooling-part-2-examining-the-clang-ast-with-clang-query/>
- <https://devblogs.microsoft.com/cppblog/exploring-clang-tooling-part-3-rewriting-code-with-clang-tidy/>


### clang-query

```shell
clang-query test.cpp
```

Dump the AST on a match:

```
set output dump
```

Reduce noise (not always reliable):

```
set bind-root false
```

Matching AST nodes:

```
// Match a for statement
m forStmt
// Match the 'pow' function
m functionDecl(hasName('pow'))
```

- [LibAST Matchers Reference](https://clang.llvm.org/docs/LibASTMatchersReference.html)


# objdump

| flag      | desc                                                               |
|--------- |------------------------------------------------------------------ |
| -D        | disassemble contents of all sections (not just ones expected code) |
| -b binary | treat target as binary blob                                        |
| -i        | info - show available architectures and object formats             |
| -m        | set architecture ('machine')                                       |

```shell
# Disassemble the executable
objdump -D ./executable
```


# Hotpatching

`MOV EDI, EDI` is used because it's a safe two-byte NOP that can be used across x86-compatible manufactures - according to Raymond Chen, this was even figured out by *talking* to the manufacturers.


## Some notes on NOP

> if you want to get technical, there isn't really any difference between those two. Since the processor doesn't stop, a NOP does *something*, regardless of how its implemented --- at the bare minimum, it increments the instruction pointer. Selecting a "good" NOP is harder than expected because 1. it must do nothing observable to a program, 2. it must work on all processor models we want to support, 3. if it slows down the program, it should do so to the least extent possible. As the discussion here shows, doing nothing well can be surprisingly hard. Perhaps the Tao Te Ching should be consulted, in addition to any processor manuals. -Jeroen Mostert

> Why not just use two NOP instructions at the entry point?
> 
> Well, because a NOP instruction consumes one clock cycle and one pipe, so two of them would consume two clock cycles and two pipes. (The instructions will likely be paired, one in each pipe, so the combined execution will take one clock cycle.) On the other hand, the MOV EDI, EDI instruction consumes one clock cycle and one pipe. (In practice, the instruction will occupy one pipe, leaving the other available to execute another instruction in parallel. You might say that the instruction executes in half a cycle.) However you calculate it, the MOV EDI, EDI instruction executes in half the time of two NOP instructions.
> 
> On the other hand, the five NOPs inserted before the start of the function are never executed, so it doesn't matter what you use to pad them. It could've been five garbage bytes for all anybody cares.
> 
> But much more important than cycle-counting is that the use of a two-byte NOP avoids the Detours problem: If the code had used two single-byte NOP instructions, then there is the risk that you will install your patch just as a thread has finished executing the first single-byte NOP and is about to begin executing the second single-byte NOP, resulting in the thread treating the second half of your JMP $-5 as the start of a new instruction. -Raymond Chen


## Windows

Add `/hotpatch` to the compiler flags.


## Links

- <https://blogs.msdn.microsoft.com/ishai/2004/06/24/why-does-the-compiler-generate-a-mov-edi-edi-instruction-at-the-beginning-of-functions/>
- <https://blogs.msdn.microsoft.com/oldnewthing/20110921-00/?p=9583/>
- <https://blogs.msdn.microsoft.com/oldnewthing/20130102-00/?p=5663/>


# Control Flow Integrity

<https://blog.trailofbits.com/2016/10/17/lets-talk-about-cfi-clang-edition/> <https://clang.llvm.org/docs/ControlFlowIntegrity.html>


# Compilers Explorer

<https://godbolt.org/>