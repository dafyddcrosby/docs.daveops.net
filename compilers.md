# Compilers
# GCC

flag                  | desc
---                   | ---
-S                    | get assembly code
-g                    | debug (for GDB)
-pedantic             | warnings for strict ISO C
-std=c99              | use C99 standard
-Wall                 | show warnings of questionable practices
-Wextra               | warnings not covered by -Wall
-Wformat-y2k          | look for strftime code that uses double-digit year
-Wunreachable-code    | warn for unreachable code
-Wduplicated-cond     | warn about duplicated condition in if-else-if chains
-Wduplicated-branches | warn about duplicate branches in if-else-if chains
-Wlogical-op          | warn where a bitwise operation may have been intended
-Wnull-dereference    | warn where compiler detects path that dereferences a null pointer
-Wjump-misses-init    | warn where a goto jump misses variable initialization
-Wshadow              | warn when a local variable shadows another variable/parameter/type/class member
-Wformat=2            | aggressively check for format errors, warn about possible security bugs

## Links

* <http://gcc.gnu.org/onlinedocs/gcc/Code-Gen-Options.html>
# LLVM

[Github](https://github.com/llvm/llvm-project)

## List available targets

```bash
llc -version
```

## Modules

<http://clang.llvm.org/docs/Modules.html>

## Playing with AST, clang-query and clang-tidy

- https://clang.llvm.org/docs/IntroductionToTheClangAST.html

Note - when compiling from source:

```bash
-DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra"
```

### Dump AST of file

```bash
clang -Xclang -ast-dump -fsyntax-only test.cpp
```

### clang-tidy

Adding a new check

```bash
cd llvm/clang-tools-extra/clang-tidy
python add_new_check.py misc my-first-check
```


- https://devblogs.microsoft.com/cppblog/exploring-clang-tooling-part-2-examining-the-clang-ast-with-clang-query/
- https://devblogs.microsoft.com/cppblog/exploring-clang-tooling-part-3-rewriting-code-with-clang-tidy/
- <https://devblogs.microsoft.com/cppblog/exploring-clang-tooling-part-1-extending-clang-tidy/>

### clang-query

```bash
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

flag      | desc
---       | ---
-D        | disassemble contents of all sections (not just ones expected code)
-b binary | treat target as binary blob
-i        | info - show available architectures and object formats
-m        | set architecture ('machine')

```bash
# Disassemble the executable
objdump -D ./executable
```
