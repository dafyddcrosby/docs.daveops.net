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
