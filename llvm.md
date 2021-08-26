---
title: llvm
---

## List available targets

```bash
llc -version
```

## Modules

<http://clang.llvm.org/docs/Modules.html>

## Playing with AST, clang-query and clang-tidy

Note - when compiling from source:

```
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

### clang-query

Dump the AST

```
set output dump
```

Reduce noise (not always reliable)

```
set bind-root false
```

### Links
* https://clang.llvm.org/docs/IntroductionToTheClangAST.html
* https://devblogs.microsoft.com/cppblog/exploring-clang-tooling-part-1-extending-clang-tidy/
* https://devblogs.microsoft.com/cppblog/exploring-clang-tooling-part-2-examining-the-clang-ast-with-clang-query/
* https://devblogs.microsoft.com/cppblog/exploring-clang-tooling-part-3-rewriting-code-with-clang-tidy/
