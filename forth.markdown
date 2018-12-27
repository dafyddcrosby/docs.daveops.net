---
title: Forth
---

| words | action                        |
|-------|-------------------------------|
| .     | pop an element off the stack  |
| .s    | display the contents of stack |
| drop  | drop top of stack             |
| \ ... | comment                       |
| see   | decompile                     |

words | action
include <filename> | includes a forth source file

colon definitions
-----------------

```
: funcname ( stack effect comment )
  do stuff ;

\ local variables
: funcname { a b -- b a }
  b a ;
```

