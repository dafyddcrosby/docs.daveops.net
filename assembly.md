---
title: Assembly
---

## Intel Syntax

* Assembler detects type of symbols (no need to add sigil)

```
mnemonic  destination, source
```

Arithmetic expressions in square brackets

## AT&T Syntax

* Register names have a % prefix
* Literal values have a $ prefix

```
mnemonic  source, destination
```

The mnemonic has a suffix indicating the size

suffix | desc
---    | ---
q      | qword
l      | lword
w      | word
b      | byte
