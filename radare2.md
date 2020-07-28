---
title: Radare2
---

```bash
# Get binary info
rabin2 -I FILE

# Examine executable
radare2 FILE
```

## Cheatsheet

### Visual mode

key | thing
--- | ---
V   | visual mode
VV  | visual graph
c   | cursor mode


### Analysis

key             | thing
---             | ---
aa              | analyze "all"  (good for most debugging)
aaa             | analyze all
afl             | analyze function list
afn name [addr] | rename function [at address]
afvn name       | rename argument/local
axt [addr]      | find data/code references to this address

### Info

Interesting offsets (sections, functions, symbols, etc) are called "flags"

key     | thing
---     | ---
ie      | get entrypoint
iz      | list strings in data sections
izz     | search for all strings in binary
ii      | get imports
iI      | binary information
iS      | display sections
f       | show flags in flagspace
fs FLAG | change to different flagspace

### Projects

key       | thing
---       | ---
Ps <name> | Save project
Po <name> | Open project
. <name>  | Interpret script


### Disassembly

key | thing
--- | ---
pdf | print disassemble function

## Resources

* https://radare.gitbooks.io/radare2book/content/
