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

| key | thing       |
|-----|-------------|
| V   | visual mode |
| VV  | visual graph |
| ie  | get entrypoint |
| aa  | analyze "all"  (good for most debugging) |
| aaa | analyze all    |
| afl | analyze function list |
| fs FLAG | change to different flagspace |
| f | show flags in flagspace |
| iz | list strings in data sections |
| izz | search for all strings in binary |
| axt [addr] | find data/code references to this address |
| pdf | print disassemble function |

Interesting offsets (sections, functions, symbols, etc) are called "flags"

## Resources

* https://radare.gitbooks.io/radare2book/content/
