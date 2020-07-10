---
title: nasm
---

```bash
# Valid output formats
nasm -hf
# don't preprocess (assemble only)
nasm -a ...
# assemble to ELF64
nasm -f elf64 example.asm
# assemble to macho64 (macOS)
nasm -f macho64 example.asm
```
