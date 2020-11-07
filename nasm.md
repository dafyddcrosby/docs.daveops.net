---
title: nasm
---

```bash
# Valid output formats
nasm -hf
# don't preprocess (assemble only)
nasm -a ...
# assemble to raw binary
nasm -f bin
# assemble to ELF64 (Linux)
nasm -f elf64 example.asm
# assemble to macho64 (macOS)
nasm -f macho64 example.asm
```

```asm
; Set to 32-bit
BITS 32
```
