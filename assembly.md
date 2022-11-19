# Assembly


## Intel Syntax

- Assembler detects type of symbols (no need to add sigil)

```
mnemonic  destination, source
```

Arithmetic expressions in square brackets


## AT&T Syntax

- Register names have a % prefix
- Literal values have a $ prefix

```
mnemonic  source, destination
```

The mnemonic has a suffix indicating the size

| suffix | desc  |
|------ |----- |
| q      | qword |
| l      | lword |
| w      | word  |
| b      | byte  |


# VASM assembler

<http://sun.hasenbraten.de/vasm/>


## Building

```shell
# Build assembler for 68000 with motorola syntax
make CPU=m68k SYNTAX=mot
```


# nasm

```shell
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


# shellcode


## Dealing with null bytes

Since strcpy stops at a null byte, you'll need to adjust your instructions to avoid it

```asm
; moving zero into a register
; null byte
mov eax, 0
; no null byte
xor eax, eax
```