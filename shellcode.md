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
