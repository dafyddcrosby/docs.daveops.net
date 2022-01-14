# objdump

flag      | desc
---       | ---
-D        | disassemble contents of all sections (not just ones expected code)
-b binary | treat target as binary blob
-i        | info - show available architectures and object formats
-m        | set architecture ('machine')

```bash
# Disassemble the executable
objdump -D ./executable
```
