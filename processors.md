# Processors

[Unicorn CPU emulator](http://www.unicorn-engine.org/)



# TIS-100
Tessellated Intelligence System

## System details
Massively parallel computer architecture comprised of non-uniformly interconnected heterogeneous nodes

### Node Type T21 - Basic Execution Node

* After executing the last instruction of the program, execution automatically continues to the first instruction.
* All registers store integer values between -999 and 999 (inclusive).
* Comments start with #, text after ## is used as the title of the program.
* Labels are used for jump instructions.

### TIS-100 Registers

Register | Notes
---      | ---
ACC      | Accumulator register.
BAK      | Non-addressible temporary storage for values in ACC.


### TIS-100 Ports

Port                  | Notes
---                   | ---
LEFT, RIGHT, UP, DOWN | Communication register for topologically adjacent nodes. Blocks indefinitely if used until transaction with other node occurs.
ANY                   | Reads or writes a value that becomes available on ANY port. (TODO - deterministic which node reads ANY value first?)
LAST                  | Refers to the port last read or written using the ANY pseudo-port.


### TIS-100 Instructions

Instruction      | Description
---              | ---
NOP              | pseudo-intruction converted to ADD NIL.
MOV <SRC>, <DST> | SRC is read and written to DST.
SWP              | Exchange ACC and BAK.
SAV              | Write ACC to BAK.
ADD <SRC>        | Add SRC to ACC, store result in ACC. (TODO - overflow behaviour?)
SUB <SRC>        | Subtract SRC from ACC, store result in ACC. (TODO - underflow behaviour?)
NEG              | The value of ACC is arithmetically negated. Zero remains the same.
JMP <LABEL>      | Transfer execution to instruction after LABEL.
JEZ <LABEL>      | Transfer execution to instruction after LABEL if value of ACC is zero.
JNZ <LABEL>      | Transfer execution to instruction after LABEL if value of ACC is not zero.
JGZ <LABEL>      | Transfer execution to instruction after LABEL if value of ACC is greater than zero.
JLZ <LABEL>      | Transfer execution to instruction after LABEL if value of ACC is less than zero.
JRO <SRC>        | Transfer execution to instruction at offset specified by SRC relative to current instruction.
HCF              | Halt and Catch Fire (undocumented)


Node Type T30 - Stack Memory Node
---------------------------------

Node Type T31 - Random Access Memory Node
-----------------------------------------

Debugger
--------

Visualization Module
--------------------



# Motorola 68000

Note: This information is generally geared to the 68000 specifically (for use with the Sega Genesis), later processor models have more/enhanced instructions :-)

## ISA

field | desc
---   | ---
# X    | immediate (raw) data
ea    | effective address (data register or memory address)
An    | address register
Dn    | data register
Rn    | any register

Mnemonic     | Sizes | Desc
---          | ---   | ---
add ea,ea    | b,w,l | Add source to destination (either needs to be Dn)
addi #X,ea   | b,w,l | Add source to destination (either needs to be Dn)
bCC label    | b,w   | See Condition Code test table (68020+ supports long)
bchg Dn,ea   | b,l   | test a bit in destination operand, set Z code, flip bit
bclr Dn,ea   | b,l   | test a bit in destination operand, set Z code, set bit to 0
bset Dn,ea   | b,l   | test a bit in destination operand, set Z code, set bit to 1
clr ea       | b,w,l | Zero out destination
eor Dn,ea    | b,w,l | Exclusive OR (XOR)
exg Rn       | l     | Exchange registers
lea ea,An    | l     | Load effective address into an address register
move ea,ea   | b,w,l | Move source ea to dest ea (assembler converts to movea if uses direct register)
movea ea,An  | w,l   | Move ea to address register
moveq #x, Dn | l     | Move immediate long to data register
not ea       | b,w,l | Flip bits of effective address
swap Dn      | w     | Swap upper word with lower word
tst ea       | b,w,l | test an operand (sets Z if operand zero)

Only byte-sizes can use odd addressing (word+long crash).

### Condition Code Tests

Mnemonic | Condition
---      | ---
CC(HI)   | Carry Clear
CS(LO)   | Carry Set
EQ       | Equal
GE       | Greater or Equal
GT       | Greater Than
HI       | High
LE       | Less or Equal
LS       | Low or Same
LT       | Less Than
MI       | Minus
NE       | Not Equal
PL       | Plus
VC       | Overflow Clear
VS       | Overflow Set

### Condition codes

code | condition
---  | ---
X    | ?
N    | Negative
Z    | Zero
V    | Overflow
C    | Carry

## Motorola assembly syntax

```asm
; mnemonic.size source,destination
```

Size is 'b' (byte), 'w' (word), or 'l' (long)

operand    | desc
---        | ---
$00000042  | memory address of 00000042
# 42        | decimal 42 literal

# $42       | hexadecimal 42 literal

# %01000010 | binary 42 literal
a0         | address register 0
d0         | data register 0
$42(a0)    | add offset of 42 to address register 0
(a0)+      | increment address register 0 (register incremented after instruction)
-(a0)      | decrement address register 0 (register decreased before instruction)

## Links

* http://68k.hax.com
* http://www.freescale.com/files/archives/doc/ref_manual/M68000PRM.pdf


# Intel x86

## Real Mode

* less than 1MB of RAM
* no virtual memory
* no hardware memory protection

## ISA

ins       | desc
---       | ---
push OP   | Push operand to stack
pop OP    | Pop operand off stack
call ADDR | Call function by setting EIP to ADDR. Next instruction following call pushed to stack
ret       | Pop return address from stack, set EIP to address
int OP    | Generate software interrupt

## Bootstrapping

Starts in 16-bit Real Mode, for compatibility reasons

Zero the data segment registers first thing, since their content is unknown.

The BIOS transfers the first 512 bytes of data from the device into 0x7c00. The last two bytes need to be 0x55 and then 0xAA to be considered a valid bootsector.

## Factoids

* IA-64 refers to Itanium, not the Intel 64-bit x86 architecture.
* x86 is little endian

## Resources

* [Disabling Intel Management Engine](http://blog.ptsecurity.com/2017/08/disabling-intel-me.html)
* <http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html>
* <http://www.cirosantilli.com/x86-paging/>
* [Intel x86 considered harmful (PDF)](https://blog.invisiblethings.org/papers/2015/x86_harmful.pdf)


# Cyrix
[Cyrix coma bug](https://en.wikipedia.org/wiki/Cyrix_coma_bug)



# ENIAC
<https://en.wikipedia.org/wiki/First_Draft_of_a_Report_on_the_EDVAC>

# MIL-STD-1750A
<https://en.wikipedia.org/wiki/MIL-STD-1750A>



