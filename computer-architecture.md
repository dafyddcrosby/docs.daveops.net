# Computer Architecture
# Unicorn CPU emulator

[Unicorn CPU emulator](http://www.unicorn-engine.org/)

# Motorola 68000

Note: This information is generally geared to the 68000 specifically (for use with the Sega Genesis), later processor models have more/enhanced instructions :-)

- http://68k.hax.com
- http://www.freescale.com/files/archives/doc/ref_manual/M68000PRM.pdf

## ISA

field | desc
---   | ---
 #X   | immediate (raw) data
 ea   | effective address (data register or memory address)
 An   | address register
 Dn   | data register
 Rn   | any register

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

operand     | desc
---         | ---
 $00000042  | memory address of 00000042
 #42        | decimal 42 literal
 #$42       | hexadecimal 42 literal
 #%01000010 | binary 42 literal
 a0         | address register 0
 d0         | data register 0
 $42(a0)    | add offset of 42 to address register 0
 (a0)+      | increment address register 0 (register incremented after instruction)
 -(a0)      | decrement address register 0 (register decreased before instruction)

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


## Node Type T30 - Stack Memory Node

## Node Type T31 - Random Access Memory Node

## Debugger

## Visualization Module



# Intel x86

## ISA

ins       | desc
---       | ---
push OP   | Push operand to stack
pop OP    | Pop operand off stack
call ADDR | Call function by setting EIP to ADDR. Next instruction following call pushed to stack
ret       | Pop return address from stack, set EIP to address
int OP    | Generate software interrupt

## Bootstrapping

Starts in 16-bit [Real Mode](#real-mode), for compatibility reasons

Zero the data segment registers first thing, since their content is unknown.

The BIOS transfers the first 512 bytes of data from the device into 0x7c00. The last two bytes need to be 0x55 and then 0xAA to be considered a valid bootsector.
## Real Mode

* less than 1MB of RAM
* no virtual memory
* no hardware memory protection

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

# Sega Genesis/Mega Drive

The Genesis (Mega Drive in Japan) has 2 on-board processors, a 68000 @ 8MHz and a z80 @ 4MHz (to handle sound processing).

- https://en.wikibooks.org/wiki/Genesis_Programming

## 68000 Memory map

<!-- TODO cross-reference -->

Start address | End address | Description                     | 32X SH-2 address
---           | ---         | ---                             | ---
0x000000      | 0x3FFFFF    | Cartridge ROM/RAM               | 0x2000000-0x23FFFFF
0x400000      | 0x7FFFFF    | Reserved (used by the Mega-CD and 32X)
0x800000      | 0x9FFFFF    | Reserved (used by the 32X)
0x840000      | 0x85FFFF    | 32X frame buffer                | 0x4000000-0x401FFFF
0x860000      | 0x87FFFF    | 32X frame buffer overwrite mode | 0x4020000-0x403FFFF
0x880000      | 0x8FFFFF    | 32X cartridge ROM (first 512kB bank only)
0x900000      | 0x9FFFFF    | 32X cartridge bankswitched ROM (any 512kB bank, controlled by 32X registers)
0xA00000      | 0xA0FFFF    | Z80 memory space
0xA10000      | 0xA10001    | Version register
0xA10002      | 0xA10003    | Controller 1 data
0xA10004      | 0xA10005    | Controller 2 data
0xA10006      | 0xA10007    | Expansion port data
0xA10008      | 0xA10009    | Controller 1 control
0xA1000A      | 0xA1000B    | Controller 2 control
0xA1000C      | 0xA1000D    | Expansion port control
0xA1000E      | 0xA1000F    | Controller 1 serial transmit
0xA10010      | 0xA10011    | Controller 1 serial receive
0xA10012      | 0xA10013    | Controller 1 serial control
0xA10014      | 0xA10015    | Controller 2 serial transmit
0xA10016      | 0xA10017    | Controller 2 serial receive
0xA10018      | 0xA10019    | Controller 2 serial control
0xA1001A      | 0xA1001B    | Expansion port serial transmit
0xA1001C      | 0xA1001D    | Expansion port serial receive
0xA1001E      | 0xA1001F    | Expansion port serial control
0xA10020      | 0xA10FFF    | Reserved
0xA11000      |             | Memory mode register
0xA11002      | 0xA110FF    | Reserved
0xA11100      | 0xA11101    | Z80 bus request
0xA11102      | 0xA111FF    | Reserved
0xA11200      | 0xA11201    | Z80 reset
0xA11202      | 0xA12FFF    | Reserved
0xA13000      | 0xA130FF    | TIME registers; used to send signals to the cartridge
0xA130F1      |             | SRAM access register
0xA130F3      |             | Bank register for address 0x80000-0xFFFFF
0xA130F5      |             | Bank register for address 0x100000-0x17FFFF
0xA130F7      |             | Bank register for address 0x180000-0x1FFFFF
0xA130F9      |             | Bank register for address 0x200000-0x27FFFF
0xA130FB      |             | Bank register for address 0x280000-0x2FFFFF
0xA130FD      |             | Bank register for address 0x300000-0x37FFFF
0xA130FF      |             | Bank register for address 0x380000-0x3FFFFF
0xA14000      | 0xA14003    | TMSS "SEGA"
0xA14104      | 0xBFFFFF    | Reserved
0xC00000      | 0xC00001    | VDP data port
0xC00002      | 0xC00003    | VDP data port (mirror)
0xC00004      | 0xC00005    | VDP control port
0xC00006      | 0xC00007    | VDP control port (mirror)
0xC00008      | 0xC00009    | VDP H/V counter
0xC0000A      | 0xC0000F    | VDP H/V counter (mirror)
0xC00011      |             | PSG output
0xC00013      | 0xC00017    | PSG output (mirror)
0xC0001C      | 0xC0001D    | Debug register
0xC0001E      | 0xC0001F    | Debug register (mirror)
0xC00020      | 0xFEFFFF    | Reserved
0xFF0000      | 0xFFFFFF    | 68000 RAM

## Z80 memory map

<!-- TODO cross-reference -->

Start | End   | Description
---   | ---   | ---
0000h | 1FFFh | Z80 RAM
2000h | 3FFFh | Reserved
4000h |       | M2612 A0
4001h |       | M2612 D0
4002h |       | M2612 A1
4003h |       | M2612 D1
4004h | 5FFFh | Reserved
6000h |       | Bank register
6001h | 7F10h | Reserved
7F11h |       | PSG
7F12h | 7FFFh | Reserved
8000h | FFFFh | 68000 memory bank

## SegaCD / MegaCD

Start address | End address | Description
---           | ---         | ---
0x000000      | 0x01FFFF    | MegaCD BIOS ROM
0x020000      | 0x03FFFF    | MegaCD "Program RAM" Bank Access
0x200000      | 0x23FFFF    | MegaCD "WORD RAM"
0xA12000      | 0xA120XX    | MegaCD "Gate Array"
0xFFFD00      | 0xFFFDFF    | MegaCD Interrupt/Exception vectors

# Supercomputers

- <http://www.netlib.org/linpack/>
- <http://www.netlib.org/lapack/>

