---
title: Sega Genesis/Mega Drive
---

The Genesis (Mega Drive in Japan) has 2 on-board processors, a 68000 @ 8MHz and a z80
@ 4MHz (to handle sound processing).

## 68000 Memory map

<!-- TODO cross-reference -->

Start address | End address | Description                     | 32X SH-2 address
---           | ---         | ---                             | ----
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
--- | --- | ---
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

## Links 

* https://en.wikibooks.org/wiki/Genesis_Programming
