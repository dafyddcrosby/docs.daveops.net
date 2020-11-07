---
title: Intel x86
---

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
