---
title: Hotpatching
---

`MOV EDI, EDI` is used because it's a safe two-byte NOP that can be used across
x86-compatible manufactures - according to Raymond Chen, this was even figured
out by *talking* to the manufacturers.


## Some notes on NOP


> if you want to get technical, there isn't really any difference between
> those two. Since the processor doesn't stop, a NOP does *something*, regardless
> of how its implemented — at the bare minimum, it increments the instruction
> pointer. Selecting a "good" NOP is harder than expected because 1. it must do
> nothing observable to a program, 2. it must work on all processor models we
> want to support, 3. if it slows down the program, it should do so to the least
> extent possible. As the discussion here shows, doing nothing well can be
> surprisingly hard. Perhaps the Tao Te Ching should be consulted, in addition to
> any processor manuals.
> -Jeroen Mostert



> Why not just use two NOP instructions at the entry point?
> 
> Well, because a NOP instruction consumes one clock cycle and one pipe, so two
> of them would consume two clock cycles and two pipes. (The instructions will
> likely be paired, one in each pipe, so the combined execution will take one
> clock cycle.) On the other hand, the MOV EDI, EDI instruction consumes one
> clock cycle and one pipe. (In practice, the instruction will occupy one pipe,
> leaving the other available to execute another instruction in parallel. You
> might say that the instruction executes in half a cycle.) However you calculate
> it, the MOV EDI, EDI instruction executes in half the time of two NOP
> instructions.
> 
> On the other hand, the five NOPs inserted before the start of the function are
> never executed, so it doesn’t matter what you use to pad them. It could’ve been
> five garbage bytes for all anybody cares.
> 
> But much more important than cycle-counting is that the use of a two-byte NOP
> avoids the Detours problem: If the code had used two single-byte NOP
> instructions, then there is the risk that you will install your patch just as a
> thread has finished executing the first single-byte NOP and is about to begin
> executing the second single-byte NOP, resulting in the thread treating the
> second half of your JMP $-5 as the start of a new instruction.
> -Raymond Chen

## Windows

Add `/hotpatch` to the compiler flags.

## Links

* <https://blogs.msdn.microsoft.com/ishai/2004/06/24/why-does-the-compiler-generate-a-mov-edi-edi-instruction-at-the-beginning-of-functions/>
* <https://blogs.msdn.microsoft.com/oldnewthing/20110921-00/?p=9583/>
* <https://blogs.msdn.microsoft.com/oldnewthing/20130102-00/?p=5663/>


