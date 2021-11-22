---
title: Ruby - fibers
---

A fiber is an independent execution context that can be paused and resumed
programmatically. There's always a currently active fiber, which is created by
the runtime for each thread. Fibers are managed in the userspace program, using
cooperative multitasking (the fiber must voluntarily give up control) instead
of the OS' pre-emptive multitasking.

A fiber always starts in a suspended state, it will not run until you switch to it with #transfer.

States: `:running`, `:waiting`, `:runnable`, `:dead`

## Links

- [Fiber class](https://ruby-doc.org/core-3.0.2/Fiber.html)
- [Fiber::SchedulerInterface](https://ruby-doc.org/core-3.0.2/Fiber/SchedulerInterface.html)
- <https://noteflakes.com/articles/2021-10-20-explaining-ruby-fibers>
