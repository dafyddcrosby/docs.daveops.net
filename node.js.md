---
title: node.js
---

[NodeJS ECMAScript Support](http://node.green/)

## Debugging

```bash
node --inspect ...
```

Can also be turned on with SIGUSR1
Node <7 - Debugger API
Node >8 - Inspector API

### Debugger
Connect with ``node inspect HOST:PORT``
Debug directly with ``node inspect file.js``
Inserting ``debugger;`` in your code sets a breakpoint

#### Stepping

cont, c - Continue execution
next, n - Step next
step, s - Step in
out, o - Step out
pause - Pause running code (like pause button in Developer Tools)


#### Breakpoints

setBreakpoint(), sb() - Set breakpoint on current line
setBreakpoint(line), sb(line) - Set breakpoint on specific line
setBreakpoint('fn()'), sb(...) - Set breakpoint on a first statement in functions body
setBreakpoint('script.js', 1), sb(...) - Set breakpoint on first line of script.js
clearBreakpoint('script.js', 1), cb(...) - Clear breakpoint in script.js on line 1



#### Information

backtrace, bt - Print backtrace of current execution frame
list(5) - List scripts source code with 5 line context (5 lines before and after)
watch(expr) - Add expression to watch list
unwatch(expr) - Remove expression from watch list
watchers - List all watchers and their values (automatically listed on each breakpoint)
repl - Open debugger's repl for evaluation in debugging script's context
exec expr - Execute an expression in debugging script's context


#### Execution control

run - Run script (automatically runs on debugger's start)
restart - Restart script
kill - Kill script


#### Various

scripts - List all loaded scripts
version - Display V8's version


