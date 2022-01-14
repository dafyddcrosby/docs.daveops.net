# GCC

flag                  | desc
---                   | ---
-S                    | get assembly code
-g                    | debug (for GDB)
-pedantic             | warnings for strict ISO C
-std=c99              | use C99 standard
-Wall                 | show warnings of questionable practices
-Wextra               | warnings not covered by -Wall
-Wformat-y2k          | look for strftime code that uses double-digit year
-Wunreachable-code    | warn for unreachable code
-Wduplicated-cond     | warn about duplicated condition in if-else-if chains
-Wduplicated-branches | warn about duplicate branches in if-else-if chains
-Wlogical-op          | warn where a bitwise operation may have been intended
-Wnull-dereference    | warn where compiler detects path that dereferences a null pointer
-Wjump-misses-init    | warn where a goto jump misses variable initialization
-Wshadow              | warn when a local variable shadows another variable/parameter/type/class member
-Wformat=2            | aggressively check for format errors, warn about possible security bugs

## Links

* <http://gcc.gnu.org/onlinedocs/gcc/Code-Gen-Options.html>
