Lua
===


Iterate through file
--------------------
.. code-block:: lua

 for line in io.lines("my.txt") do print(line) end

Syntax cheatsheet
-----------------
.. code-block:: lua
   
 function function_name ( args ) body end
 
 while exp do block end
 
 for variable = from_exp , to_exp [, step_exp] do block end
 (foreach) for var {, var} in explist do block end
 
 repeat block until exp
 
 if exp then block { elseif exp then block } [ else block ] end
