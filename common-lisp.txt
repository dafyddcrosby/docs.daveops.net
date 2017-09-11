Common Lisp
===========
:date: 2015-02-27

Compile a file
--------------
.. code-block:: lisp

   (compile-file "file.lisp")

Format directives
-----------------
(format destination control-string args) => result

destination can be nil, t (ie STDOUT), a stream, or string w/ fill pointer

+---+-----------------------------------+
| ~ | Directive                         |
+===+===================================+
| % | newline                           |
+---+-----------------------------------+
| { | Consume a list                    |
+---+-----------------------------------+
| a | 'aesthetic', output in human form |
+---+-----------------------------------+
| R | human output of number            |
+---+-----------------------------------+

