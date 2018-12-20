# Common Lisp
Compile a file
--------------

	(compile-file "file.lisp")


Format directives
-----------------

(format destination control-string args) => result

destination can be nil, t (ie STDOUT), a stream, or string w/ fill pointer

| ~ | Directive                         |
|---|-----------------------------------|
| % | newline                           |
| { | Consume a list                    |
| a | 'aesthetic', output in human form |
| R | human output of number            |


