---
title: Common Lisp
---

## Basics

```lisp
;; global variable
(defparameter *foo* 123)
;; global constant
(defconstant *limit* 100)

; basic function
(defun divisible-by-3 (n)
  (= (mod n 3) 0))

; emit to STDOUT
(format t "hello world")

; iterate over a list
(dolist (x '(chunky bacon)) (print x))

; expand a macro
(macroexpand-1 '(dolist (x '(chunky bacon)) (print x)))

```

## Source files

```lisp
; load a file
(load "file.lisp")

; compile a file
(compile-file "file.lisp")
```

## Property lists

```lisp
; create a plist
(list :first "frank" :last "sinatra")

; get a value from the plist
(getf '(:first "frank" :last "sinatra") :first)
; returns "frank"
```

## Format directives

(format destination control-string args) => result

destination can be nil, t (ie STDOUT), a stream, or string w/ fill pointer

| ~ | Directive                         |
|---|-----------------------------------|
| % | newline                           |
| { | Consume a list                    |
| a | 'aesthetic', output in human form |
| R | human output of number            |

## Streams

```lisp
;; check if it's a stream
(output-stream-p *standard-output*)
(input-stream-p *standard-input*)
;; write to standard output
(write-char #\a *standard-output*)
(print 'blerk *standard-output*)
;; read a character
(read-char *standard-input*)
;; write to file
(with-open-file (file-handle "example.txt" :direction :output)
(print "text to write" file-handle))
;; read from file
(with-open-file (file-handle "example.txt" :direction :input)
(read file-handle))
;; file exists?
(probe-file "/path/to/file")
;; mkdir -p
(ensure-directories-exist "/path/to/file")
```

## Resources

* [Paradigms of Artificial Intelligence Programming](https://github.com/norvig/paip-lisp)
* [Practical Common Lisp](http://www.gigamonkeys.com/book/)

<!--TODO

* is there any effort to update the Hyperspec?
* generate Lisp reading list. Start with 1.3 in the Hyperspec?

defun &rest, &optional, etc

writing macros

look up *query-io* global variable

-->
