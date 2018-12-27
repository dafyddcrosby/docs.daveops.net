---
title: Common Lisp
---

## Basics

```common-lisp
;; global variable
(defparameter *foo* 123)
;; global constant
(defconstant *limit* 100)

; basic function
(defun divisible-by-3 (n)
  (= (mod n 3) 0))
```

## Compile a file

```lisp
(compile-file "file.lisp")
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
