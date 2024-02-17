# Lisp

(This is more a directory of interesting Lisp artifacts and sites I've stumbled across, and don't want to have to find again).

- [ISLISP](http://www.islisp.info) - a Lisp standard


## Quotes

> "LISP is a high-level language, but you can still feel the bits sliding between your toes." -Guy Steele


# Scheme

- <https://www.scheme.com>


## Syntax cheatsheet

```scheme
"This is a string"
CaseSensitiveIdentifiers

(define add2 
  (lambda (n)
    (+ n 2)))
;; syntactic sugar:
(define (add2 n)
  (+ n 2))

(load "filename.ss")
```


# Common Lisp


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
|--- |--------------------------------- |
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

- [Paradigms of Artificial Intelligence Programming](https://github.com/norvig/paip-lisp)
- [Practical Common Lisp](http://www.gigamonkeys.com/book/)
- <https://github.com/CodyReichert/awesome-cl>
- <http://www.adamtornhill.com/articles/lispweb.htm>
- <https://edicl.github.io/hunchentoot/#teen-age>
- <https://edicl.github.io/cl-who/>
- <https://lisp-lang.org/books/>
- <https://www.onlineprogrammingbooks.com/free-lisp-books/>


## Quicklisp

<https://www.quicklisp.org/beta/>


### download

```shell
curl -O https://beta.quicklisp.org/quicklisp.lisp
curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
gpg --verify quicklisp.lisp.asc quicklisp.lisp
```


### initial install

```lisp
(load "quicklisp.lisp")
```


### get software update

```lisp
(ql:update-dist "quicklisp")
```


### Load a system

```lisp
(ql:quickload "system-name")
```


### Find a system

```lisp
(ql:system-apropos "term")
```


### Load Quicklisp every time Lisp starts

```lisp
(ql:add-to-init-file)
```


# Clozure

[Clozure homepage](https://ccl.clozure.com/)

```lisp
(external-call "getpid" :pid_t)
```


# Interlisp

- <https://interlisp.org/>
- <https://github.com/interlisp>


## Prominent Interlisp Programs


### Automated Mathematician

<https://github.com/white-flame/am>


### EURISKO

<https://github.com/white-flame/eurisko>
