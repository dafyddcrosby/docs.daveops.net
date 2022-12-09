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