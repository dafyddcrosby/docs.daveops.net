
# Emacs Lisp

[An Introduction to Programming in Emacs Lisp](https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html)


## Lexical binding

To enable lexical-binding in a file:

```elisp
;; -*- lexical-binding: t -*-
```


## Keystrokes

| desc                    | keystroke |
|----------------------- |--------- |
| execute line            | C-x C-e   |
| execute, dump in buffer | C-j       |


## Limitations

From <https://www.emacswiki.org/emacs/EmacsLispLimitations>

> EmacsLisp is a surprisingly powerful, rich dialect of Lisp. It can be used to do many things, making it practically a general use application language, and not just a language for extending Emacs.
> 
> However, you will run into certain immovable walls the further you go:
> 
> - It is not, and presumably never will be, multi-threaded.
> - Once you “throw” from an inner form, you can never return to that evaluation context. In other languages, continuations allow you to do exactly that.
> - There is currently no way to start a process in the background, and direct its standard output and standard error streams to different locations.
> - You cannot create a n-pixel tall line of whitespace (you can, however, create an n-pixel wide character of whitespace). This becomes important if you’re trying to use graphics in Emacs 21.
> - Requires launching a subprocess to read from a device file on UNIX.
> - Emacs Lisp is fairly slow, and there’s not much to be done about it. Converting key algorithms to C is an arduous process which requires rebuilding Emacs.
> - Memory consumption gets out-of-hand REAL FAST if you don’t pay attention. If you’re writing a new library, be sure to turn on garbage collection messages by setting ‘garbage-collection-messages’ to t. If you see the collection messages every few seconds, you can be sure that your library is eating up and spitting out cons cells and strings at a furious rate.
> - Emacs cannot disown a process it has invoked. The only way is to invoke your process through “/bin/nohup” on UNIX systems. See PersistentProcesses.
> - Emacs Lisp is not Scheme. See SchemeAndLisp.


### Memory consumption

From <https://www.emacswiki.org/emacs/EmacsLispLimitations>

> - Memory consumption gets out-of-hand REAL FAST if you don’t pay attention. If you’re writing a new library, be sure to turn on garbage collection messages by setting ‘garbage-collection-messages’ to t. If you see the collection messages every few seconds, you can be sure that your library is eating up and spitting out cons cells and strings at a furious rate.


# Emacs Lisp

| M-x                   | desc                                              |
|--------------------- |------------------------------------------------- |
| ielm                  | Emacs Lisp REPL                                   |
| lisp-interaction-mode | Enable Lispl interaction mode                     |
| scratch-buffer        | creates new scratch buffer for Emacs Lisp testing |