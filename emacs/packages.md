
# Emacs Package Management

# package.el

Standard with Emacs 24

| desc                             | M-x                      |
|-------------------------------- |------------------------ |
| install a package                | package-install          |
| remove unused dependent packages | package-autoremove       |
| get list of installable packages | package-list-packages    |
| refresh package repository       | package-refresh-contents |

```elisp
; FSF
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/packages/"))
; Milkypostman's ELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
```


## help

| keystrokes | desc             |
|---------- |---------------- |
| C-h P      | describe package |


## Package

| key | desc                      |
|--- |------------------------- |
| i   | mark for installation     |
| u   | unmark                    |
| x   | execute action on package |


# use-package

Nice way to keep configuration of emacs packages tidy

Standard with Emacs 29

[use-package GitHub page](https://github.com/jwiegley/use-package/)


# Quelpa

[Quelpa](https://github.com/quelpa/quelpa) uses the MELPA package recipes and builds them locally from source

`quelpa-upgrade-all` to upgrade all packages

`quelpa RET package_name` to install a package

`(quelpa 'package-name)` to use in Emacs lisp


# Repositories

[EmacsWiki: ELPA](https://www.emacswiki.org/emacs/ELPA)

<https://elpa.gnu.org/>

Emacs Lisp Package Archive