# Emacs Package Management


# package.el

Standard with Emacs 24

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


# use-package

Standard with Emacs 29

[use-package GitHub page](https://github.com/jwiegley/use-package/)


# Quelpa

[Quelpa](https://github.com/quelpa/quelpa) uses the MELPA package recipes and builds them locally from source


# Repositories

[EmacsWiki: ELPA](https://www.emacswiki.org/emacs/ELPA)

<https://elpa.gnu.org/>

Emacs Lisp Package Archive