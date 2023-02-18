
# Abbrev Mode

[EmacsWiki for Abbrev Mode](https://www.emacswiki.org/emacs/AbbrevMode)

| desc                                       | command                   | keystroke |
| ---                                        | ---                       | ---       |
| define a global abbrev                     | add-global-abbrev         | C-x a g   |
| define a local (mode-specific) abbrev with | add-mode-abbrev           | C-x a l   |
| define a word in the buffer (ie inverse)   | inverse-add-global-abbrev | C-x a i g |
| cancel an expansion                        | quoted-insert             | C-q       |
| define an abbrev from minibuffer           | define-global-abbrev      |           |
| save abbrevs when in abbrev mode           | edit-abbrevs-redefine     | C-c C-c   |


# Editing abbrevs

| desc                                               | command           |
|-------------------------------------------------- |----------------- |
| write lisp file of user-created abbrev definitions | write-abbrev-file |
| edit in-memory abbrev definitions                  | edit-abbrevs      |
| read definitions file                              | read-abbrev-file  |