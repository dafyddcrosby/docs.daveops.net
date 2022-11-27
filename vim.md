# vim

<http://www.vimninjas.com/>


## profile Vim startup time

Requires vim 7.2.269+

```shell
vim --startuptime start.log
```


## Sort on a virtual column

```
:sort /.*\%10v/
```


## Get filetype

```
:set filetype?
```


## Show line numbers

```
:set number
```


## Inserting glyphs

ctrl-k starts digraph mode

:help digraphs

g8 - see bytes for UTF-8 character


## Omnicompletion

<C-X><C-O>


## Set shell

```
:set shell=/bin/bash
```


## Redraw shell

ctrl-l


# Regular expressions

`:help pattern-searches`

- <http://www.vimregex.com/>


# Search and replace

| desc                                                      | letter |
|--------------------------------------------------------- |------ |
| Confirm substitutions                                     | c      |
| Replace all line occurrences in the line (not just first) | g      |
| Ignore case                                               | i      |
| Case insensitive                                          | I      |


# Character classes

`:help character-classes`

Uppercase for inverse

| desc                          | mc |
|----------------------------- |--- |
| any character except new line | .  |
| tab or space character        | \s |
| printable character           | \p |
| filename character            | \f |
| keyword character             | \k |
| identifier character          | \i |
| digit                         | \d |
| hex digit                     | \x |
| octal digit                   | \o |
| word character [a-zA-Z\_]     | \w |
| head of word character        | \h |
| alphabetic character          | \a |
| lowercase character           | \l |
| uppercase character           | \u |


## Delete all lines containing a pattern

```
:g/profile/d
:g/^\s*$/d
```


# slimv

| desc                                | key |
|----------------------------------- |--- |
| connect to SWANK server             | ,c  |
| evaluate defun                      | ,d  |
| evaluate buffer                     | ,b  |
| evaluate region                     | ,r  |
| evaluate current expression         | ,e  |
| open symbol documentation           | ,s  |
| open symbol underneath in HyperSpec | ,h  |
| Slimv menu                          | ,,  |

- <https://github.com/kovisoft/slimv>
- <https://kovisoft.github.io/slimv-tutorial/>


# tabs

```shell
# Open a session from CLI
vim -S session.vim
```

| desc         | keys |
|------------ |---- |
| next tab     | gt   |
| previous tab | gT   |


# Modelines

```
<!-- vim: set nospell: -->
```