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

| letter | desc                                                      |
|------ |--------------------------------------------------------- |
| c      | Confirm substitutions                                     |
| g      | Replace all line occurrences in the line (not just first) |
| i      | Ignore case                                               |
| I      | Case insensitive                                          |


# Character classes

`:help character-classes`

Uppercase for inverse

| mc | desc                          |
|--- |----------------------------- |
| .  | any character except new line |
| \s | tab or space character        |
| \p | printable character           |
| \f | filename character            |
| \k | keyword character             |
| \i | identifier character          |
| \d | digit                         |
| \x | hex digit                     |
| \o | octal digit                   |
| \w | word character [a-zA-Z\_]     |
| \h | head of word character        |
| \a | alphabetic character          |
| \l | lowercase character           |
| \u | uppercase character           |


## Delete all lines containing a pattern

```
:g/profile/d
:g/^\s*$/d
```


# slimv

| key | desc                                |
|--- |----------------------------------- |
| ,c  | connect to SWANK server             |
| ,d  | evaluate defun                      |
| ,b  | evaluate buffer                     |
| ,r  | evaluate region                     |
| ,e  | evaluate current expression         |
| ,s  | open symbol documentation           |
| ,h  | open symbol underneath in HyperSpec |
| ,,  | Slimv menu                          |

- <https://github.com/kovisoft/slimv>
- <https://kovisoft.github.io/slimv-tutorial/>


# tabs

```shell
# Open a session from CLI
vim -S session.vim
```

| keys | desc         |
|---- |------------ |
| gt   | next tab     |
| gT   | previous tab |


# Modelines

```
<!-- vim: set nospell: -->
```