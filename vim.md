---
title: vim
---

<http://www.vimninjas.com/>

## profile Vim startup time

Requires vim 7.2.269+

```bash
vim --startuptime start.log
```

## Sort on a virtual column

```text
:sort /.*\%10v/
```

## Get filetype

```text
:set filetype?
```

## Show line numbers

```text
:set number
```

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

- http://www.vimregex.com/

 # Search and replace

letter | desc
---    | ---
c      | Confirm substitutions
g      | Replace all line occurrences in the line (not just first)
i      | Ignore case
I      | Case insensitive

 # Character classes

`:help character-classes`

Uppercase for inverse 

mc  | desc
--- | ---
.   | any character except new line
\s  | tab or space character
\p  | printable character
\f  | filename character
\k  | keyword character
\i  | identifier character
\d  | digit
\x  | hex digit
\o  | octal digit
\w  | word character [a-zA-Z_]
\h  | head of word character
\a  | alphabetic character
\l  | lowercase character
\u  | uppercase character

## Delete all lines containing a pattern

```text
:g/profile/d
:g/^\s*$/d
```

# slimv

key | desc
--- | ---
,c  | connect to SWANK server
,d  | evaluate defun
,b  | evaluate buffer
,r  | evaluate region
,e  | evaluate current expression
,s  | open symbol documentation
,h  | open symbol underneath in HyperSpec
,,  | Slimv menu

* https://github.com/kovisoft/slimv
* https://kovisoft.github.io/slimv-tutorial/

# tabs

```bash
# Open a session from CLI
vim -S session.vim
```

keys | desc
---  | ---
gt   | next tab
gT   | previous tab

# Modelines

```
<!-- vim: set nospell: -->
```

<!-- vim: set nospell: -->
