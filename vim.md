---
title: vim
---

<http://www.vimninjas.com/>

## profile Vim startup time
vim 7.2.269+

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

## Delete all lines containing a pattern

```text
:g/profile/d
:g/^\s*$/d
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

## Regular expressions

- http://www.vimregex.com/


## Links


<!-- set a modeline -->
<!-- vim: set nospell: -->


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

## Open a session from CLI
```bash
vim -S session.vim
```

## Switch tabs

gt - next tab
gT - previous tab

