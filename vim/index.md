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

## Links

* http://www.vimregex.com/

<!-- set a modeline -->
<!-- vim: set nospell: -->
