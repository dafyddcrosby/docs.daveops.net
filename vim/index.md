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

```
:sort /.*\%10v/
```

## Get filetype

```
:set filetype?
```

## Delete all lines containing a pattern

```
:g/profile/d
:g/^\s*$/d
```

## Show line numbers

```
:set number
```

## Omnicompletion
<C-X><C-O>

## Set shell

```
:set shell=/bin/bash
```

## Links
* http://www.vimregex.com/

<!-- set a modeline -->
<!-- vim: set nospell: -->
