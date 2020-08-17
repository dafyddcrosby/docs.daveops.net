---
title: comby
---

flag           | desc
---            | ---
-f SUFFIX      | search for files with suffix
-i             | replace files in-place
-d DIR         | only files in DIR
-diff          | get patches as unified diffs
-templates DIR | use a template directory (see below)

## matching syntax

pattern    | desc
---        | ---
:[hole]    | match zero or more characters (including whitespace, and across newlines) in a lazy fashion. When :[hole] is inside delimiters, as in {:[h1], :[h2]} or (:[h]), matching stops inside them. Holes can be used outside of delimiters as well.
:[[hole]\] | match one or more alphanumeric characters and _
:[hole.]   | (with a period at the end) matches one or more alphanumeric characters and punctuation (like ., ;, and -)
:[hole\n]  | (with a \n at the end) matches one or more characters up to a newline, including the newline.
:[ ]	(with a space) matches only whitespace characters, excluding newlines. To assign the matched whitespace to variable, put the variable name after the space, like :[ hole].
:[?hole]   | (with a ? before the variable name) optionally matches syntax. Optional holes work like ordinary holes, except that if they fail to match syntax, the variable is assigned the empty string "". Optional hole support is currently an experimental feature.

## templates

Create a directory with `match` and `rewrite` files

```bash
comby .go -templates /path/to/dir
```
