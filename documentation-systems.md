# Documentation Systems
# man

 #  | Description
--- | ---
 1  | General commands
 2  | System calls
 3  | Library functions, covering in particular the C standard library
 4  | Special files (usually devices, those found in /dev) and drivers
 5  | File formats and conventions
 6  | Games and screensavers
 7  | Miscellanea
 8  | System administration commands and daemons
 n  | Tcl/TK
 x  | X Windows
 p  | POSIX specifications
# Sphinx

## Create new project



 sphinx-quickstart

## Removing extra blank pages from PDF

```python

 latex_elements = {
   'classoptions': ',openany,oneside',
   'babel': '\\usepackage[english]{babel}',
 }
```
# pandoc

## Making slides

```bash
pandoc -t beamer -V colortheme:whale -i presentation.md -o presentation.pdf
```

* [beamer color theme matrix](https://hartwork.org/beamer-theme-matrix/)

## Markdown

[Using extensions](https://pandoc.org/MANUAL.html#extensions)

```bash
pandoc -f markdown+footnotes+yaml_metadata_block
```

### raw_tex

Allows inserting raw TeX commands into the doc

### footnotes

```markdown
I need a footnote[^footnote]

[^footnote]: This footnote explains why
```

### raw_attributes

### yaml_metadata_block

### backtick_code_blocks

### task_lists

## LaTeX

```yaml
header-includes:
- |
  ```{=latex}
  \let\oldsection\section
  \renewcommand{\section}[1]{\clearpage\oldsection{#1}}
  ```
```

## Links

- [Pandoc manual](https://pandoc.org/MANUAL.html)
