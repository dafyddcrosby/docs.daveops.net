# Documentation Systems


# man

| # | Description                                                      |
|--- |---------------------------------------------------------------- |
| 1 | General commands                                                 |
| 2 | System calls                                                     |
| 3 | Library functions, covering in particular the C standard library |
| 4 | Special files (usually devices, those found in /dev) and drivers |
| 5 | File formats and conventions                                     |
| 6 | Games and screensavers                                           |
| 7 | Miscellanea                                                      |
| 8 | System administration commands and daemons                       |
| n | Tcl/TK                                                           |
| x | X Windows                                                        |
| p | POSIX specifications                                             |


# Sphinx

```shell
# Create new project
sphinx-quickstart
```


## Removing extra blank pages from PDF

```python
latex_elements = {
  'classoptions': ',openany,oneside',
  'babel': '\\usepackage[english]{babel}',
}
```


# pandoc


## Making slides

```shell
pandoc -t beamer -V colortheme:whale -i presentation.md -o presentation.pdf
```

- [beamer color theme matrix](https://hartwork.org/beamer-theme-matrix/)


## Markdown

[Using extensions](https://pandoc.org/MANUAL.html#extensions)

```shell
pandoc -f markdown+footnotes+yaml_metadata_block
```


### `raw_tex`

Allows inserting raw TeX commands into the doc


### footnotes

```
I need a footnote[^footnote]

[^footnote]: This footnote explains why
```


## Links

- [Pandoc manual](https://pandoc.org/MANUAL.html)