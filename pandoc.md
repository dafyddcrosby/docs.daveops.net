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
