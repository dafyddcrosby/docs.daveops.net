---
title: pandoc
---

## Markdown footnotes

```
I need a footnote[^footnote]

[^footnote]: This footnote explains why
```

## Making slides

```bash
pandoc -t beamer -V colortheme:whale -i presentation.md -o presentation.pdf
```

* [beamer color theme matrix](https://hartwork.org/beamer-theme-matrix/)
