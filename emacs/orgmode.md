
# Org Mode

Possibly the killer app for Emacs

[Org Mode Website](https://orgmode.org/)

[Org Mode Manual](https://orgmode.org/manual/)


# Hyperlinking

| desc                 | keystrokes |
| ---                  | ---        |
| insert link          | C-c C-l    |
| learn to open a link | C-c C-o    |
| jump back            | C-c &      |


## Interlinking

```
[[LINK][[[]]LINK][DESCRIPTION]]
```


## Link abbreviations (custom link targets)

[orgmode docs on link abbreviations](https://orgmode.org/manual/Link-Abbreviations.html)

```elisp
(setq org-link-abbrev-alist
      '(("duckduckgo" . "https://duckduckgo.com/?q=%s")
        ("wp"         . "https://en.wikipedia.org/wiki/%s")
        ("omap"       . "https://nominatim.openstreetmap.org/ui/search.html?q=%s&polygon=1")))
```


# Agenda

| desc             | keystroke |
| ---              | ---       |
| open agenda mode | C-c a     |
