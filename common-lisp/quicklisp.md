---
title: Quicklisp
---

Load a system
-------------

```common-lisp
(ql:quickload "system-name")
```

Find a system
-------------

```common-lisp
(ql:system-apropos "term")
```

Load Quicklisp every time Lisp starts
-------------------------------------

```common-lisp
(ql:add-to-init-file)
```
