---
title: Python - pdb
tags: ["python"]
---

cmd | desc
--- | ---
h   | help
w   | (where) stacktrace
l   | list source
s   | step
n   | next
d   | down frame
b   | breakpoint
tb  | temporary breakpoint

## CLI

```bash
python -m pdb file.py
```

## Invoke debugger when line is hit

```python
# Python 3.7+
breakpoint()

# Python < 3.7
import pdb
pdb.set_trace()
```
