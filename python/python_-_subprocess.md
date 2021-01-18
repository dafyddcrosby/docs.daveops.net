---
title: Python - subprocess
---

## Popen

```python
pipe = subprocess.Popen(args_list, shell=True, stdout=subprocess.PIPE)
output - pipe.communicate()[0]
```
