---
title: Design Patterns
---

Knowledge of design patterns make it easier to hold software designs in your head (a la memory chunking)

## Singleton
```python
class Singleton:
    __single = None
    def __init__( self ):
        if Singleton.__single:
            raise Singleton.__single
        Singleton.__single = self
```
