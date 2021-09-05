---
title: XML
---

## Prettify

```bash
tidy -xml -i -m [file]
```

## XMLLint

```bash
# Check XML file is well-formed
xmllint --noout $FILE

# Check XML file against local DTD file
xmllint --noout --dtdvalid ./local.dtd $FILE
```

- <http://www.xmlsoft.org/>
