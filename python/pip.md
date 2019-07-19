---
title: pip
tags: ["Python"]
---

## Create a package list

```bash
pip freeze > requirements.txt
```

## Upgrade package

```bash
pip install -U package
```

## Install a package list

```bash
pip install -r requirements.txt
```

## Dreamhost pip3 usage
```bash
virtualenv -p python3 env
. env/bin/activate
pip3 install #...
```
