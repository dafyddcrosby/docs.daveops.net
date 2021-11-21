---
title: mercurial
tags: VersionControl
---

## Clone a repo

```bash
hg clone $REPO
cd $REPO
hg add $FILES
hg commit -m "Changes"
hg push
```

## New repo

```bash
hg init $DIR
cd $DIR
hg add $FILES
hg commit -m "Changes"
```

## Links

* https://www.mercurial-scm.org/
