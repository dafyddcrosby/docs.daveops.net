---
title: YUM
tags: RedHat
---

## Create a group

```bash
yum-groups-manager -n "My Group" --id=mygroup --save=mygroups.xml --mandatory yum glibc rpm
createrepo -g /path/to/mygroups.xml /srv/my/repo
```

## Find the package that provides a library

```bash
yum whatprovides *Xlib.h
```

## versionlock

RPM is either yum-versionlock or yum-plugin-versionlock

```bash
# add lock at current version 
yum versionlock add PACKAGE
# list locked version
yum versionlock list
# delete an entry
yum versionlock delete PACKAGE
# blow away entire versionlock file
yum versionlock clear
```
