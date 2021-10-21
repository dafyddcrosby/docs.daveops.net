---
title: Fossil
---

```bash
# initialize repo
fossil init foo.repo
# make a working directory, check out a local tree
mkdir working && cd working && fossil open ../foo.repo
```

## Commands

cmd         | desc
---         | ---
add *file*  | add file to upcoming commit
amend       | Amend commit metadata
branch      | manage branches for a repo
cat         | print file as it exists in the repo
clean       | delete "extra" files
clone       | clone another fossil repo
commit      | commit changes
delete/rm   | mark file no longer part of project
diff        | diff the files
extras      | show files not tracked in source tree
finfo       | shows file history
timeline    | print summary of activity
ui          | start web server
unversioned | Manage unversioned artifacts in the repo
update      | Changes version of current checkout

## Import from git

```bash
git fast-export --all | fossil import --git new-repo.fossil
```

## Ignore glob

```bash
mkdir -p .fossil-settings
echo "ignore.this.file" > .fossil-settings/ignore-glob
fossil add .fossil-setings/ignore-glob
```

## Links

* <https://www.fossil-scm.org>
* [Git import](https://www.fossil-scm.org/home/doc/trunk/www/inout.wiki)
* [Fossil for Git users](https://www.fossil-scm.org/home/doc/trunk/www/gitusers.md)
