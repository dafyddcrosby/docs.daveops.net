---
title: Git
tags: VersionControl, git
---

## Create bare repository

```bash
mkdir -p project.git && cd project.git && git --bare init
git clone --bare -l non_bare_repo new_bare_repo
```

```ini
[remote "origin"]
fetch = +refs/heads/*:refs/remotes/origin/*
url = git.example.com:/path/project.git
```

## Checkout remote repository

```bash
git fetch
git checkout -b local_branch_name remote/branch_name
```

## Rename a branch

```bash
git branch -m master main
```

## Sequential versioning

```bash
git rev-list --reverse HEAD | awk "/$(git log -n 1 --pretty="format:%h")/ {print NR}"
```

## Reset git to a specific commit

```bash
git reset [hash]
git reset --soft HEAD@{1}
git commit -m "revert to [hash]"
git reset --hard
```

If you just need to amend last commit,

```
git commit --amend
```

## Handy hooks

### Push to website on git push

On server, put this in ``hooks/post-receive``

```bash
#!/bin/sh
GIT_WORK_TREE=/path/to/www.example.org git checkout -f
```

## Push branch to remote server

```bash
git push origin branch_name
```

## Delete remote branch

```bash
git push origin :branch_to_delete
git branch -d branch_to_delete
```

## Tagging

```bash
git tag  # list tags
git tag -a v1.0 -m "Creating v1.0 tag"  # Create a tag
git describe --tags  # Show current tag
git push --tags  # push tags to remote
git checkout v1.0 # Check out tag 'v1.0'
```

## Stashing

```bash
git stash list  # list stashes
git stash save "message here"  # create a stash
git stash show <stash>  # show stash diff
git stash apply stash@{1}  # apply the code you stashed
git stash drop <stash>  # delete specified stash
git stash clear  # delete all stashes
```

## Add remote branch

```bash
git remote add upstream <git://github.com/user/repo.git>
```

## Show nicely formatted changelog

```bash
git log --graph --oneline --abbrev-commit --decorate
```

## See commits from individual

```bash
git log --author="david"
```

## Get list of contributors

```bash
git shortlog -s -n
```

## Remove non-tracked files

```bash
git clean -n  # dry run
git clean -f  # delete the files
```

## Squash commits into single commit

```bash
git rebase -i <hash>
```

## Untrack files without deletion

```bash
echo "filename" >> .gitignore
git rm --cached filename
git add -u
git commit -m "removing filename from version control"
```

## Load my dotfiles to the home directory

```bash
cd ~
git init
git remote add origin git@github.com:dafyddcrosby/dotfiles.git
git pull origin main
```

## Get list of staged files for commit

```bash
git diff --cached --name-status | sed 's/.\s*//'
```

## Diff remote repo

```bash
git diff <branch> <remote>/<branch>
```

## Import Sourceforge CVS repo

```bash
rsync -av rsync://w3m.cvs.sourceforge.net/cvsroot/w3m/ w3m
git cvsimport -p x -v -d /absolute/path/to/w3m w3m
```

## Import another repo as a subtree

```bash
git remote add -f remote_name git@example.com:remote_repo.git
git merge -s ours --no-commit remote_name/main
git read-tree --prefix=newpath/ -u remote_name/main
git commit -m "Subtree merged in newpath"
``` 

## Search git history

```bash
git log -S <search term>
```

## Retrieve single file from a specific revision in git

```bash
git checkout <HASH> -- ./path/to/file
```

## Remove branches that have been merged to main

```bash
git branch --merged | grep -v \* | xargs git branch -D
```

## Signing a commit

In the commit message

```text
Signed-off-by: David Crosby <email@example.com>
```

## Objects

```bash
# See all objects
git rev-list --objects --all
```

## zlol

<http://whatthecommit.com/>

## Misc
<https://gitea.io>
