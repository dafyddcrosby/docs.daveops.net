---
Git
---
:date: 2015-04-28

Create bare repository
==============================
::

 mkdir -p project.git && cd project.git && git --bare init

 git clone --bare -l non_bare_repo new_bare_repo

 [remote "origin"]
        fetch = +refs/heads/*:refs/remotes/origin/*
        url = git.example.com:/path/project.git

Checkout remote repository
==========================
::

 git fetch
 git checkout -b local_branch_name remote/branch_name


Sequential versioning
==============================
::

 git rev-list --reverse HEAD | awk "/$(git log -n 1 --pretty="format:%h")/ {print NR}"

Reset git to a specific commit
==============================
::

 git reset [hash]
 git reset --soft HEAD@{1}
 git commit -m "revert to [hash]"
 git reset --hard

If you just need to amend last commit,

::

 git commit --amend

Handy hooks
==============================
Push to website on git push
-----------------------------------
on server, put this in ``hooks/post-receive``

::

 #!/bin/sh
 GIT_WORK_TREE=/path/to/www.example.org git checkout -f

Push branch to remote server
==============================
::

 git push origin branch_name

Delete remote branch
==============================
::

 git push origin :branch_to_delete
 git branch -d branch_to_delete

Tagging
==============================
::

 git tag  # list tags
 git tag -a v1.0 -m "Creating v1.0 tag"  # Create a tag
 git describe --tags  # Show current tag
 git push --tags  # push tags to remote

Stashing
==============================
::

 git stash list  # list stashes
 git stash save "message here"  # create a stash
 git stash show <stash>  # show stash diff
 git stash apply stash@{1}  # apply the code you stashed
 git stash drop <stash>  # delete specified stash
 git stash clear  # delete all stashes

Add remote branch
==============================
::

 git remote add upstream git://github.com/user/repo.git

Show nicely formatted changelog
===============================
::

 git log --graph --oneline --abbrev-commit --decorate

Get list of contributors
==============================
::

 git shortlog -s -n

Remove non-tracked files
==============================
::

 git clean -n  # dry run
 git clean -f  # delete the files

Squash commits into single commit
=================================
::

 git rebase -i <hash>

Untrack files without deletion
==============================
::

 echo "filename" >> .gitignore
 git rm --cached filename
 git add -u
 git commit -m "removing filename from version control"

Load my dotfiles to the home directory
======================================
::

 cd ~
 git init
 git remote add origin git@github.com:dafyddcrosby/dotfiles.git
 git pull origin master

Get list of staged files for commit
===================================
::

 git diff --cached --name-status | sed 's/.\s*//'

Diff remote repo
================
::

 git diff <branch> <remote>/<branch>

zlol
====
http://whatthecommit.com/

