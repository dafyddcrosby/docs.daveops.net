# Version Control
# Git

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

## git bisect

```bash
# Visualize bisect
git bisect visualize
# Finish bisect
git bisect reset
```


## Git LFS

https://git-lfs.github.com/


```
# Checking out files
git lfs fetch
git lfs checkout
```


## Git commit signing

```bash
# sign a commit
git commit -S -m MESSAGE

# specify a default signing key
git config --global user.signingkey KEYID

# configure git to always sign commits
git config --global commit.gpgsign true

# Using X509 (a la smimesign + Git 2.19+)
git config --global gpg.x509.program smimesign
git config --global gpg.format x509
```

- https://git-scm.com/book/en/v2/Git-Tools-Signing-Your-Work
- https://help.github.com/en/github/authenticating-to-github/signing-commits


### Importing a local SVN repo to git

```bash
git svn clone file:///path/to/repo -T trunk -b branches -t tags
```


## git-crypt

https://www.agwa.name/projects/git-crypt/


### Gitlab
[Running Gitlab in Docker](https://docs.gitlab.com/omnibus/docker/#run-the-image)


## GitHub

### Templates and special files

file                          | desc
---                           | ---
README.md                     | Initial documentation
CODEOWNERS                    | Specify who has access
.github/dependabot.yml        | Configuration for [Dependabot](https://docs.github.com/en/github/administering-a-repository/enabling-and-disabling-version-updates)
docs/CODE_OF_CONDUCT.md       | Code of Conduct for the project
docs/FUNDING.yml              | Specify accounts for [donations](https://docs.github.com/en/github/administering-a-repository/displaying-a-sponsor-button-in-your-repository)
docs/ISSUE_TEMPLATES/         | Templates for [issues](https://docs.github.com/en/github/building-a-strong-community/configuring-issue-templates-for-your-repository)
docs/pull_request_template.md | Single template for a pull request
docs/PULL_REQUEST_TEMPLATE/   | Multiple templates for pull requests
docs/SECURITY.md              | Security response documentation
docs/SUPPORT.md               | Let people know where to get help

You can use `/docs` or `/.github`

You can also create a [template
repository](https://docs.github.com/en/github/building-a-strong-community/creating-a-default-community-health-file#creating-a-repository-for-default-files)

### Profile

If you create a repo with the same name as your account, you can have
`README.md` at the top of your profile

### Signing releases

```bash
git tag -s software-0.1
git push --tags
# publish a release
# download tarball
# verify tarball matches git
gpg --armor --detach-sign software-0.1.tar.gz
# add gpg signature to release
```
### Get Atom feed of file changes

https://github.com/user/repo/commits/master/path/to/file.atom


### GitHub - Enterprise

#### CLI

```bash
# Set a message that's visible to everyone
ghe-announce -s MESSAGE
# Remove previously set message
ghe-announce -u
# Get webhook fails for a certain day
ghe-webhook-logs -f -a YYYYMMDD
```

#### Documentation

- <https://docs.github.com/en/enterprise-server@2.22/admin/configuration/command-line-utilities>
- <https://docs.github.com/en/enterprise-server@2.22/admin>
- <https://docs.github.com/en/enterprise-server@2.22/user/github>


# Fossil

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

## Ignore glob

```bash
mkdir -p .fossil-settings
echo "ignore.this.file" > .fossil-settings/ignore-glob
fossil add .fossil-setings/ignore-glob
```

## Import from git

```bash
git fast-export --all | fossil import --git new-repo.fossil
```

## Links

* <https://www.fossil-scm.org>
* [Git import](https://www.fossil-scm.org/home/doc/trunk/www/inout.wiki)
* [Fossil for Git users](https://www.fossil-scm.org/home/doc/trunk/www/gitusers.md)


# mercurial

* https://www.mercurial-scm.org/
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


# CVS

## Tagging an instance

```bash
cvs rtag -D "2010-1-28" tag_name module_name
```

## Untagging an instance

```bash
cvs rtag -d tag_name module_name
```

## CVsync

<http://www.openbsd.org/cvsync.html>

### Check available modules

	cvsync cvsync://<host>[:<port>]/</port></host>



# SVN

## Get information about a repo
```bash
svnlookup info /path/to/repo
# latest revision number
svnlookup youngest /path/to/repo
```

