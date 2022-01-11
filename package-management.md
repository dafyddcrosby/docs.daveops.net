# Package management
# YUM
"Yellowdog Updater, Modified"
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


# Homebrew

## CLI

command  | desc
---      | ---
outdated | list outdated homebrew packages
pin      | pin to a particular version
services | formulae integration with launchctl
cleanup  | remove old versions of packages
switch   | switch between installed versions
leaves   | Show installed formulae with no deps

## Services

```bash
brew services list
brew services start foo
```

## Installing from source

```bash
brew install FORMULA.rb --build-from-source
```

## Links

* <https://brew.sh/>
* <http://formulae.brew.sh/>


# pkgin

## Update pkgin database

```
pkgin -y up
```

DB located at /var/db/pkg

## Links

* <https://wiki.smartos.org/display/DOC/Working+with+Packages>
* <https://pkgsrc.joyent.com>
* <https://www.pkgsrc.org/>
* <http://pkgin.net>
