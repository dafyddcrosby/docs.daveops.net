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
# APK
Used in Alpine Linux

```
apk add
apk del
apk --no-cache
```




---
title: RPM
tags: RedHat
---

[List of RPM macros](http://www.zarb.org/~jasonc/macros.php)

```bash
# List files in an RPM
rpm -qlp file.rpm

# Find packages that depend on a particular package
rpm -q --whatrequires ${PACKAGE}

# Get grepable info from RPM
rpm --querytags # get list of tags
rpm -q --queryformat="%{NAME}: %{LICENSE}\n" package_name

# Verify package integrity
rpm -V <package>

# Show package dependencies
rpm -qpR <.rpm file>
rpm -qR <package name>

# Install obsolete RPM
rpm -i --nodeps ./RPM

# Show RPM changelog
rpm -q --changelog <package>
```

## Rebuild SRPM

```bash
# Centos 6
rpmbuild --rebuild <SRPM>
# also
rpm -i <SRPM>
rpmbuild -ba rpmbuild/SPECS/<spec file>
```

## Extract RPM contents

```bash
rpm2cpio php-5.1.4-1.esp1.x86_64.rpm | cpio -idmv
```

## Signing RPM's with GPG

In `.rpmmacros`

```
%_signature gpg
%_gpg_name Joe Example <joe@example.org>
```

```bash
# Replace existing signature
rpm --resign package1.rpm package2.rpm ...
# Add additional sig (pre-4.1 ?)
rpm --resign package1.rpm package2.rpm ...
```

## RPM DB rebuild

```bash
rm -f /var/lib/rpm/__db* && rpm --rebuilddb
```
