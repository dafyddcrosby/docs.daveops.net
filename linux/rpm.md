---
title: RPM
tags: ["Red Hat"]
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
```

Rebuild SRPM
------------

```bash
# Centos 6
rpmbuild --rebuild <SRPM>
# also
rpm -i <SRPM>
rpmbuild -ba rpmbuild/SPECS/<spec file>
```

Extract RPM contents
--------------------

```bash
rpm2cpio php-5.1.4-1.esp1.x86_64.rpm | cpio -idmv
```

Signing RPM's with GPG
----------------------

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

RPM DB rebuild
--------------

```bash
rm -f /var/lib/rpm/__db*
rpm --rebuilddb
```
