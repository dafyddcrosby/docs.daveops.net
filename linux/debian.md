---
title: Debian
---

# Package management

* `/etc/apt/sources.list` - list of APT repositories

```bash
# Install a package
dpkg -i file.deb
# Remove a package
dpkg -r packagename
# Purge package (ie get rid of config files, too)
dpkg -p packagename

# Get list of packages on the system
dpkg -l
# List files in a package
dpkg -L packagename
# Get package information
dpkg -s packagename
# See what package provides a file
dpkg -S /path/to/file

# Reconfigure a package
dpkg-reconfigure packagename

# Update repo list
apt-get update
# Upgrade installed packages
apt-get upgrade
# Install a package
apt-get install packagename
# Search for a package
apt-cache search KEYWORD
# See dependencies for a package
apt-cache depends packagename
# Remove a package
apt-get remove packagename
```

# misc
Default run level is 2
