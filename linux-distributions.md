# Linux Distributions
# Fedora
- [Code of Conduct](https://docs.fedoraproject.org/en-US/project/code-of-conduct/)
## Upgrading to new Fedora versions

```bash
sudo dnf upgrade --refresh
sudo dnf install dnf-plugin-system-upgrade
sudo dnf system-upgrade download --refresh --releasever=33
```

https://fedoraproject.org/wiki/DNF_system_upgrade

## Links


# CentOS

- [RPM package sources](https://git.centos.org/project/rpms)
- [kernel sources](https://git.centos.org/sources/kernel/)
- <https://www.centos.org/download/>

## Building AMIs

- <https://github.com/CentOS/sig-cloud-instance-build/>
- [Images repository](https://cloud.centos.org/centos)
- [AWS instructions on importing VMs](https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html)


# NixOS

Uses /etc/nixos/configuration.nix for machine config

nixos-rebuild to use it

nix-channel handles the repositories

## /etc/nixos/configuration.nix

```
# auto upgrade system
system.autoUpgrade.enable = true;
```

## NixOS containers

As of 2019-01-04, "NixOS containers are not perfectly isolated from the host
system. This means that a user with root access to the container can do things
that affect the host. So you should not give container root access to untrusted
users." from <https://nixos.org/nixos/manual/index.html#ch-containers>

```bash
# Create a container
nixos-container create foo
# Run a container
nixos-container start foo
```
# Debian

## Package management

- `/etc/apt/sources.list` - list of APT repositories

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
# Remove a package
apt-get remove packagename

# Search for a package
apt-cache search KEYWORD
# See dependencies for a package
apt-cache depends packagename
# Get package version
apt-cache showpkg packagename
```

## misc
Default run level is 2


# Alpine Linux

```
apk add
apk del
apk --no-cache
```


# SUSE

## Zypper

```bash
# Install a package
zypper install PACKAGE
# Search for a package
zypper search -t PACKAGE
```



# Arch Linux

## Installing packages
```bash
pacman -S [package]
```

