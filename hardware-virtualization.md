# Hardware Virtualization


# QEMU

<http://www.qemu-advent-calendar.org/>

<http://wiki.qemu.org/Main_Page> <http://wiki.qemu.org/Documentation/GettingStartedDevelopers> <http://wiki.qemu.org/Documentation/ISAManuals>


# VirtualBox

```shell
# list running VMs
VBoxManage list runningvms
```


## Installing VirtualBox Extension Pack

- <https://www.virtualbox.org/wiki/Downloads>
- (as root) `VBoxManage extpack install <tarball>`


## Vagrant

Install vagrant-vbguest to use Guest Additions


# Vagrant

<https://www.vagrantup.com/>


## Install CentOS

```shell
vagrant init centos/7
vagrant up
vagrant ssh
```


# Firecracker

<https://firecracker-microvm.github.io/>
