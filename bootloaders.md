# Bootloaders


# GRUB

Except where noted, this is for GRUB 2+


## Console

```shell
# Turn on the pager in console mode
set pager=1
# See what the variables are
set
# Find devices available
ls
```


## Grub grub.cfg creation

```shell
# RHEL-ish
grub2-mkconfig -o /boot/grub2/grub.cfg
# Deb-ish
update-grub
```


## GRUB 1

- `/boot/grub/grub.conf`


# LILO

The Linux Loader, which is reinstalled with each config change (by running `lilo`)

- `/etc/lilo.conf`

| arg          | desc                        |
|------------ |--------------------------- |
| large-memory | allow ram disk above 15MB   |
| lba32        | use logical block addresses |
