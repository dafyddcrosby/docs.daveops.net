Linux
-----


Force filesystem check on next boot
===================================
{{{
touch /forcefsck
}}}

Socket programming with /dev/tcp
================================
{{{
exec 3<>/dev/tcp/www.google.com/80
echo -e "GET / HTTP/1.1\n\n" >&3
cat <&3
}}}

See what services are using a particular port
=============================================
Run as root:
{{{
lsof -w -n -i (tcp|udp):<port>
}}}
or
{{{
netstat -luntp
}}}

See if hard drive is on its last legs
=====================================
{{{
smartctl -H /dev/sda
}}}

Get reboot/shutdown history
==============================
{{{
last -x
}}}

Get time from Unix timestamp
==============================
{{{
date -d @$TIMESTAMP
}}}

Find all files with a setuid/setgid bit set
===========================================
{{{
find / -perm +6000 -type f -exec ls -ld {} \; > setuid.txt &
}}}

Burn an ISO from the command prompt
===================================
{{{
cdrecord -v -data image.iso
}}}

Delete user, their home directory, and their mailbox
====================================================
{{{
userdel -r [user]
}}}

Add user, home directory
==============================
{{{
useradd -m [user]
}}}

Create system user
==============================
{{{
useradd -r [user]
}}}

See password policies for user
==============================
{{{
chage -l [user]
}}}

Fixing missing shared library
==============================
* Create a .conf file in {{{/etc/ld.so.conf.d/}}} and put the library's directory in it.
* Run {{{ldconfig}}} to reload the system paths

Find files changed in the past day
==================================
{{{
find . -ctime -1 -type f
}}}
Disable caps lock
{{{
setxkbmap -option ctrl:nocaps
}}}

