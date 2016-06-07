Linux
=====
:date: 2015-03-07

force filesystem check on next boot
-----------------------------------
::

 touch /forcefsck

Socket programming with /dev/tcp
--------------------------------
::

 exec 3<>/dev/tcp/www.google.com/80
 echo -e "GET / HTTP/1.1\n\n" >&3
 cat <&3

See what services are using a particular port
---------------------------------------------
Run as root:

::

 lsof -w -n -i (tcp|udp):<port>

or

::

  netstat -luntp

See if hard drive is on its last legs
-------------------------------------
::

 smartctl -H /dev/sda

Get reboot/shutdown history
---------------------------
::

 last -x

Get time from Unix timestamp
----------------------------
::

 date -d @$TIMESTAMP

Find all files with a setuid/setgid bit set
-------------------------------------------
::

 find / -perm +6000 -type f -exec ls -ld {} \; > setuid.txt &

Burn an ISO from the command prompt
-----------------------------------
::

 cdrecord -v -data image.iso

Delete user, their home directory, and their mailbox
----------------------------------------------------
::

 userdel -r [user]

Add user, home directory
------------------------
::

 useradd -m [user]

Create system user
------------------
::

 useradd -r [user]

See password policies for user
------------------------------
::

 chage -l [user]

Fixing missing shared library
-----------------------------
* Create a .conf file in `/etc/ld.so.conf.d/` and put the library's directory in it.
* Run `ldconfig` to reload the system paths

Find files changed in the past day
----------------------------------
::

 find . -ctime -1 -type f

Disable caps lock
-----------------
::

 setxkbmap -option ctrl:nocaps

Set time on machine that doesn't have NTP
-----------------------------------------
::

 date --set="$(ssh user@server date)"

Inter-user communication
------------------------
::

 # Get list of logged in users
 who
 # Send message to all users
 wall [message]
 # Send message to another user's terminal
 write user [ttyname]
 # Enable/disable terminal message
 mesg [n|y]

Assembly
--------
System call table located at ``/usr/include/asm/unistd.h``
Red Hat syscall man pages installed with ``man-pages`` RPM. ``man 2 syscalls`` for a list, ``man 2 <syscall>`` for the syscall.

Put syscall in EAX, put arguments in other ExX registers, call the interrupt, result usually in EAX

Get filesystems kernel can use
------------------------------
::

 cat /proc/filesystems


.. TODO
   https://perf.wiki.kernel.org/index.php/Tutorial
