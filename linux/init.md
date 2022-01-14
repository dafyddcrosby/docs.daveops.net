# init

Traditional init systems used `/etc/inittab`

## Runlevels

```bash
# See the current runlevel
runlevel
who -r

# Change runlevel
init 5
telinit 5
```

## /etc/rc.d

File links that start with S start the service, K stops the service. Numbers determine order of operation.

## chkconfig

Generally only available on RHEL-ish

```bash
# List services
chkconfig --list

# Turn httpd on at levels 2 and 4
chkconfig --level 24 httpd on
```

## LSB

level | desc
---   | ---
0     | halt
1     | single user mode
2     | multi user mode
3     | multi user mode w/ networking
4     | not used/user-definable
5     | runlevel 3 plus graphics
6     | reboot
