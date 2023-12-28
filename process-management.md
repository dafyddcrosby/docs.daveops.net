
# Process Management

# Daemontools

| flag | action                                                                                                            |
|---- |----------------------------------------------------------------------------------------------------------------- |
| -u   | Up. If the service is not running, start it. If the service stops, restart it.                                    |
| -d   | Down. If the service is running, send it a TERM signal and then a CONT signal. After it stops, do not restart it. |
| -o   | Once. If the service is not running, start it. Do not restart it if it stops.                                     |
| -p   | Pause. Send the service a STOP signal.                                                                            |
| -c   | Continue. Send the service a CONT signal.                                                                         |
| -h   | Hangup. Send the service a HUP signal.                                                                            |
| -a   | Alarm. Send the service an ALRM signal.                                                                           |
| -i   | Interrupt. Send the service an INT signal.                                                                        |
| -t   | Terminate. Send the service a TERM signal.                                                                        |
| -k   | Kill. Send the service a KILL signal.                                                                             |


# upstart

Included in CentOS 6, Ubuntu

<http://upstart.ubuntu.com/>

Scripts in `/etc/init`

Disable services by using .override instead of .conf on the files in /etc/init

```shell
echo manual > /etc/init/apache2.override
```


# init

Traditional init systems used `/etc/inittab`


## Runlevels

```shell
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

```shell
# List services
chkconfig --list

# Turn httpd on at levels 2 and 4
chkconfig --level 24 httpd on
```


## LSB

| level | desc                          |
|----- |----------------------------- |
| 0     | halt                          |
| 1     | single user mode              |
| 2     | multi user mode               |
| 3     | multi user mode w/ networking |
| 4     | not used/user-definable       |
| 5     | runlevel 3 plus graphics      |
| 6     | reboot                        |
