SystemD
=======
:date: 2016-03-07
:modified: 2017-02-20
:tags: init, Linux, Red Hat

| Command | Notes |
| systemctl | List services |
| systemctl start SERVICE | Used to start a service (not reboot persistent)
| systemctl stop SERVICE |  Used to stop a service (not reboot persistent)
| systemctl restart SERVICE |      Used to stop and then start a service
| systemctl reload SERVICE |       When supported, reloads the config file without interrupting pending operations.
| systemctl condrestart SERVICE |  Restarts if the service is already running.
| systemctl status SERVICE |       Tells whether a service is currently running.
| systemctl enable SERVICE |       Turn the service on, for start at next boot, or other trigger.
| systemctl disable SERVICE |      Turn the service off for the next reboot, or any other trigger.
| systemctl is-enabled SERVICE |   Used to check whether a service is configured to start or not in the current environment.
| systemctl list-unit-files --type=service | Print a table of services that lists which runlevels each is configured on or off
| systemctl daemon-reload |        Used when you create a new service file or modify any configuration 

Directories
-----------

- /etc/systemd/system/\*.wants/SERVICE.service - Used to list what levels this service is configured on or off

Single User Mode
----------------
Add the following to the kerneel arguments `systemd.unit=rescue.target`
