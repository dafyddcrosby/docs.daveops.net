# Process Management

# Daemontools

flag | action
---  | ---
-u   | Up. If the service is not running, start it. If the service stops, restart it.
-d   | Down. If the service is running, send it a TERM signal and then a CONT signal. After it stops, do not restart it.
-o   | Once. If the service is not running, start it. Do not restart it if it stops.
-p   | Pause. Send the service a STOP signal.
-c   | Continue. Send the service a CONT signal.
-h   | Hangup. Send the service a HUP signal.
-a   | Alarm. Send the service an ALRM signal.
-i   | Interrupt. Send the service an INT signal.
-t   | Terminate. Send the service a TERM signal.
-k   | Kill. Send the service a KILL signal.


# upstart

Included in CentOS 6, Ubuntu

<http://upstart.ubuntu.com/>

Scripts in `/etc/init`

Disable services by using .override instead of .conf on the files in /etc/init

```bash
echo manual > /etc/init/apache2.override
```
