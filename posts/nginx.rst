nginx
-----


Rotate logs
==============================
{{{
mv access.log access.log.0
kill -USR1 `cat master.nginx.pid`
sleep 1
gzip access.log.0    # do something with access.log.0
}}}

