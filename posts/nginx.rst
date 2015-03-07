nginx
=====

Rotate logs
-----------
::

 mv access.log access.log.0
 kill -USR1 `cat master.nginx.pid`
 sleep 1
 gzip access.log.0    # do something with access.log.0

A funny aside
-------------

One of my coworkers said that "nginx is just a hipster Apache". I *wish* that's
the most ignorant thing he's ever said (sit down with me for a beer, and I'll
give a few other gems).

* `The C10K Problem <http://www.kegel.com/c10k.html>`_
