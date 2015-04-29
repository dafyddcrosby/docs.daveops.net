Apache
======
:date: 2015-04-28

Disable SSL 2/3
---------------
::

 SSLProtocol All -SSLv2 -SSLv3

Rotate logs
-----------
::

 kill -SIGHUP httpd

Misc
----

* `Mozilla SSL Configuration Generator <https://mozilla.github.io/server-side-tls/>`_
