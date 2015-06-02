Apache
======
:date: 2015-04-28

Rotate logs
-----------
::

 kill -SIGHUP httpd

General Hardening
-----------------

SSL Config
^^^^^^^^^^
::

 SSLProtocol          All -SSLv2 -SSLv3
 SSLHonorCipherOrder  on
 SSLCompression       off
 # HSTS (mod_headers is required) (15768000 seconds = 6 months)
 Header always set Strict-Transport-Security "max-age=15768000"

General Apache Config
^^^^^^^^^^^^^^^^^^^^^
::

 # Disable product version
 ServerTokens Prod
 ServerSignature Off

Misc
----

* `Mozilla SSL Configuration Generator <https://mozilla.github.io/server-side-tls/ssl-config-generator>`_
