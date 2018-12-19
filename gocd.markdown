# GoCD
Enable SSL debugging
--------------------
::

   # In /etc/default/go-server, add:
   GO_SERVER_SYSTEM_PROPERTIES="$GO_SERVER_SYSTEM_PROPERTIES -Djavax.net.debug=ssl:record"
   # Restart server
   # Debug logs go to go-server.log

