# VNC
::

 ssh desthost -L 5900:localhost:5900
 x11vnc -display :0 -nopw
 vncviewer :0

Testing a connection
--------------------

::

  nc -v <host> <port>
  # should return "RFB ..."

