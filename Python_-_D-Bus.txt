Python - D-Bus
--------------
:tags: Python 


Connecting to D-Bus
==============================
.. code-block:: python

 import dbus
 
 bus = dbus.SystemBus()
 try:
     thing = bus.get_object('com.tech.Thing', '/com/tech/Thing')
 except:
     print("Oh snap, couldn't connect")
