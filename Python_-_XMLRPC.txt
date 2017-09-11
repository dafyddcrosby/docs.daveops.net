Python - XMLRPC
===============
:tags: Python 

Connecting to an XMLRPC server
------------------------------
.. code-block:: python

 import xmlrpclib
 
 server = 'http://blah.com/XMLRPC'
 svr = xmlrpclib.Server(server)
 
 session_id = '2'
 params = { "session_id" : int(session_id)}
 response = svr.foo.bar(params)
