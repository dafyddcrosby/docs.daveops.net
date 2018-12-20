# Python - XMLRPC
@Python 

Connecting to an XMLRPC server
------------------------------

```python

 import xmlrpclib
 
 server = '<http://blah.com/XMLRPC>'
 svr = xmlrpclib.Server(server)
 
 session_id = '2'
 params = { "session_id" : int(session_id)}
 response = svr.foo.bar(params)
```
