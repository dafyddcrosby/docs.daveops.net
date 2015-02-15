Tornado
-------

SSL
===
.. code-block:: python

 server = tornado.httpserver.HTTPServer(application, ssl_options={
     "certfile": "/path/to/ssl/cert.crt",
     "keyfile": "/path/to/ssl/tornado_key.pem",
 })
 server.bind(port)
 server.start(0)
 tornado.ioloop.IOLoop.instance().start()
