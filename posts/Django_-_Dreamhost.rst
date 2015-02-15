Django - Dreamhost
------------------
:tags: django 

Restarting Passenger
==============================
::

 touch ~/example.com/tmp/restart.txt

Settings for Passenger
==============================
.. code-block:: python

 import sys, os
 
 sys.path.append(os.getcwd())
 sys.path.append(os.path.join(os.getcwd(), 'project'))
 
 # Prepend virtualenv to path so that it's loaded first
 sys.path.insert(0,'/home/user/example.com/venv/bin')
 sys.path.insert(0,'/home/user/example.com/venv/lib/python2.6/site-packages/Django-1.4.3-py2.6.egg-info')
 sys.path.insert(0,'/home/user/example.com/venv/lib/python2.6/site-packages')
 
 os.environ['DJANGO_SETTINGS_MODULE'] = "project.settings"
 import django.core.handlers.wsgi
 application = django.core.handlers.wsgi.WSGIHandler()
