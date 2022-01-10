---
title: Django
tags: ["django"]
---

Start project
-------------



 django-admin.py startproject project_name

Start demo server
-----------------



 python manage.py runserver

Check for construction errors in models
---------------------------------------



 python manage.py validate

Put models in database
----------------------



 python manage.py syncdb

Shell
-----



 python manage.py shell

Permanent redirect
------------------

in urls.py's urlpatterns

```python
url(r'^$', lambda x: HttpResponsePermanentRedirect('/n
```


# Dreamhost

Restarting Passenger
--------------------

``touch ~/example.com/tmp/restart.txt``

Settings for Passenger
----------------------

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



## Enabling admin mode

* Uncomment "django.contrib.admin" in the ~INSTALLED_APPS setting.
* `./manage.py syncdb`
* Uncomment admin lines in urls.py
* Create admin.py in your app:

```python
from django.contrib import admin
from app.models import Thing

admin.site.register(Thing)
```

## Reset admin password w/o password reset

```bash
./manage.py shell
```

```python
from django.contrib.auth.models import User
users = User.objects.all()
users
users[0].set_password('whatever');
users[0].save()
```



# South

Installing in a project
-----------------------


* ``pip install South``
* add 'south' to ~INSTALLED_APPS
* ``./manage.py syncdb``


Adding South to an existing project
-----------------------------------


* add 'south' to ~INSTALLED_APPS
* ``./manage.py syncdb``
* ``./manage.py convert_to_south myapp``


Initial migrate schema creation
-------------------------------



 ./manage.py schemamigration app --initial
 ./manage.py migrate app

Change schema
-------------



 ./manage.py schemamigration app --auto
 ./manage.py migrate app





# Django - taggit

## Installing in a project

* ``pip install django-taggit``
* add 'taggit' to ~INSTALLED_APPS
* ``./manage.py syncdb``


```python
from django.db import models
from taggit.managers import TaggableManager

class Thing(models.Model):
# ... fields here

tags = TaggableManager()
```
