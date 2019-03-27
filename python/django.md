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
