---
title: Django - admin mode
tags: ["django"]
---

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

