---
title: upstart
---

Included in CentOS 6, Ubuntu

<http://upstart.ubuntu.com/>

Scripts in `/etc/init`

Disable services by using .override instead of .conf on the files in /etc/init

```bash
echo manual > /etc/init/apache2.override
```
