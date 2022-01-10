---
title: GNOME
---

## Turn caps lock into control key

```bash
dconf write /org/gnome/desktop/input-sources/xkb-options "['ctrl:nocaps']"
```


---
title: GDM
---

## Add a WM to GDM dropdown

Add a ``.desktop`` file to ``/usr/share/xsessions``

