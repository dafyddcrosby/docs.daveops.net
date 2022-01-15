# Graphics Libraries
# SDL

* <https://www.libsdl.org/>
* [Go SDL2 bindings](https://github.com/veandco/go-sdl2)


# GNOME

## Turn caps lock into control key

```bash
dconf write /org/gnome/desktop/input-sources/xkb-options "['ctrl:nocaps']"
```


# GDM

## Add a WM to GDM dropdown

Add a ``.desktop`` file to ``/usr/share/xsessions``

