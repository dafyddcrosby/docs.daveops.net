
# Graphics Libraries

# SDL

- <https://www.libsdl.org/>
- [Go SDL2 bindings](https://github.com/veandco/go-sdl2)
- <http://sdltutorials.com>


# GNOME


## Move windows

Alt-F7 to move a finicky window


## Turn caps lock into control key

```shell
dconf write /org/gnome/desktop/input-sources/xkb-options "['ctrl:nocaps']"
```


## GDM


### Add a WM to GDM dropdown

Add a `.desktop` file to `/usr/share/xsessions`


# Wayland

- <https://github.com/natpen/awesome-wayland>


# X Windows

- [X and NeWS history](http://minnie.tuhs.org/pipermail/tuhs/2017-September/010471.html)

[XTerm](terminal-emulation.md)


## xrandr


### Make PAL card output NTSC

After the first two lines in `/etc/gdm/Init/Default` , put:

```
xrandr --output S-video --set “tv standard” ntsc
```