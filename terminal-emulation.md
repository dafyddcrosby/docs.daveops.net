# Terminal Emulation

https://en.wikipedia.org/wiki/Terminal_emulator

# DEC VTs

https://vt100.net/

# Terminal Emulators

## Alacritty
Comes with [X Windows](x_windows.md)
- https://alacritty.org/
- [GitHub](https://github.com/alacritty/alacritty)

## xterm

Command         | Keys
---             | ---
Paste clipboard | shift + insert
Main menu       | ctrl + left click
Font menu       | ctrl + right click

# Using bash to make TUIs

https://github.com/dylanaraps/writing-a-tui-in-bash

# ncurses

- [Home](https://invisible-island.net/ncurses/)
- [FAQ](https://invisible-island.net/ncurses/ncurses.faq.html)

# Notcurses

A much prettier (albeit less compliant) TUI library

- [GitHub](https://github.com/dankamongmen/notcurses)
- [Home](https://nick-black.com/dankwiki/index.php/Notcurses)



# tmux

## Share session

```bash
# Create session
tmux -S /tmp/tmsession
chmod 777 /tmp/tmsession
# Connect
tmux -S /tmp/tmsession attach
```

## Keyboard shortcuts

Shortcut         | Description
---              | ---
C-b ?            | List keybindings
C-b "            | Split screen horizontally
C-b %            | Split screen vertically
C-b arrow        | Switch pane
C-b (held) arrow | Change size of pane
C-b ;            | Switch to last active pane
C-b x            | Kill pane
C-b c            | New window
C-b n            | Next window
C-b p            | Previous window
C-b &            | Kill window
C-b PgUp/PgDn    | Scroll mode
C-b .            | Move window


# GNU Screen

```bash
# Reattach a terminal
screen -r
# Attach to a non-detached session (pair-programming)
screen -x

# Share session
screen -d -m -S (session name)
screen -x (session name)
```

## Keyboard shortcuts

Shortcut | Command       | Description
---      | ---           | ---
C-a "    | windowlist -b | list all windows
C-a C-a  | other         | go to previous screen
C-a C-d  | detach        | detach from screen back to the terminal
C-a k    | kill          | kill window
C-a H    | log           | log the current window
C-a C    |               | new window

## Scrollback

	C-a :
	scrollback <lines>
	C-a [

