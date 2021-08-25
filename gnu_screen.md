---
title: GNU Screen
---

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

