---
title: tmux
---

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
