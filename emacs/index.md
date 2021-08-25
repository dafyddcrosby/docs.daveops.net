---
title: Emacs
---

## packages

keystrokes                   | desc
---                          | ---
M-x list-packages            | lists installed packages and packages available from repos
M-x package-refresh-contents | Update package list
C-h P <package>              | describe package

key | desc
--- | ---
i   | mark for installation
u   | unmark
x   | execute action on package

## misc

keystrokes           | desc
---                  | ---
C-h C-t              | show Emacs TODO
C-h m                | show module info
M-x telnet           | telnet to a server
M-x report-emacs-bug | report a bug
M-x calc             | calc mode

## macOS daemon

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
    "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
 <plist version="1.0">
  <dict>
    <key>Label</key>
    <string>emacs.daemon</string>
    <key>ProgramArguments</key>
    <array>
      <string>/usr/local/bin/emacs</string>
      <string>--daemon</string>
    </array>
   <key>RunAtLoad</key>
   <true/>
   <key>ServiceDescription</key>
   <string>Emacs Daemon</string>
  </dict>
</plist>
```

```bash
launchctl load -w ~/Library/LaunchAgents/emacs.daemon.plist
```

## Links

* [Emacs standing alone on a Linux Kernel](http://www.informatimago.com/linux/emacs-on-user-mode-linux.html)
