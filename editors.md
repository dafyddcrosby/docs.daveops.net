# Editors


# Emacs

- C-x - Character eXtend. Followed by one character.
- M-x - Named command eXtend. Followed by a long name.

| keystrokes | desc         |
|---------- |------------ |
| C-x C-c    | quit         |
| C-g        | stop command |


## files

| keystrokes            | desc                                |
|--------------------- |----------------------------------- |
| C-x C-f               | find file                           |
| C-x C-s               | save file                           |
| C-x s                 | save buffers to files               |
| M-x recover-this-file | Recover file from an auto-save file |


## buffers, windows, tabs

| keystrokes | desc                        |
|---------- |--------------------------- |
| C-x b      | switch buffers              |
| C-x C-b    | list buffers                |
| C-x 1      | one window, kill all others |
| C-x t b    | open new tab with buffer    |


## help

| keystrokes | desc                   |
|---------- |---------------------- |
| C-h C-t    | show Emacs TODO        |
| C-h m      | show module info       |
| C-h n      | show version changelog |
| C-h i      | show manuals           |


## modes

| keystrokes          | desc                                            |
|------------------- |----------------------------------------------- |
| M-x fundmental-mode | Fundamental mode (apostrophe is word separator) |
| M-x text-mode       | Text mode (intended for human language)         |


## misc text

| keystrokes         | desc                                    |
|------------------ |--------------------------------------- |
| M-x auto-fill-mode | split text                              |
| C-x f              | Set fill mode margin (default 70 chars) |


## misc

| keystrokes           | desc               |
|-------------------- |------------------ |
| M-x telnet           | telnet to a server |
| M-x report-emacs-bug | report a bug       |
| M-x calc             | calc mode          |
| M-x lunar-phases     | phases of the moon |
| M-x doctor           | "doctor" chatbot   |


## evil mode

| keystrokes | desc                         |
|---------- |---------------------------- |
| C-z        | toggle evil mode             |
| C-x C-z    | put emacs to background mode |

- [Emacs standing alone on a Linux Kernel](http://www.informatimago.com/linux/emacs-on-user-mode-linux.html)


## packages

| keystrokes                   | desc                                                       |
|---------------------------- |---------------------------------------------------------- |
| M-x list-packages            | lists installed packages and packages available from repos |
| M-x package-refresh-contents | Update package list                                        |
| C-h P                        | describe package                                           |

| key | desc                      |
|--- |------------------------- |
| i   | mark for installation     |
| u   | unmark                    |
| x   | execute action on package |


## macOS daemon

```
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

```shell
launchctl load -w ~/Library/LaunchAgents/emacs.daemon.plist
```


## Emacs Lisp

| ks      | desc                    |
|------- |----------------------- |
| C-x C-e | execute line            |
| C-j     | execute, dump in buffer |


## SLIME

| ks      | desc         |
|------- |------------ |
| C-c C-e | execute line |


# VSCode


## Keyboard shortcuts

| shortcut      | desc                   |
|------------- |---------------------- |
| ctrl-k ctrl-s | keyboard shortcut menu |
| ctrl-\\       | split pane             |