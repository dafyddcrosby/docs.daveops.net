
# Emacs

# Intro

- C-x - Character eXtend. Followed by one character.
- M-x - Execute Extended Command (Named command eXtend). Followed by a long name
- M-S-x - Execute Extended Command for Buffer - show/run commands relevant to current buffer (Emacs 28+)

| keystrokes | M-x                        | desc         |
|---------- |-------------------------- |------------ |
| C-x C-c    | save-buffers-kill-terminal | quit         |
| C-g        | keyboard-exit              | stop command |
| M-x        | execute-extended-command   |              |


# files

| keys    | M-x               | desc                                |
|------- |----------------- |----------------------------------- |
| C-x C-f |                   | find file                           |
| C-x C-s |                   | save file                           |
| C-x s   |                   | save buffers to files               |
|         | recover-this-file | Recover file from an auto-save file |


## File local variables

Header

```
-*- mode: mode-name; variable: value -*-
```

Footer

```
Local Variables:
mode: mode-name
variable: value
End:
```


# UI


## buffers

| keys    | desc           |
|------- |-------------- |
| C-x b   | switch buffers |
| C-x C-b | list buffers   |


## Tabs

[Online emacs doc - tab bars](https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Bars.html)

| keys    | M-x           | desc                                     |
|------- |------------- |---------------------------------------- |
| C-x t b |               | open new tab with buffer                 |
| C-x t 2 |               | new tab                                  |
| C-tab   |               | swap tab                                 |
|         | tab-bar-mode  | create a tab bar                         |
|         | tab-rename    | renames a tab                            |
|         | tab-line-mode | create a tab line in the window's buffer |


## windows

| keys    | desc                          |
|------- |----------------------------- |
| C-x 0   | delete active window          |
| C-x 1   | one window, kill all others   |
| C-x 2   | split window below            |
| C-x 3   | split window right            |
| C-x 4 b | change buffer in other window |


## frames

| keys    | desc                         |
|------- |---------------------------- |
| C-x 5 2 | Create new frame             |
| C-x 5 b | Switch buffer in other frame |
| C-x 5 0 | Delete active frame          |
| C-x 5 1 | Delete other frames          |


## Fonts

M-x list-colors-display for supported color names


# text manipulation

| keys      | M-x              | desc                                    |
|--------- |---------------- |--------------------------------------- |
|           | auto-fill-mode   | split text                              |
| C-x f     |                  | Set fill mode margin (default 70 chars) |
| C-x 8 e i | emoj-insert      |                                         |
| C-x 8 e s | emoji-search     |                                         |
|           | line-number-mode | line number in status bar               |


# bookmarks

bookmarks are persistent across sessions

| keys    | desc             |
|------- |---------------- |
| C-x r m | Set bookmark     |
| C-x r l | List bookmarks   |
| C-x r b | Jump to bookmark |


# shells

| M-x       | desc                   |
|--------- |---------------------- |
| shell     | comint-driven shell    |
| eshell    | emacs-lisp shell       |
| ansi-term | ANSI terminal emulator |


# help

Append C-h to a prefix key for combination documentation


## Info Mode

| keystrokes | M-x                        | desc                                      |
|---------- |-------------------------- |----------------------------------------- |
| C-h R      | info-display-manual MANUAL | open a specific Info manual               |
| C-h i      | info                       | opens up Info Mode to the builtin manuals |

| Key | Purpose                                   |
|--- |----------------------------------------- |
| [   | previous node                             |
| ]   | next node                                 |
| l   | back in history                           |
| r   | forward in history                        |
| n   | next sibling node                         |
| p   | previous sibling node                     |
| u   | one level to a parent node                |
| SPC | scroll one screen at a time               |
| TAB | cycle through cross-references and links  |
| RET | opens the active link                     |
| m   | prompts for a menu item name and opens it |
| d   | go to the info directory                  |
| q   | closes the info browser                   |


## apropos

| key   | M-x                   | desc                  |
|----- |--------------------- |--------------------- |
|       | apropos               | search **everything** |
| C-h a | apropos-command       | search commands       |
| C-h d | apropos-documentation | search documentation  |


## describe

| key   | M-x               | desc                      |
|----- |----------------- |------------------------- |
| C-h m | describe-mode     | major mode documentation  |
| C-h x | describe-command  | command documentation     |
| C-h f | describe-function | function documentation    |
| C-h v | describe-variable | variable documentation    |
| C-h k | describe-key      | key binding documentation |

| key | desc                |
|--- |------------------- |
| i   | open info manual    |
| s   | go to source        |
| c   | customize interface |


# Modes

| M-x             | desc                                            |
|--------------- |----------------------------------------------- |
| fundmental-mode | Fundamental mode (apostrophe is word separator) |
| text-mode       | Text mode (intended for human language)         |


## Customize

`M-x customize` to customize settings, `M-x customize-browse` for tree mode, `M-x customize-themes` for themes


## Org Mode

Possibly the killer app for Emacs

[Org Mode Website](https://orgmode.org/)


### Documentation

[Org Mode Manual](https://orgmode.org/manual/)


### Hyperlinking

| desc                 | keystrokes |
| ---                  | ---        |
| insert link          | C-c C-l    |
| learn to open a link | C-c C-o    |
| jump back            | C-c &      |


#### Interlinking

```
[[LINK][[[]]LINK][DESCRIPTION]]
```


#### Link abbreviations (custom link targets)

[orgmode docs on link abbreviations](https://orgmode.org/manual/Link-Abbreviations.html)

```elisp
(setq org-link-abbrev-alist
      '(("duckduckgo" . "https://duckduckgo.com/?q=%s")
        ("wp"         . "https://en.wikipedia.org/wiki/%s")
        ("omap"       . "https://nominatim.openstreetmap.org/ui/search.html?q=%s&polygon=1")))
```


### Agenda

| desc             | keystroke |
| ---              | ---       |
| open agenda mode | C-c a     |


### Org Roam

[Org Roam website](https://www.orgroam.com/)

[Org Roam Manual](https://www.orgroam.com/manual.html)


#### Aliasing for easier roam navigation

```
:ROAM_ALIASES:
```


## Bug Reference Mode

Built-in in version 28.2

```elisp
((nil . ((bug-reference-url-format . "https://example.org/browse/%s")
         (bug-reference-bug-regexp . "\\(\\[\\([A-Z]+-[0-9]+\\)\\]\\)"))))
```


## Abbrev Mode

[EmacsWiki for Abbrev Mode](https://www.emacswiki.org/emacs/AbbrevMode)

| desc                                       | command                   | keystroke |
| ---                                        | ---                       | ---       |
| define a global abbrev                     | add-global-abbrev         | C-x a g   |
| define a local (mode-specific) abbrev with | add-mode-abbrev           | C-x a l   |
| define a word in the buffer (ie inverse)   | inverse-add-global-abbrev | C-x a i g |
| cancel an expansion                        | quoted-insert             | C-q       |
| define an abbrev from minibuffer           | define-global-abbrev      |           |
| save abbrevs when in abbrev mode           | edit-abbrevs-redefine     | C-c C-c   |


### Editing abbrevs

| desc                                               | command           |
|-------------------------------------------------- |----------------- |
| write lisp file of user-created abbrev definitions | write-abbrev-file |
| edit in-memory abbrev definitions                  | edit-abbrevs      |
| read definitions file                              | read-abbrev-file  |


## Help mode

| desc                            | command          | key   |
| ---                             | ---              | ---   |
| all keystrokes                  | help-for-help    | C-h ? |
| all keybindings                 | describe-binding | C-h b |
| show manuals                    |                  | C-h i |
| display documentation for a key | describe-key     | C-h k |
| module info                     | describe-mode    | C-h m |
| show version changelog          |                  | C-h n |

| desc          | key |
| ---           | --- |
| next page     | n   |
| previous page | p   |
| up            | u   |


## BibTeX

| C-c C-e a | bibtex-article        | Add an article                               |
| C-c C-e b | bibtex-book           | Add a book                                   |
| C-c C-e ? |                       | Show entry types and shortcuts               |
| C-j       | bibtex-next-field     | Jump to next field                           |
| C-down    | bibtex-next-entry     | Jump to the next entry                       |
| C-up      | bibtex-previous-entry | Jump to the previous entry                   |
| C-c C-c   | bibtex-clean-entry    | Clean the entry                              |
| C-c C-q   | bibtex-fill-entry     | Align the fields                             |
| -         | bibtex-reformat       | Reformat all entries inregion or buffer      |
| -         | bibtex-sort-buffer    | Sort all entries in the buffer by their keys |


### BibTeX in Org Mode

Foo is file name, plain is bibliography mode

```
#+BIBLIOGRAPHY: foo plain
```


# Emacs Community and Development

| key     | command          | desc         |
| ---     | ---              | ---          |
| C-h C-t | view-emacs-todo  | TODO list    |
| C-h n   | view-emacs-news  | version info |
|         | report-emacs-bug | report a bug |


## Email lists

<http://savannah.gnu.org/mail/?group=emacs>


## Source code

git://git.sv.gnu.org/emacs.git


## EmacsConf

Conference typically in December

<https://emacsconf.org/>


# Emacs server/client mode

|                 |             |
|---------------- |------------ |
| M-x server-start | start server |


## emacsclient

| -c  | graphical frame |
| -nw | terminal frame  |


# Design of Emacs

[Emacs Paper by RMS](https://www.gnu.org/software/emacs/emacs-paper.html)

> A sign of the success of the EMACS design is that EMACS has been requested by over a hundred sites and imitated at least ten times.

The extensibility of EMACS is the point, not the key commands. Any editor where interpreter facilities are always available at runtime could (more or less) do the same thing.

One argument made was that because Emacs was written in Lisp, it pulled Lisp from the ivory towers into systems programming. Not sure I agree with that one totally (especially given the jankiness of [Emacs Lisp](#org1c4e854)), but it's an interesting use of it.


<a id="org1c4e854"></a>

# Emacs Lisp


## Documentation and Resources

[An Introduction to Programming in Emacs Lisp](https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html)


## Emacs Lisp

| M-x                   | desc                                      |
|--------------------- |----------------------------------------- |
| ielm                  | Emacs Lisp REPL                           |
| lisp-interaction-mode | Enable Lisp interaction mode              |
| scratch-buffer        | new scratch buffer for Emacs Lisp testing |
| eval-buffer           | evaluate entire buffer                    |
| eval-region           | evaluate marked region                    |


## Lexical binding

To enable lexical-binding in a file:

```elisp
;; -*- lexical-binding: t -*-
```


## Keystrokes

| desc                    | keystroke |
|----------------------- |--------- |
| execute line            | C-x C-e   |
| execute, dump in buffer | C-j       |


# Platform support


## Emacs on Linux


### Running as a user systemd service

Write to `~/.config/systemd/user/emacs.service`

```
[Unit]
Description=Emacs text editor
Documentation=info:emacs man:emacs(1) https://gnu.org/software/emacs/

[Service]
Type=forking
ExecStart=/usr/bin/emacs --daemon
ExecStop=/usr/bin/emacsclient --eval "(kill-emacs)"
Environment=SSH_AUTH_SOCK=%t/keyring/ssh
Restart=on-failure

[Install]
WantedBy=default.target
```


### Gnome desktop icon

`~/.local/share/applications/emacs-client.desktop`

```shell
desktop-file-validate ~/.local/share/applications/emacs-client.desktop
xdg-desktop-menu install ~/.local/share/applications/emacs-client.desktop
xdg-desktop-icon install ~/.local/share/applications/emacs-client.desktop
```

```
[Desktop Entry]
Name=Emacs (client)
GenericName=Text Editor
Comment=Edit text
MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
Exec=/usr/bin/emacsclient --create-frame --alternate-editor "emacs" %F
Icon=emacs
Type=Application
Terminal=false
Categories=Development;TextEditor;
StartupNotify=true
StartupWMClass=Emacs
```


### Build from source (Fedora)

```sh
sudo dnf install \
  texinfo gnutls-devel autoconf make gcc ncurses-devel sqlite-devel \
  libtree-sitter libtree-sitter-devel tree-sitter-cli \ # tree-sitter support
  sqlite-devel \ # SQLite3 support
  jansson-devel \ # Faster JSON for LSP
  gtk3-devel giflib-devel libXpm-devel \ # Graphical install
  libgccjit-devel # Native lisp compiler

git clone git://git.sv.gnu.org/emacs.git
cd emacs
sh autogen.sh
./configure
```


## Emacs on MacOS


### macOS daemon

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


## Use Emacs for scripting

> If you start an executable script with
> 
> \#!/usr/bin/emacs -x
> 
> Emacs will start without reading any init files (like with '--quick'), and then execute the rest of the script file as Emacs Lisp. When it reaches the end of the script, Emacs will exit with an exit code from the value of the final form.


# Fun and Games

- [Emacs standing alone on a Linux Kernel](http://www.informatimago.com/linux/emacs-on-user-mode-linux.html)

| M-x          | desc                 |
|------------ |-------------------- |
| telnet       | telnet to a server   |
| calc         | calc mode            |
| lunar-phases | phases of the moon   |
| doctor       | "doctor" chatbot     |
| emacs-uptime | Emacs uptime counter |


# Emacs Package Management


## package.el

Standard with Emacs 24

| desc                             | M-x                      |
|-------------------------------- |------------------------ |
| install a package                | package-install          |
| remove unused dependent packages | package-autoremove       |
| get list of installable packages | package-list-packages    |
| refresh package repository       | package-refresh-contents |

```elisp
; FSF
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/packages/"))
; Milkypostman's ELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
```


### help

| keystrokes | desc             |
|---------- |---------------- |
| C-h P      | describe package |


### Package

| key | desc                      |
|--- |------------------------- |
| i   | mark for installation     |
| u   | unmark                    |
| x   | execute action on package |


## use-package

Nice way to keep configuration of emacs packages tidy

Standard with Emacs 29

[use-package GitHub page](https://github.com/jwiegley/use-package/)


## Quelpa

[Quelpa](https://github.com/quelpa/quelpa) uses the MELPA package recipes and builds them locally from source

`quelpa-upgrade-all` to upgrade all packages

`quelpa RET package_name` to install a package

`(quelpa 'package-name)` to use in Emacs lisp


## Repositories

[EmacsWiki: ELPA](https://www.emacswiki.org/emacs/ELPA)

<https://elpa.gnu.org/>

Emacs Lisp Package Archive


# Third-Party Packages


## Evil Mode

| desc                         | keystrokes |
|---------------------------- |---------- |
| toggle evil mode             | C-z        |
| put emacs to background mode | C-x C-z    |


## SLIME

<https://github.com/slime/slime>

| desc         | keystroke |
|------------ |--------- |
| execute line | C-c C-e   |


## Emacspeak

Text-to-speech interface

- <https://emacspeak.sourceforge.net/>
- <http://tvraman.github.io/emacspeak/manual/>
