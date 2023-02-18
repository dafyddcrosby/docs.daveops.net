
# Emacs on Linux

# Running as a user systemd service

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


# Gnome desktop icon

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