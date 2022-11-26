# Terminal Emulation

<https://en.wikipedia.org/wiki/Terminal_emulator>


# tmux


## Share session

```shell
# Create session
tmux -S /tmp/tmsession
chmod 777 /tmp/tmsession
# Connect
tmux -S /tmp/tmsession attach
```


## Keyboard shortcuts

| Shortcut         | Description                |
|---------------- |-------------------------- |
| C-b ?            | List keybindings           |
| C-b "            | Split screen horizontally  |
| C-b %            | Split screen vertically    |
| C-b arrow        | Switch pane                |
| C-b (held) arrow | Change size of pane        |
| C-b ;            | Switch to last active pane |
| C-b x            | Kill pane                  |
| C-b c            | New window                 |
| C-b n            | Next window                |
| C-b p            | Previous window            |
| C-b &            | Kill window                |
| C-b PgUp/PgDn    | Scroll mode                |
| C-b .            | Move window                |


## Adding to tmux buffers from shell

```shell
echo "hello" | tmux loadb -
tmux saveb - | sed 's,hello,hi,'
```


# SSH


## Generating a new key

```shell
# Generate a new RSA keypair
ssh-keygen -t rsa -b 4096 -C "<username> generated <date>" -f id_rsa
# Generate a new ed25519 keypair
ssh-keygen -t ed25519 -C "<username> generated <date>"
```


## Getting key fingerprint

```shell
ssh-keygen -lf .ssh/id_rsa.pub
```


## Security notes


### General

- Use protocol 2


### Client

- If using ssh-add (ie the ssh-agent), also use `-c` and `-t <seconds>` arguments, and use an askpass program to confirm connections
- Use HashKnownHosts to obscure which hosts you connect to. To retroactively do this on an existing file `ssh-keygen -H`


### Server

- Use something like fail2ban, and rate limit incoming connections

```
IgnoreRhosts yes
RhostsRSAAuthentication no
HostbasedAuthentication no

# Don't use tunneled cleartext passwords
PubkeyAuthentication yes
PasswordAuthentication no
PermitEmptyPasswords no
ChallengeResponseAuthentication no

# Disable root user login
PermitRootLogin no

UsePam yes

# Disable X11 forwarding
X11Forwarding no
# Disable TCP forwarding (unless you *actually* need it)
AllowTcpForwarding no

# Lock down to specific group of users 
AllowGroup ssh_users

HostbasedAuthentication no
PermitUserEnvironment no
StrictModes yes
UsePrivilegeSeparation yes
```


## Troubleshooting

As silly as some of these seem, I've seen them all apply at one point or another.


### Client-side

- Are you using the correct key?
    - Confirm this using the -i flag

- Have you ordered your flags correctly?
    - Confirm this by looking at the man page - order is important!

- Is your username correct?
- Have you confirmed you're trying to connect to the right box?
    - Is the DNS record connect? is your SSH client correctly resolving to that address?
    - Do you need to have a VPN turned on?
    - Is your routing correct?
    - Does the fingerprint match?


### Server-side

- Is sshd running?
- Is the firewall open?
- Are the logs showing any connections?


# DEC VTs

<https://vt100.net/>


# Terminal Emulators


## Alacritty

- <https://alacritty.org/>
- [GitHub](https://github.com/alacritty/alacritty)


## xterm

Comes with [X Windows](x_windows.md)

| Command         | Keys               |
|--------------- |------------------ |
| Paste clipboard | shift + insert     |
| Main menu       | ctrl + left click  |
| Font menu       | ctrl + right click |


# Using bash to make TUIs

<https://github.com/dylanaraps/writing-a-tui-in-bash>


# ncurses

- [Home](https://invisible-island.net/ncurses/)
- [FAQ](https://invisible-island.net/ncurses/ncurses.faq.html)


# Notcurses

A much prettier (albeit less compliant) TUI library

- [GitHub](https://github.com/dankamongmen/notcurses)
- [Home](https://nick-black.com/dankwiki/index.php/Notcurses)


# GNU Screen

```shell
# Reattach a terminal
screen -r
# Attach to a non-detached session (pair-programming)
screen -x

# Share session
screen -d -m -S (session name)
screen -x (session name)
```


## Keyboard shortcuts

| Shortcut | Command       | Description                             |
|-------- |------------- |--------------------------------------- |
| C-a ”    | windowlist -b | list all windows                        |
| C-a C-a  | other         | go to previous screen                   |
| C-a C-d  | detach        | detach from screen back to the terminal |
| C-a k    | kill          | kill window                             |
| C-a H    | log           | log the current window                  |
| C-a C    |               | new window                              |


## Scrollback

```
C-a :
scrollback <lines>
C-a [
```


# Minicom


## Attach to a serial console

```shell
# ensure that the dev node has correct permissions
minicom -s
# select "Serial port setup"
# save
```


# Telnet


## Neat things to connect to

- <http://www.livingcomputermuseum.org>
- <http://www.telnet.org/htm/places.htm>
- towel.blinkenlights.nl - ASCII Star Wars
- nyancat.dakko.us - Nyan cat