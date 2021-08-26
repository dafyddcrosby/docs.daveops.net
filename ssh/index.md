---
title: SSH
---

## Generating a new key

```bash
# Generate a new RSA keypair
ssh-keygen -t rsa -b 4096 -C "<username> generated <date>" -f id_rsa
# Generate a new ed25519 keypair
ssh-keygen -t ed25519 -C "<username> generated <date>"
```

## Getting key fingerprint

```bash
ssh-keygen -lf .ssh/id_rsa.pub
```

## Security notes

### General


* Use protocol 2


### Client

* If using ssh-add (ie the ssh-agent), also use ``-c`` and ``-t <seconds>`` arguments, and use an askpass program to confirm connections
* Use HashKnownHosts to obscure which hosts you connect to. To retroactively do this on an existing file ``ssh-keygen -H``

### Server

* Use something like fail2ban, and rate limit incoming connections

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
