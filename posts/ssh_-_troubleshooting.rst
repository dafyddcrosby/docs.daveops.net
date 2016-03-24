SSH - Troubleshooting
=====================
:date: 2016-03-24

As silly as some of these seem, I've seen them all apply at one point or another.

Client-side
-----------

- Are you using the correct key?
  - confirm this using the -i flag
- Have you ordered your flags correctly?
  - confirm this by looking at the man page - order is important!
- Is your username correct?
- Have you confirmed you're trying to connect to the right box?
  - is the DNS record connect? is your SSH client correctly resolving to that address?
  - do you need to have a VPN turned on?
  - is your routing correct?
  - does the fingerprint match?

Server-side
-----------

- Is sshd running?
- Is the firewall open?
- Are the logs showing any connections?
