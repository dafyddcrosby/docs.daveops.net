SSH
---


Getting key fingerprint
==============================
{{{ssh-keygen -lf .ssh/id_rsa.pub}}}
Set time on machine that doesn't have NTP
=========================================
{{{date --set="$(ssh user@server date)"}}}

