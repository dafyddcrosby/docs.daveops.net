GPG
===

Generate GPG keypair
--------------------
::

 gpg --gen-key

Export the public key
---------------------
::

 gpg --armor --export user@example.com > user.gpg.pub

Encrypt a file
--------------
::

 gpg --encrypt --recipient user@example.com --output <file>.gpg <file>

Show key fingerprints
---------------------
::

 gpg --list-keys --fingerprint

Links
-----

`GNU Privacy Handbook <https://www.gnupg.org/gph/en/manual.html>`_
