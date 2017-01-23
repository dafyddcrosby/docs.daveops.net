GPG
===
:date: 2015-12-07
:modified: 2017-01-23

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

Decrypt a file
--------------
::

  gpg --output <file> --decrypt <file>.gpg

Show key fingerprints
---------------------
::

 gpg --list-keys --fingerprint
 # Show 32-bit fingerprints
 gpg --list-keys --fingerprint --keyid-format=short


Verify signature of document
----------------------------
::

 # Compressed, signed doc
 gpg --output doc --decrypt doc.sig
 # Detached signature
 gpg --verify doc.sig doc

Links
-----

`GNU Privacy Handbook <https://www.gnupg.org/gph/en/manual.html>`_
