---
title: GPG
---

Generate GPG keypair
--------------------

```bash
gpg --gen-key
```

Export the public key
---------------------

```bash
gpg --armor --export user@example.com > user.gpg.pub
```

Encrypt a file
--------------

```bash
gpg --encrypt --recipient user@example.com --output <file>.gpg <file>
```

Decrypt a file
--------------

```bash
gpg --output <file> --decrypt <file>.gpg
```

Show key fingerprints
---------------------

```bash
gpg --list-keys --fingerprint
# Show 32-bit fingerprints
gpg --list-keys --fingerprint --keyid-format=short
```

Verify signature of document
----------------------------

```bash
# Compressed, signed doc
gpg --output doc --decrypt doc.sig
# Detached signature
gpg --verify doc.sig doc
```

Generate a subkey
-----------------

```bash
gpg --edit-key KEYNAME
```
```
gpg> addkey
```

List private keys
-----------------

```bash
gpg --list-secret-keys
# A # after the letters sec means that the secret key is not usable
```

Deleting keys
-------------

```bash
gpg --delete-key USERNAME
gpg --delete-secret-key USERNAME
```

Links
-----

* [GNU Privacy Handbook](https://www.gnupg.org/gph/en/manual.html)
* [best practices](https://riseup.net/en/security/message-security/openpgp/best-practices)
