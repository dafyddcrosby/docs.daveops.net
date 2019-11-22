---
title: Git commit signing
---

```bash
# sign a commit
git commit -S -m MESSAGE

# specify a default signing key
git config --global user.signingkey KEYID

# configure git to always sign commits
git config --global commit.gpgsign true

# Using X509 (a la smimesign + Git 2.19+)
git config --global gpg.x509.program smimesign
git config --global gpg.format x509
```

## Links
* https://git-scm.com/book/en/v2/Git-Tools-Signing-Your-Work
* https://help.github.com/en/github/authenticating-to-github/signing-commits
