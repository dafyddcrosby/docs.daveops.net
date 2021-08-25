---
title: Restic
---

```bash
# Initialize the repo
restic init -r $REPO_PATH

# Run a backup
restic backup -r $REPO_PATH $PATH_TO_BACKUP

# List snapshots
restic snapshots -r $REPO_PATH
```

## Links

* https://restic.readthedocs.io/en/stable/index.html
