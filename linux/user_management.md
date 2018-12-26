---
title: User Management
---

# Users

```bash
# change user info
chfn

# Delete user, their home directory, and their mailbox
userdel -r [user]

# Add user, home directory
useradd -m [user]

# Create system user
useradd -r [user]

# See password policies for user
chage -l [user]
```

Only superusers can change ownership of a file

Executable scripts require read and execute bits

## umask

Octal mask to deny permissions by default

Files can't have execution at creation, but directories do. Set your octal mask to deal with the executable

# Groups

* newgrp - logs into a new shell with a new primary group
* chgrp - change the group for files
* groupadd - create a group
* usermod - add users to the group

Sticky bit - t or T in the mode line

