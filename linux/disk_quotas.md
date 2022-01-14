# Disk quotas

Add usrquota/grpquota to the mount options of the drive

```bash
# Create quota database
quotacheck -cugm /

# Turn quota on for all disks
quotaon -a

# Get quota usage for a user
quota USER

# Create a report of user quota usage
repquota /

# Change a quota for a user
edquota USER
# Copy a quota setting to another user
edquota -p USER1 USER2
# Change grace period
edquota -t
# Set disk quotas
setquota USER block-soft block-hard inode-soft inode-hard mount-point
```
