---
title: EC2
---

Get instance metadata from within VM
------------------------------------
<https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html>

```bash
curl http://169.254.169.254/

# Get list of IAM roles
curl http://169.254.169.254/latest/meta-data/iam/security-credentials/

# Get role credentials
curl http://169.254.169.254/latest/meta-data/iam/security-credentials/ROLE_NAME
# You'll likely want AccessKeyID, SecretAccessKey, and Token
```

Hypervisors
-----------
<http://www.brendangregg.com/blog/2017-11-29/aws-ec2-virtualization-2017.html>

### Nitro
[Nitro virtualization (YT)](https://www.youtube.com/watch?v=LabltEXk0VQ)

### Xen

IPv6
----
<https://aws.amazon.com/blogs/aws/new-ipv6-support-for-ec2-instances-in-virtual-private-clouds/>

