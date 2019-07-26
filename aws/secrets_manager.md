---
title: Secrets Manager
---

```bash
# Get secret
aws secretsmanager get-secret-value --secret-id NAME_or_ARN --version-stage AWSCURRENT
# Get resource policy
aws secretsmanager get-resource-policy --secret-id NAME_or_ARN
```
