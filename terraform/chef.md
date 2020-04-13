---
title: Terraform - chef
---

## Using chef-vault

```hcl
provisioner "chef" {

  # add instance to chef-vault
  vault_json = <<EOF
{
  "databag": [
    "item1",
    "item2"
  ]
}
EOF
}
```
