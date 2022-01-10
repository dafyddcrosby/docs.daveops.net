---
title: Terraform
---

## Syntax

```hcl
# This is a comment
variable "ami" {
  description = "the AMI to use"
}
```

## CLI

```bash
# taint/untaint a resource
terraform taint RESOURCE
terraform untaint RESOURCE
```

## Links

* [Source Code](https://github.com/hashicorp/terraform)
* [Project documentation](https://www.terraform.io/docs/)
* [TFsec - static scanner](https://www.tfsec.dev/)

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


---
title: Terraform Custom Providers
---

https://www.terraform.io/docs/extend/writing-custom-providers.html

