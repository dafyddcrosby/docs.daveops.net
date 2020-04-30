---
title: IAM
tags: ["AWS"]
---

## Create a user

```bash
aws iam create-user --user-name <user>
```

## Create an access key

```bash
aws iam create-access-key --user-name <user>
```

## Attaching IAM roles to an instance

```bash
aws ec2 associate-iam-instance-profile --instance-id YourInstanceId --iam-instance-profile Name=YourNewRole-Instance-Profile
```

## Signing Certificates

```bash
aws iam upload-signing-certificate --user-name user-name --certificate-body file://path/to/certificate.pem
aws iam list-signing-certificates --user-name user-name
```

## Server certificates

```bash
# get cert names
aws iam list-server-certificates
# get cert details
aws iam get-server-certificate --server-certificate-name NAME
# upload cert
aws iam upload-server-certificate --server-certificate-name NAME --certificate-body file://public_key_cert_file.pem --private-key file://my_private_key.pem --certificate-chain file://my_certificate_chain_file.pem
# delete a cert
aws iam delete-server-certificate --server-certificate-name NAME
```

## Resource-level permissions

Some permissions can't be narrowed to something like a tag. This is an easy
trip-up when you're dealing policy creation, as they'll need to be slightly
different statements.

* [Supported resource-level
  permissions](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-iam-actions-resources.html)
* [Unsupported resource-level
  permissions](https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ec2-api-permissions.html#ec2-api-unsupported-resource-permissions)

## Links

* [Complete IAM Reference](https://iam.cloudonaut.io/)
* [AWS IAM docs](https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html)
* <https://www1.amazonian.io/post/list-iam-actions/>
* [ARNs and namespaces](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html)
* [IAM policy 'Version'](https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_version.html)
