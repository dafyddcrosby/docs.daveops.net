---
title: AWS
---

# AWS

# ELB certs
```bash
# Upload a new cert
aws iam upload-server-certificate --server-certificate-name my-server-certificate  --certificate-body file://my-public-key-file.pem --private-key file://my-private-key-file.pem --certificate-chain file://my-certificate-chain-file.pem
```

- [EC2 instances tables](https://www.ec2instances.info/)




# IAM
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

## Cloudsplaining

https://github.com/salesforce/cloudsplaining

```bash
cloudsplaining download
cloudsplaining scan --input-file default.json --exclusions-file exclusions.yml
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
* [ARNs and namespaces](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html)
* [IAM policy 'Version'](https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_version.html)


# EC2
## Get instance metadata from within VM

<https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html>

```bash
curl http://169.254.169.254/

# Get list of IAM roles
curl http://169.254.169.254/latest/meta-data/iam/security-credentials/

# Get role credentials
curl http://169.254.169.254/latest/meta-data/iam/security-credentials/ROLE_NAME
# You'll likely want AccessKeyID, SecretAccessKey, and Token
```

## Hypervisors

<http://www.brendangregg.com/blog/2017-11-29/aws-ec2-virtualization-2017.html>

### Nitro
[Nitro virtualization (YT)](https://www.youtube.com/watch?v=LabltEXk0VQ)

### Xen

## IPv6

<https://aws.amazon.com/blogs/aws/new-ipv6-support-for-ec2-instances-in-virtual-private-clouds/>



# EC2 AMIs
## Get CentOS AMIs

```bash
aws --region us-east-1 ec2 describe-images --owners aws-marketplace --filters Name=product-code,Values=aw0evgkw8e5c1q413zgy5pjce
```

https://cloud.centos.org/centos/7/images/
https://wiki.centos.org/Cloud/AWS

## Creating AMIs

* [AMI tools reference](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-tools-commands.html)
* <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/create-instance-store-ami.html>
* <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_ConvertingS3toEBS.html>

### VM Import

```bash
# Import an image
aws ec2 import-image --description "example-ami" --disk-containers file://ami_containers.json

# See progress of image imports
aws ec2 describe-import-image-tasks
```

https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-troubleshooting.html
https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html


# S3
```bash
# Get object ACL
aws s3api get-bucket-acl --bucket BUCKET --key KEY

# Give bucket owner full permissions on the file
aws s3api put-bucket-acl --bucket BUCKET --key KEY --acl bucket-owner-full-control
```


# s3cmd
## set new ACL

```bash
s3cmd setacl s3://BUCKET/OBJECT --acl-grant=[read|write|read_acp|write_acp|full_control|all]:USER_CANONICAL_ID [--recursive]
```


# Scout2
```bash
python Scout2.py --region REGION --force --no-browser
```


# Secrets Manager

```bash
# Get secret
aws secretsmanager get-secret-value --secret-id NAME_or_ARN --version-stage AWSCURRENT
# Get resource policy
aws secretsmanager get-resource-policy --secret-id NAME_or_ARN
```


# Transit Gateways
- [Best Practices](https://docs.aws.amazon.com/vpc/latest/tgw/tgw-best-design-practices.html)
