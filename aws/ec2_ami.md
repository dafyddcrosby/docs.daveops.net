---
title: EC2 AMIs
---

## Get CentOS AMIs
```bash
aws --region us-east-1 ec2 describe-images --owners aws-marketplace --filters Name=product-code,Values=aw0evgkw8e5c1q413zgy5pjce
```

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
