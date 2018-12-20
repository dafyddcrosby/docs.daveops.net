# AWS - CLI
@AWS

ELB certs
---------

``# Upload a new cert``
`` aws iam upload-server-certificate --server-certificate-name my-server-certificate ``
`` --certificate-body file://my-public-key-file.pem --private-key file://my-private-key-file.pem ``
`` --certificate-chain file://my-certificate-chain-file.pem``

Links
-----

* [EC2 instances tables](https://www.ec2instances.info/)


