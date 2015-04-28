AWS (command line tool)
=======================
:date: 2015-04-23

ELB certs
--------------------
::

 # Upload a new cert
 aws iam upload-server-certificate --server-certificate-name my-server-certificate 
 --certificate-body file://my-public-key-file.pem --private-key file://my-private-key-file.pem 
 --certificate-chain file://my-certificate-chain-file.pem
