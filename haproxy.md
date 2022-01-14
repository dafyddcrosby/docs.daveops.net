# HAProxy

## Setting ciphers on an SSL listener

	bind :443 ssl no-sslv3 crt /path/to/cert.pem ciphers ALL:!EXPORT:!aNULL:!eNULL:!RC4:!MD5:!DES:!3DES:!MEDIUM:!WEAK


## Links

* [HAProxy website](http://www.haproxy.org/)
* [Docker image](https://hub.docker.com/_/haproxy)
