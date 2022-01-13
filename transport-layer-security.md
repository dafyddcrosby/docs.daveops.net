# Transport Layer Security

Here's a quick checklist of best practices as of this writing. It is by no
means complete, but should get you well on your way.

## Stop using


* aNULL non-authenticated DH exchanges (MITM attack)
* eNULL (cleartext)
* SSL2 (DROWN attack)
* SSL3 (POODLE attack)
* RC4 (see: RFC 7465 and <https://blog.cloudflare.com/killing-rc4-the-long-goodbye/>)
* MD5 (collisions are trivial)
* EXPORT ciphers (deliberately weakened)
* DES (deprecated Data Encryption Standard)
* 3DES ([Sweet 32 Attack](https://sweet32.info))
* Weak Diffie-Hellman parameters ([Logjam attack](https://weakdh.org))
* SHA1 ([SHAttered attack](https://shattered.io))
* SSL Compression (CRIME attack)


## Start using


* HTTP Strict Transport Security (RFC 6797)
* OCSP Stapling


## Links


* <https://blog.qualys.com/ssllabs/2015/06/08/introducing-tls-maturity-model>
* [OpenSSL disables TLS 1.0. and 1.1](https://lists.debian.org/debian-devel-announce/2017/08/msg00004.html)
* [TLS 1.3 and Proxies](https://www.imperialviolet.org/2018/03/10/tls13.html)




# Certificate Transparency

<https://www.certificate-transparency.org/>
<https://sites.google.com/site/certificatetransparency/what-is-ct>



# Qualys

* [SSL Server Rating Guide](https://github.com/ssllabs/research/wiki/SSL-Server-Rating-Guide)


