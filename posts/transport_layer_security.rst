Transport Layer Security
========================
:date: 2016-5-9

Stop using
----------

- SSL2 (DROWN attack)
- SSL3 (POODLE attack)
- RC4 (see: RFC 7465 and https://blog.cloudflare.com/killing-rc4-the-long-goodbye/ )
- MD5 (collisions are trivial)
- Export ciphers (deliberately weakened)
- DES

Links
-----
https://blog.qualys.com/ssllabs/2015/06/08/introducing-tls-maturity-model
