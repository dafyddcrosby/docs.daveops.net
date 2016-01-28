osslsigncode
============
:date: 2015-12-17

Sign a binary
-------------
::

  osslsigncode -certs signing.cert -key signing.key -readpass passphrase_file -in unsigned_binary.exe -out signed_binary.exe -h sha256
