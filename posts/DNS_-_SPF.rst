DNS - SPF
=========
:date: 2017-01-26

Check the SPF record of a domain
--------------------------------
::

 dig -t TXT example.com +short | grep spf

SPF null record
---------------

If the domain should not be sending any email

::

 www.example.com.   IN  TXT  "v=spf1 -all"

Links
-----

- http://www.openspf.org/
- http://www.spfwizard.net/
