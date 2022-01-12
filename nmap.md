---
title: nmap
---

Handy scripts
-------------

Script             | Description
---                | ---
ssl-enum-ciphers   | get list of available SSL/TLS headers
http-trace         | see if server has a TRACE method
http-server-header | get details from the Server: header


Get list of available server ciphers
------------------------------------

	nmap --script ssl-enum-ciphers -p PORT SERVER


Specifying hosts
----------------

	# Input from list
	nmap -iL file ...


Output
------

	# Grepable
	nmap -oG file ...
	# XML
	nmap -oX file ...


Resources
---------

### Source repo
	svn co https://svn.nmap.org/nmap



# scripting engine

<https://nmap.org/book/nse.html>
<https://nmap.org/presentations/BHDC10/>

