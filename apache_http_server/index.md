---
title: Apache HTTP Server
---

Signals
-------

| Signal   | Description                                      |
|----------|--------------------------------------------------|
| SIGHUP   | Rotate logs, kill children, reload configuration |
| SIGWINCH | Graceful stop                                    |
| SIGUSR1  | Graceful restart                                 |
| SIGTERM  | Immediate stop                                   |


Apachectl options
-----------------

| Option        | ShortOption | Description                                                       |
|---------------|:------------|-------------------------------------------------------------------|
| start         |             | start daemon                                                      |
| stop          |             | stop daemon                                                       |
| restart       |             | restarts after checking configtest                                |
| graceful      |             | graceful restart (checks configtest)                              |
| graceful-stop |             | graceful stop of the daemon                                       |
| fullstatus    |             | display full status report (mod_status and text browser required) |
| configtest    | -t          | test the configuration                                            |
|               | -S          | parse the config, show what IPs are used for virtual hosts        |


General Hardening
-----------------

### SSL Config

	SSLProtocol          All -SSLv2 -SSLv3
	SSLHonorCipherOrder  on
	SSLCompression       off
	# HSTS (mod_headers is required) (15768000 seconds = 6 months)
	Header always set Strict-Transport-Security "max-age=15768000"


### General Apache Config

	# Deny access to root dir
	<Directory />
	       Options None
	       Order deny,allow
	       Deny from all
	</Directory>
	
	# Disable indexes
	Options -Indexes
	
	# Disable server-side includes and CGI scripts
	Options -Includes
	Options -ExecCGI
	
	# Disable product version
	ServerTokens Prod
	ServerSignature Off
	
	# Disable TRACE
	TraceEnable Off


Misc
----

* [Apache docs](https://httpd.apache.org/docs/)
* [Mozilla SSL Configuration Generator](https://mozilla.github.io/server-side-tls/ssl-config-generator)
* <https://httpd.apache.org/docs/current/misc/security_tips.html>



