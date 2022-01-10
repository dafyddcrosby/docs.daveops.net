---
title: Apache HTTP Server
---

Signals
-------

Signal   | Description
---      | ---
SIGHUP   | Rotate logs, kill children, reload configuration
SIGWINCH | Graceful stop
SIGUSR1  | Graceful restart
SIGTERM  | Immediate stop


Apachectl options
-----------------

Option        | ShortOption | Description
---           | :---        | ---
start         |             | start daemon
stop          |             | stop daemon
restart       |             | restarts after checking configtest
graceful      |             | graceful restart (checks configtest)
graceful-stop |             | graceful stop of the daemon
fullstatus    |             | display full status report (mod_status and text browser required)
configtest    | -t          | test the configuration
              | -S          | parse the config, show what IPs are used for virtual hosts


General Hardening
-----------------

### SSL Config

```apache
SSLProtocol          All -SSLv2 -SSLv3
SSLHonorCipherOrder  on
SSLCompression       off
# HSTS (mod_headers is required) (15768000 seconds = 6 months)
Header always set Strict-Transport-Security "max-age=15768000"
```

### General Apache Config

```apache
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
```

## Links

* [Apache docs](https://httpd.apache.org/docs/)
* [Mozilla SSL Configuration Generator](https://mozilla.github.io/server-side-tls/ssl-config-generator)
* <https://httpd.apache.org/docs/current/misc/security_tips.html>
* [Docker image](https://hub.docker.com/_/httpd)




# htaccess

## mod_rewrite

### Allow password protected directories without WordPress 404

```apache
RewriteCond %{REQUEST_URI} ^/(failed_auth\.html).*$ [NC]
RewriteRule . - [L]
```

### Do a 301 redirect

```apache
RewriteCond %{REQUEST_FILENAME} /tascam688
RewriteCond %{REQUEST_FILENAME} /tascam688/(.*)
RewriteRule (.*) http://www.lonesomecosmonaut.com/2009/tascam-688/ [R=301,L]
```

### Redirect all traffic to another domain

```
RedirectMatch 301 ^(.*)$ https://example.org
```

### Deny hotlinking of images

```apache
RewriteCond %{HTTP_REFERER} !^$
RewriteCond %{HTTP_REFERER} !^http://(www\.)?lonesomecosmonaut.com/.*$ [NC]
RewriteRule .(gif|jpg|bmp)$ - [F]
```

## mod_alias

### Put website down for maintenance

	RedirectMatch 302 ^/ /outoforder.html

## Add audio/video handling (HTML 5)

```apache
AddType audio/ogg oga ogg
AddType audio/mp3 mp3
AddType video/webm webm
```

## Run Python cgi scripts

(note - scripts should be 755)

	Options +ExecCGI
	AddHandler cgi-script .py

## Do not allow access to .htaccess file

```apache
<Files .htaccess>
order allow,deny
deny from all
</Files>
```

## Prevent directory indexing

```apache
Options -Indexes
```

## Adding user authentication

```apache
<Limit GET PUT POST>
AuthName "Please enter credentials"
AuthType Basic
AuthUserFile /path/personal-htpasswd
Require valid-user
</Limit>
```

## To create an htpasswd file

```bash
htpasswd -c .htpasswd username
```

# mod_security

	<IfModule security2_module>
	# Turn on rule engine and set default action
	SecRuleEngine On
	SecDefaultAction "phase:2,deny,log,status:403"
	</IfModule>

