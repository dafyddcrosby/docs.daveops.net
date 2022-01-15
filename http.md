# HTTP

# Telnet access

```
GET / HTTP/1.1
host: daveops.net
<line feed>
```

# HTTP response codes

- 1xx Informational
- 2xx Success
- 3xx Redirection
- 4xx Client Error
- 5xx Server Error

code | status
---  | ---
100  | Continue
101  | Switching Protocols
102  | Processing (WebDAV; RFC 2518)
200  | OK
201  | Created
202  | Accepted
203  | ~Non-Authoritative Information (since HTTP/1.1)
204  | No Content
205  | Reset Content
206  | Partial Content
207  | ~Multi-Status (WebDAV; RFC 4918)
208  | Already Reported (WebDAV; RFC 5842)
226  | IM Used (RFC 3229)
300  | Multiple Choices
301  | Moved Permanently
302  | Found
303  | See Other (since HTTP/1.1)
304  | Not Modified
305  | Use Proxy (since HTTP/1.1)
306  | Switch Proxy
307  | Temporary Redirect (since HTTP/1.1)
308  | Permanent Redirect
400  | Bad Request
401  | Unauthorized
402  | Payment Required
403  | Forbidden
404  | Not Found
405  | Method Not Allowed
406  | Not Acceptable
407  | Proxy Authentication Required
408  | Request Timeout
409  | Conflict
410  | Gone
411  | Length Required
412  | Precondition Failed
413  | Request Entity Too Large
414  | ~Request-URI Too Long
415  | Unsupported Media Type
416  | Requested Range Not Satisfiable
417  | Expectation Failed
418  | I'm a teapot (RFC 2324)
420  | Enhance Your Calm (Twitter)
422  | Unprocessable Entity (WebDAV; RFC 4918)
423  | Locked (WebDAV; RFC 4918)
424  | Failed Dependency (WebDAV; RFC 4918)
424  | Method Failure (WebDAV)
425  | Unordered Collection (Internet draft)
426  | Upgrade Required (RFC 2817)
428  | Precondition Required (RFC 6585)
429  | Too Many Requests (RFC 6585)
431  | Request Header Fields Too Large (RFC 6585)
444  | No Response (Nginx)
449  | Retry With (Microsoft)
450  | Blocked by Windows Parental Controls (Microsoft)
451  | Unavailable For Legal Reasons (Internet draft)
451  | Redirect (Microsoft)
494  | Request Header Too Large (Nginx)
495  | Cert Error (Nginx)
496  | No Cert (Nginx)
497  | HTTP to HTTPS (Nginx)
499  | Client Closed Request (Nginx)
500  | Internal Server Error
501  | Not Implemented
502  | Bad Gateway
503  | Service Unavailable
504  | Gateway Timeout
505  | HTTP Version Not Supported
506  | Variant Also Negotiates (RFC 2295)
507  | Insufficient Storage (WebDAV; RFC 4918)
508  | Loop Detected (WebDAV; RFC 5842)
509  | Bandwidth Limit Exceeded (Apache bw/limited extension)
510  | Not Extended (RFC 2774)
511  | Network Authentication Required (RFC 6585)
598  | Network read timeout error (MS)
599  | Network connect timeout error (MS)</pre>

# Apache HTTP Server

## Signals

Signal   | Description
---      | ---
SIGHUP   | Rotate logs, kill children, reload configuration
SIGWINCH | Graceful stop
SIGUSR1  | Graceful restart
SIGTERM  | Immediate stop


## Apachectl options

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


## General Hardening

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




## htaccess

### mod_rewrite

#### Allow password protected directories without WordPress 404

```apache
RewriteCond %{REQUEST_URI} ^/(failed_auth\.html).*$ [NC]
RewriteRule . - [L]
```

#### Do a 301 redirect

```apache
RewriteCond %{REQUEST_FILENAME} /tascam688
RewriteCond %{REQUEST_FILENAME} /tascam688/(.*)
RewriteRule (.*) http://www.lonesomecosmonaut.com/2009/tascam-688/ [R=301,L]
```

#### Redirect all traffic to another domain

```
RedirectMatch 301 ^(.*)$ https://example.org
```

#### Deny hotlinking of images

```apache
RewriteCond %{HTTP_REFERER} !^$
RewriteCond %{HTTP_REFERER} !^http://(www\.)?lonesomecosmonaut.com/.*$ [NC]
RewriteRule .(gif|jpg|bmp)$ - [F]
```

### mod_alias

#### Put website down for maintenance

	RedirectMatch 302 ^/ /outoforder.html

### Add audio/video handling (HTML 5)

```apache
AddType audio/ogg oga ogg
AddType audio/mp3 mp3
AddType video/webm webm
```

### Run Python cgi scripts

(note - scripts should be 755)

	Options +ExecCGI
	AddHandler cgi-script .py

### Do not allow access to .htaccess file

```apache
<Files .htaccess>
order allow,deny
deny from all
</Files>
```

### Prevent directory indexing

```apache
Options -Indexes
```

### Adding user authentication

```apache
<Limit GET PUT POST>
AuthName "Please enter credentials"
AuthType Basic
AuthUserFile /path/personal-htpasswd
Require valid-user
</Limit>
```

### To create an htpasswd file

```bash
htpasswd -c .htpasswd username
```

## mod_security

	<IfModule security2_module>
	# Turn on rule engine and set default action
	SecRuleEngine On
	SecDefaultAction "phase:2,deny,log,status:403"
	</IfModule>



# nginx

## Rotate logs

```bash
mv access.log access.log.0
kill -USR1 `cat master.nginx.pid`
sleep 1
gzip access.log.0    # do something with access.log.0
```

## Basic TLS config

Check for recent config at <https://mozilla.github.io/server-side-tls/ssl-config-generator/>

```nginx
server {
      listen 443 ssl;
  
      # certs sent to the client in SERVER HELLO are concatenated in ssl_certificate
      ssl_certificate /path/to/signed_cert_plus_intermediates;
      ssl_certificate_key /path/to/private_key;
      ssl_session_timeout 5m;
      ssl_session_cache shared:SSL:5m;
      ssl_session_tickets off;
      
      # Diffie-Hellman parameter for DHE ciphersuites, recommended 2048 bits
      ssl_dhparam /path/to/dhparam.pem;
  
      # Intermediate configuration. tweak to your needs.
      ssl_protocols TLSv1.1 TLSv1.2;
      ssl_ciphers '<paste intermediate ciphersuite here>';
      ssl_prefer_server_ciphers on;
  
      # Enable this if your want HSTS (recommended)
      # add_header Strict-Transport-Security max-age=15768000;
  
      # OCSP Stapling ---
      # fetch OCSP records from URL in ssl_certificate and cache them
      ssl_stapling on;
      ssl_stapling_verify on;
      ## verify chain of trust of OCSP response using Root CA and Intermediate certs
      ssl_trusted_certificate /path/to/root_CA_cert_plus_intermediates;
      resolver <IP DNS resolver>;
  
      # ...
}
```

## Redirect only / query

```nginx
location = / {
      # this matches only the / query.
}
```

## Disable nginx version in header
In http, server, or location

```nginx
server_tokens off
```

## Resources

* <https://nginx.org/en/docs/http/ngx_http_rewrite_module.html#return>
* <https://nginx.org/en/docs/http/ngx_http_core_module.html#location>

## A funny aside

One of my coworkers said that "nginx is just a hipster Apache". However,
there's sound technical reasons for choosing nginx called [the C10K
Problem](http://www.kegel.com/c10k.html)

## Links

* [Docker image](https://hub.docker.com/_/nginx)


# wget

## Mirror a website

```bash
wget -mk <site>
```


# curl

```bash
# Use client cert
curl --cert client.crt --key client.key URL

# Get the cookie
curl -c JAR ...
# Send the cookie
curl -b JAR ...

# Override referer
curl -e ...

# Send form data with curl
curl -F "key=val" ...

curl -F "uploadedfile=@FILE_TO_UPLOAD;filename=FILENAME_TO_SEND"

# Use POST
curl -X POST -F "url=https://example.org" ..

## Send JSON object
curl -X POST -H "Content-Type: application/json" \
    -d '{"name": "admin", "email": "admin@example.com"}' \
        https://example.org
```
## Get request headers

```bash
curl -i example.com
```

## See if server uses gzip/deflate

```bash
curl -I -H 'Accept-Encoding: gzip,deflate' http://example.com
```

