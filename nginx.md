# nginx
@HTTP

Rotate logs
-----------

	mv access.log access.log.0
	kill -USR1 `cat master.nginx.pid`
	sleep 1
	gzip access.log.0    # do something with access.log.0

Basic TLS config
----------------

Check for recent config at <https://mozilla.github.io/server-side-tls/ssl-config-generator/>

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
	  
	      ....
	}

Redirect only / query
---------------------

	location = / {
	      # this matches only the / query.
	}

Disable nginx version in header
-------------------------------
In http, server, or location

	server_tokens off

Resources
---------


* <https://nginx.org/en/docs/http/ngx_http_rewrite_module.html#return>
* <https://nginx.org/en/docs/http/ngx_http_core_module.html#location>


A funny aside
-------------

One of my coworkers said that "nginx is just a hipster Apache". I *wish* that's the most ignorant thing he's ever said (sit down with me for a beer, and I'll give a few other gems).


* [The C10K Problem](http://www.kegel.com/c10k.html)


