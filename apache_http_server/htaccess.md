# htaccess
mod_rewrite
-----------

### Allow password protected directories without WordPress 404

	RewriteCond %{REQUEST_URI} ^/(failed_auth\.html).*$ [NC]
	RewriteRule . - [L]

### Do a 301 redirect

	RewriteCond %{REQUEST_FILENAME} /tascam688
	RewriteCond %{REQUEST_FILENAME} /tascam688/(.*)
	RewriteRule (.*) http://www.lonesomecosmonaut.com/2009/tascam-688/ [R=301,L]

### Deny hotlinking of images

	RewriteCond %{HTTP_REFERER} !^$
	RewriteCond %{HTTP_REFERER} !^http://(www\.)?lonesomecosmonaut.com/.*$ [NC]
	RewriteRule .(gif|jpg|bmp)$ - [F]

mod_alias
---------


### Put website down for maintenance

	RedirectMatch 302 ^/ /outoforder.html

Add audio handling (HTML 5)
---------------------------

	AddType audio/ogg oga ogg
	AddType audio/mp3 mp3

Run Python cgi scripts
----------------------

(note - scripts should be 755)

	Options +ExecCGI
	AddHandler cgi-script .py

Do not allow access to .htaccess file
-------------------------------------

	<Files .htaccess>
	order allow,deny
	deny from all
	</Files>

Prevent directory indexing
--------------------------

	Options -Indexes

Adding user authentication
--------------------------

	<Limit GET PUT POST>
	AuthName "Please enter credentials"
	AuthType Basic
	AuthUserFile /path/personal-htpasswd
	Require valid-user
	</Limit>

To create an htpasswd file
--------------------------

	htpasswd -c .htpasswd username

