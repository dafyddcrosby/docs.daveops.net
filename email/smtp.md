# SMTP

Connect via command line
------------------------

```bash
# Connect via telnet
telnet example.com 25
# Encrypted connection
openssl s_client -connect example.com:587 -starttls smtp
```

```
HELO example.com
MAIL FROM: from@example.com
RCPT TO: to@example.com
DATA
Blah blah
.
<CR-LF>
<CR-LF>
```

Ports
-----


* 25: SMTP, typically used for MTA to MTA
* 465: SMTPS
* 587


