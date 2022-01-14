# EMail

## See also

* [+smtp](./email/smtp.md)


## Videos

* [Email Hatest the Living (Ricardo Signes, rjbs)](https://www.youtube.com/watch?v=4s9IjkMAmns)
## Links

* [jwz: "HTML email, was that your fault?"](https://www.jwz.org/blog/2017/09/html-email-was-that-your-fault/)
* [blacklist checking](https://mxtoolbox.com/SuperTool.aspx)

## Connect via command line

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

## Ports

* 25: SMTP, typically used for MTA to MTA
* 465: SMTPS
* 587





# OfflineIMAP

- [OfflineIMAP Manual](http://docs.offlineimap.org/en/latest/MANUAL.html)
- [Setting up Launchd for OfflineIMAP](http://grantlucas.com/posts/2012/10/setting-launchd-offlineimap)

## Signals

- SIGUSR1 - aborts current sleep, triggering a full sync


# Mutt

[Manual](http://www.mutt.org/doc/manual/)

## Open system mail spool

```bash
mutt -f /var/mail/USERNAME
```

## Add header to messages

```
my_hdr X-Operating-System: TempleOS
```

## Whitelist TLS fingerprint

```
tls on
tls_fingerprint <fingerprint>
```

## Search

command        | desc
---            | ---
~b EXPR        | Search for messages containing EXPR in the body
~C EXPR        | To: or CC:
~f EXPR        | From:
~d [MIN]-[MAX] | messages with date-sent in date range
~l             | mailing lists
~s             | Subject:

# Postfix

# Retry delivery

```
postqueue -f
```

# Drop queue

```
postsuper -d ALL
```

# Print non-default configuration values

```
postconf -n
```
