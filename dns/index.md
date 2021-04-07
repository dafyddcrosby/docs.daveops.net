---
title: DNS
---

## Using dig

```bash
# Reverse lookup
dig -x [ip addr]

# get root servers
dig NS com

# Get nameserver glue records
dig NS example.com @b.gtld-servers.net

# Get SOA (serial, refresh, retry, expiry, minimum)
dig +short example.com soa
```

## Query name server for IP addresses

``nslookup [name] [dns server]``


## Add Route53 subdomain to zone file

```bind
; drop this in the example.com zone file
$ORIGIN subdomain.example.com.
@ IN NS ns-x.awsdns-x.net.
@ IN NS ns-x.awsdns-x.com.
@ IN NS ns-x.awsdns-x.co.uk.
@ IN NS ns-x.awsdns-x.org.
```

## Protect domain that doesn't use email

* an SPF record that says you do not have any sending servers
  * TXT record, @ : "v=spf1 -all"
* a DMARC record to reject any email from your domain
  * TXT record, _dmarc : "v=DMARC1;p=reject;sp=reject;adkim=s;aspf=s;fo=1;rua=mailto:stub@example.org"
* an empty DKIM key record
  * TXT record, *._domainkey : "v=DKIM1; p="
* (optional) null MX record
  * priority 0

[Source](https://www.gov.uk/guidance/protect-domains-that-dont-send-email)

## Links

* [Kaminsky DNS Vulnerability](http://www.unixwiz.net/techtips/iguide-kaminsky-dns-vuln.html)
* [Cricket Liu's DNS Advisor](http://ww2.infoblox.com/services/dns_advisor_tool.cfm)
* [Criticism of DNSSEC](https://sockpuppet.org/blog/2015/01/15/against-dnssec/)
