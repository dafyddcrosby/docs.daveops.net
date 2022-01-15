# Networking
# Border Gateway Protocol

## RPKI
* http://rpki.exposed/
* https://isbgpsafeyet.com/
## Running an AS

### Getting an ASN
* https://www.arin.net/resources/guide/request/#autonomous-system-numbers-asns
* https://www.arin.net/participate/policy/nrpm/#5-as-numbers

### Books
* BGP Design and Implementation (Cisco Press)
* BGP Table Growth

### Links
* [ARIN](https://arin.net)
* [Internet Peering Playbook](http://drpeering.net/tools/HTML_IPP/ipptoc.html)
* https://peeringdb.com
* https://blog.thelifeofkenneth.com/2017/11/creating-autonomous-system-for-fun-and.html
# ICMP

RFC 792

* [Should I Block ICMP?](http://shouldiblockicmp.com/)
# Networking

## traceroute/ping
(as root)


 mtr google.com

## list listening ports


 netstat -plunt

## Alarm when ping is successful


 ping -i 60 -a IP_address

## get external ip


 curl ipecho.net/plain
 curl ifconfig.me

## Bogons

* 0.0.0.0/8
* 10.0.0.0/8
* 100.64.0.0/10
* 127.0.0.0/8
* 169.254.0.0/16
* 172.16.0.0/12
* 192.0.0.0/24
* 192.0.2.0/24
* 192.168.0.0/16
* 198.18.0.0/15
* 198.51.100.0/24
* 203.0.113.0/24
* 224.0.0.0/3

From https://www.team-cymru.org/Services/Bogons/bogon-bn-agg.txt
# IPv4 - subnetting

Natural mask

First bits of address | default mask | Decimal
---                   | ---          | ---
0 x x x               | 8 bits long  | < 128
1 0 x x               | 16 bits long | 128-191
1 1 0 x               | 24 bits long | 192-223
1 1 1 0               | multicast    | 224-239

Greater than 239 the address is reserved

# IPv6

<https://ipv6.he.net/certification>

Link local: FE80::/10
Site local: FEC0::/10 (Deprecated)

Reverse DNS
nibble 4-bit boundaries

# Named Data Networking
<http://named-data.net/project/execsummary/>

# Network Names
Have a formal grammar for parsing

Don't rely on institutional memory - a person with fresh eyes should know where
and what everything is just by reading the records

Don't use vendor type/make/model in DNS name

Use CNAMEs to wean off old names

Pre-derive all current names before committing to a name scheme

## Links


* <https://www.nanog.org/meetings/nanog31/presentations/ringel.pdf>



# Spanning Tree

2 states:
* Forwarding
* Blocking

Note that STP predates LAN switches, hence mention of bridges

Failed/shutdown interfaces are placed into an STP disabled state.

The bridge ID is an 8-byte value, unique to the switch. 2 byte priority field,
and 6-byte for the MAC address.

Root switch is whatever has lowest priority, and in a tie, the lowest bridge ID.

If a switch hears a Hello with a lower BID, it stops advertising and forwards
the superior Hello.

For best root cost tiebreakers,
1. lowest neighbor bridge ID.
2. lowest neighbor port priority.
3. lowest neighbor internal port number.

STP root switch sends a new Hello BPDU every 2 seconds by default. When a switch
hasn't received a Hello (MaxAge is 10xHello, so 20 seconds by default) or gets
different details, it reacts to the topology change. When transitioning from
blocking to forwarding, it goes through Listening state (no forwarded frames,
removes stale MAC table entries), then Learning (no forwarded frames, but learns
MAC addresses of frames sent to interface). Forward delay state changes are 15
seconds each (so 30 seconds from blocking to forwarding). In summary, a topology
change could lead to a 50 second delay using STP.

RSTP (IEEE 802.1w originally, 802.1Q today) is an improvement of STP, where
network convergence can happen in a few seconds (10 seconds in worst case). It
allows switches to replace their root parts without the blocking->forwarding
transition wait time in some cases, the ability to replace a designated port
without waiting for forwarding state, and lower wait times on the timers. MaxAge
for Hello is 3 times the Hello timer. There are also messages that can be sent
to neighboring switches asking if problems are occuring, reducing wait times.
There is a concept of Alternate port (which can replace the root port when
failing), and a Backup port (when the designated port is failing)


## Bridge Protocol Data Units

### Hello BPDU

Has root bridge ID, sender's bridge ID, sender's root cost, and timers on the
root switch

## Spanning Tree Algorithm

Elect a root switch, all ports in forwarding

Non-root switches determine which port has least cost to root switch (root
cost). That "root port" is put in forwarding state.

With two switches on a link, the one with the lowest root cost is placed in a
forwarding state. That switch is the "designated switch," and the interface the
"designated port"

Any leftover interfaces are put in a blocking state.

## Standards

Spanning Tree - IEEE 802.1D
Rapid Spanning Tree - IEEE 802.1w
Multiple Spanning Tree - IEEE 802.1s
all incorporated into 802.1Q-2014
# whois

```bash
# get POC info from ARIN
whois 'p ! + NAME-ARIN'
```
# RADIUS

## RFCs
* 2865
* 2866

## Links
* https://www.cisco.com/c/en/us/support/docs/security-vpn/remote-authentication-dial-user-service-radius/12433-32.html
* https://freeradius.org/documentation/
* http://networkradius.com/doc/FreeRADIUS%20Technical%20Guide.pdf
# Hot Standby Router Protocol

RFC 2281

[No RFC for V2](https://www.cisco.com/c/en/us/td/docs/ios-xml/ios/ipapp_fhrp/configuration/15-mt/fhp-15-mt-book/fhp-hsrp-v2.html)
# SOAP

Simple Object Access Protocol

## Use curl to send a request

```bash
curl -d @request.xml -H "Content-Type: application/soap+xml;charset=UTF-8" http://localhost:9090/thing
```
# HNAP

Home Network Administration Protocol

patent US7827252B2

https://en.wikipedia.org/wiki/Home_Network_Administration_Protocol

https://www.cisco.com/web/partners/downloads/guest/hnap_protocol_whitepaper.pdf

## Get modem details

http://IP_ADDRESS/HNAP1/
# ARP

## Running arpwatch

```bash
arpwatch -i <interface> -u <non-root username>
```

## Use a static ARP table

```bash
# Single address:
arp -s <ip> <mac>

# File:
arp -f <filepath>
```
# DNS

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


# SPF

## Check the SPF record of a domain

	dig -t TXT example.com +short | grep spf

## SPF null record

If the domain should not be sending any email

	www.example.com.   IN  TXT  "v=spf1 -all"

## Links

* <http://www.openspf.org/>
* <http://www.spfwizard.net/>




# BIND

## Production ready

* Rate limiting is set in the options
* Recursion is disabled on the authoritative servers
* Zone transfers are locked down
# iperf3

* https://iperf.fr/
# mtr
## Look for UDP packet loss


 mtr -u <dest>

# nmap

## Handy scripts

Script             | Description
---                | ---
ssl-enum-ciphers   | get list of available SSL/TLS headers
http-trace         | see if server has a TRACE method
http-server-header | get details from the Server: header


## Get list of available server ciphers

	nmap --script ssl-enum-ciphers -p PORT SERVER


## Specifying hosts

	# Input from list
	nmap -iL file ...


## Output

	# Grepable
	nmap -oG file ...
	# XML
	nmap -oX file ...


## Resources

### Source repo
	svn co https://svn.nmap.org/nmap



# scripting engine

<https://nmap.org/book/nse.html>
<https://nmap.org/presentations/BHDC10/>

# rinetd
## Caveats
Doesn't redirect UDP or FTP

## Source
<http://www.boutell.com/rinetd/>

## /etc/rinetd.conf

	bindaddress bindport connectaddress connectport

0.0.0.0 binds to any every available local IP address

# tcpdump

## Get all ICMP packets

```bash
tcpdump icmp
```

## See what's connecting to a port

```bash
tcpdump dst port <PORT>
```

## See what's coming from an IP

```bash
tcpdump src 1.2.3.4
```
# TCP Wrappers
 
```
# blank lines and lines starting with '#' ignored
<daemon list> : <client list> [: <option> : <option> : ...]
```

* /etc/hosts.allow
* /etc/hosts.deny

* <https://access.redhat.com/documentation/en-US/Red_Hat_Enterprise_Linux/6/html/Security_Guide/sect-Security_Guide-TCP_Wrappers_and_xinetd.html>
* <https://jamalahmed.wordpress.com/2010/03/19/using-etchosts-allow-and-etchosts-deny-to-secure-unix/>
* <https://debian-administration.org/article/87/Keeping_SSH_access_secure>

# EST Protocol

RFC 7030

http://testrfc7030.com/
# netstat


# netstat

State       | Description
---         | ---
LISTEN      | accepting connections
ESTABLISHED | connection up and passing data
SYN_SENT    | TCP; session has been requested by us; waiting for reply from remote endpoint
SYN_RECV    | TCP; session has been requested by a remote endpoint for a socket on which we were listening
LAST_ACK    | TCP; our socket is closed; remote endpoint has also shut down; we are waiting for a final acknowledgement
CLOSE_WAIT  | TCP; remote endpoint has shut down; the kernel is waiting for the application to close the socket
TIME_WAIT   | TCP; socket is waiting after closing for any packets left on the network
CLOSING TCP | our socket is shut down; remote endpoint is shut down; not all data has been sent
FIN_WAIT1   | TCP; our socket has closed; we are in the process of tearing down the connection
FIN_WAIT2   | TCP; the connection has been closed; our socket is waiting for the remote endpoint to shut down

# NTP

* [Team Cymru NTP hardening guide](https://team-cymru.com/community-services/templates/secure-ntp-template/)
# OpenVPN

## Setting up an OpenVPN server

```bash
# Set up a cert. authority
cd /etc/openvpn/easy-rsa/
# Edit the vars file
. ./vars
./clean-all
./build-ca
# Create server certs
./build-key-server server
# Create client certs
./build-key client1
# Build Diffie Hellman parameters
./build-dh
```

## Hardening

* Explicitly set the server `cipher`, `tls-cipher` and `auth`
* Use at least 2048-bit RSA keys
* Use `tls-auth` to mitigate DDoS
* Keep CA PKI secure. If the CA key is compromised, you'll need to reissue
* Generate private keys on the target system
* Use strong key passphrases
* Avoid sharing keys across targets
* Generate a CRL at the creation of a VPN
* Use Diffie-Hellman parameters of 2048-bit+
* Set the `script-security` level to what is appropriate
* If you've got servers+clients greater than OpenVPN 2.3.2, set `tls-version-min` to 1.2

## CLI misc

```bash
# See list of supported ciphers
openvpn --show-ciphers
# See list of supported HMACs
openvpn --show-digests
# See list of supported TLS cipher-suites
openvpn --show-tls
```

## Using a static key

```bash
# generate static key
openvpn --genkey --secret static.key
```

In configuration files:

```
secret static.key
# or
<tls-auth>
Key contents
</tls-auth>
```
# Telcos
When connecting to a telco, ask for:

- Serial IP addresses
- circuit ID
- customer #
- line encapsulation
- Ethernet IP address


