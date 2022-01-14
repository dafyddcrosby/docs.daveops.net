# iptables

## Set up default DROP rule for eth0

```bash
iptables -P INPUT DROP
```

## Allow existing connections to continue

```bash
iptables -A INPUT -i eth0 -m state --state ESTABLISHED,RELATED -j ACCEPT
```

## Accept everything from the 192.168.1.x network

```bash
iptables -A INPUT -i eth0 -s 192.168.1.0/24 -j ACCEPT
```

## Drop a single host

```bash
iptables -I INPUT -s 192.168.1.100 -j DROP
```

## Allow connections from this host to 192.168.2.10

```bash
iptables -A OUTPUT -o eth0 -d 192.168.2.10 -j ACCEPT
```

## Flush table
```bash
iptables --flush
```
## Allow incoming HTTP

```bash
iptables -A INPUT -i eth0 -p tcp --dport 80 -m state --state NEW,ESTABLISHED -j ACCEPT
iptables -A OUTPUT -o eth0 -p tcp --sport 80 -m state --state ESTABLISHED -j ACCEPT
```

## Limit connections

```bash
iptables -A INPUT -p tcp --dport 80 -m limit --limit 25/minute --limit-burst 100 -j ACCEPT
```

## Simple IP masquerading
```bash
echo "1" > /proc/sys/net/ipv4/ip_forward
iptables -t nat -A POSTROUTING -o $EXT_IFACE -j MASQUERADE
```
## Backup, reload iptables rules
```bash
iptables-save > ./file
iptables-restore < ./file
```

## File location

- /etc/sysconfig/iptables

