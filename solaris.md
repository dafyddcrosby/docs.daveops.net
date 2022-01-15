# Solaris
This includes Solaris derivatives as well, such as OpenSolaris and the Illumos project
<http://www.brendangregg.com/blog/2017-09-05/solaris-to-linux-2017.html>

# Solaris

## SMF (Service Management Facility)

## Clear maintenance mode and restart

```bash
svcadm clear <FMRI>
```

## List services

```bash
svcs -a
```

## Logs

/var/svc/log

## Links

* <http://bnsmb.de/solaris/My_Little_SMF_FAQ.html>
* [Ben Rockwood's cheatsheet](http://www.cuddletech.com/blog/pivot/entry.php?id=182)


## Sun OpenBoot

## Boot from CDROM

	boot cdrom

## Test hardware

	test-all

## Show banner
	banner

## Get ethernet address
	.enet-addr

## Show hardware devices
	show-devs




# SmartOS

* vmadm - start, stop, etc virtual machines
* imgadm - find, download, install images




## SmartDataCenter

## Find VM by alias



 sdc-vmapi /vms | json -H -c "this.alias &amp;&amp; this.alias.match(/riak/)"

## Output fwapi rules



 sdc-fwapi /rules

## Update resolvers



 sdc-vmapi  /vms/<uuid>?action=update -d '{ "resolvers": ["8.8.8.8", "8.8.4.4"] }'

# OmniOS
https://omnios.org/



