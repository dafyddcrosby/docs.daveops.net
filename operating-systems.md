# Operating Systems
# Plan 9 from Bell Labs

- [Plan 9 Programmer's Manual](http://doc.cat-v.org/plan_9/1st_edition/manual.pdf)

# PRIMOS
OS built in FORTRAN


* <https://en.wikipedia.org/wiki/PRIMOS>
* <ftp://ftp.lysator.liu.se/pub/primos/>
* <http://bitsavers.informatik.uni-stuttgart.de/bits/Prime/>



# Operating systems
<http://wiki.osdev.org>

# Minix
<!-- read the microkernel source -->

## Resources

* <http://www.minix3.org/>


# POSIX

## Standards


* [POSIX 1003.1 2013](http://pubs.opengroup.org/onlinepubs/9699919799/)
* [What could have been IEEE 1003.1e/2c (AKA capabilities)](http://wt.tuxomania.net/publications/posix.1e/download.html)
  * <http://www.trustedbsd.org/privileges.html>

# Fuchsia

https://fuchsia.googlesource.com/fuchsia/+/refs/heads/main
# Android

## Keystore

```bash
# Create a keystore
keytool -genkey -v -keystore /path/to/example-key.keystore -keyalg RSA -keysize 2048 -alias alias_name -validity 10000
# Get expiration dates in the store
keytool -list -v -keystore keystore.jks
```

## Auto-sign release

Add to ant.properties

```
key.store=/path/to/example-key.keystore
key.alias=alias_name
```
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



