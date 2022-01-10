# Solaris

<http://www.brendangregg.com/blog/2017-09-05/solaris-to-linux-2017.html>



# SMF (Service Management Facility)

Clear maintenance mode and restart
----------------------------------

```bash
svcadm clear <FMRI>
```

List services
-------------

```bash
svcs -a
```

Logs
----

/var/svc/log

Links
-----

* <http://bnsmb.de/solaris/My_Little_SMF_FAQ.html>
* [Ben Rockwood's cheatsheet](http://www.cuddletech.com/blog/pivot/entry.php?id=182)


# Sun OpenBoot
@Sun

Boot from CDROM
---------------

	boot cdrom

Test hardware
-------------

	test-all

Show banner
-----------
	banner

Get ethernet address
--------------------
	.enet-addr

Show hardware devices
---------------------
	show-devs


