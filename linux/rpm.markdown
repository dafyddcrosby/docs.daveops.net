# RPM
@Red_Hat

[List of RPM macros](http://www.zarb.org/~jasonc/macros.php)

Rebuild SRPM
------------

	# Centos 6
	rpmbuild --rebuild <SRPM>
	# also
	rpm -i <SRPM>
	rpmbuild -ba rpmbuild/SPECS/<spec file>


List files in an RPM
--------------------
	rpm -qlp [file].rpm


Find packages that depend on a particular package
-------------------------------------------------

	rpm -q --whatrequires ${PACKAGE}

Get grepable info from RPM
--------------------------

Use ``rpm --querytags`` to get a list of tags

	rpm -q --queryformat="%{NAME}: %{LICENSE}\n" package_name


Verify package integrity
------------------------



 rpm -V <package>

Show package dependencies
-------------------------



 rpm -qpR <.rpm file>
 rpm -qR <package name>

Extract RPM contents
--------------------



 rpm2cpio php-5.1.4-1.esp1.x86_64.rpm | cpio -idmv

Signing RPM's with GPG
----------------------

In `.rpmmacros`



 %_signature gpg
 %_gpg_name Joe Example <joe@example.org>



 # Replace existing signature
 rpm --resign package1.rpm package2.rpm ...
 # Add additional sig (pre-4.1 ?)
 rpm --resign package1.rpm package2.rpm ...

RPM DB rebuild
--------------



 rm -f /var/lib/rpm/__db*
 rpm --rebuilddb

Install obsolete RPM
--------------------
	rpm -i --nodeps ./RPM

