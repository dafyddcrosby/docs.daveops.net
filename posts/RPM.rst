RPM
---
:tags: redhat


List files in an RPM
==============================
::

 rpm -qlp [file].rpm

Find packages that depend on a particular package
=================================================
::

 rpm -q --whatrequires ${PACKAGE}

Get grepable info from RPM
==============================
Use {{{rpm --querytags}}} to get a list of tags

::
 
 rpm -q --queryformat="%{NAME}: %{LICENSE}\n" package_name

Verify package integrity
==============================
::

 rpm -V <package>

Extract RPM contents
==============================
::

 rpm2cpio php-5.1.4-1.esp1.x86_64.rpm | cpio -idmv

Signing RPM's with GPG
======================
::

 rpm --resign package1.rpm package2.rpm ...
