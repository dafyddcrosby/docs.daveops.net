# Red Hat
Installing with a kickstart
---------------------------


* Press ESC
* ``boot: linux ks=nfs:192.168.75.132:/srv/nfs/ks.cfg``



Remove older kernels
--------------------

	# Install yum-utils and run:
	package-cleanup --oldkernels --count=1
