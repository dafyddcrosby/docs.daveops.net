# NFS


# NFS

## Install NFS

	# Install nfs-utils (Red Hat) or nfs-common (Ubuntu)
	modprobe nfs
	mkdir /repo
	mount -t nfs4 -o proto=tcp,port=2049 host:/repo /repo

## Show exports

 showmount -e [host]

