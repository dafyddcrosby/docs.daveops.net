# Mount
@Linux

Mounting an ISO
---------------

::

 mount -o loop disk1.iso /mnt/disk

Create a RAM disk
-----------------

::

 mount -t tmpfs tmpfs /mnt -o size=1024m

Lazy unmount of a partition
---------------------------

Linux 2.4.11+

::

  umount -l <mount>

