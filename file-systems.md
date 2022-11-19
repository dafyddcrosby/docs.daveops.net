# File Systems

Filesystems here are either networked or OS-agnostic


# ZFS


## Compression

caveat - compression is *not* retroactive, so you should set it on the pool immediately after creation

```shell
# See compression settings
zfs get compression
# See compression efficiency
zfs get compressratio
# Set compression (types: lzjb, lz4, gzip-[1-9])
zfs set compression=$TYPE $POOL
```


## Delegate administrative tasks

<https://blogs.oracle.com/marks/entry/zfs_delegated_administration>

```shell
zfs allow
```


## Snapshots

```shell
# List snapshots
zfs list -t snapshot
# Delete a snapshot
zfs destroy tank/home/thing@tuesday
```


## Rollback

By default, ZFS rollback cannot revert to anything beyond the most recent snapshot. -r lets you go earlier, although this is a destructive operation and deletes the intermediate snapshots

```shell
zfs rollback tanks/home/dave@tuesday
```


## See disks

```shell
zpool list
```


## References

- <http://www.solarisinternals.com/wiki/index.php/ZFS_Evil_Tuning_Guide>
- <http://jrs-s.net/2015/02/03/will-zfs-and-non-ecc-ram-kill-your-data/>


# NFS


## Install NFS

```shell
# Install nfs-utils (Red Hat) or nfs-common (Ubuntu)
modprobe nfs
mkdir /repo
mount -t nfs4 -o proto=tcp,port=2049 host:/repo /repo
```


## Show exports

```shell
showmount -e $HOST
```


# Samba


## Remotely shut down Windows machine

```
net rpc shutdown -I ipAddressOfWindowsPC -U username%password
```


# ncdu


## Only count files on same mounted filesystem

```shell
ncdu -x $MOUNT_POINT
```


# IPFS

- <https://ipfs.io>

```shell
# Initialize an IPFS system
ipfs init
```