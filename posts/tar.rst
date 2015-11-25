===
Tar
===

Clean a tar-bombed directory
==============================
::

  tar -tf <file.tar.gz> | xargs rm -r

Remove VCS junk
==============================
::

  tar --exclude-vcs -cf src.tar src/

Creating a gzipped tarball
==============================
::

  tar czfv tobemade.tar.gz tobezipped/

Extracting
==============================
tar.gz
-----------------------------------
::

  tar zxvf [file]

tar.bz2
-----------------------------------
::

  tar jxvf [file]
