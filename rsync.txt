Rsync
=====
:date: 2016-04-04

Use sudo on remote end
----------------------
::

  rsync -a -e "ssh" --rsync-path="/usr/bin/sudo /usr/bin/rsync" user@example.com:/remote_files/ local_dir/ 
