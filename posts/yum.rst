YUM
===
:date: 2016-02-08

Create a group
--------------
::

  yum-groups-manager -n "My Group" --id=mygroup --save=mygroups.xml --mandatory yum glibc rpm
  createrepo -g /path/to/mygroups.xml /srv/my/repo

versionlock
-----------
RPM is either yum-versionlock or yum-plugin-versionlock

::

  # add lock at current version 
  yum versionlock add PACKAGE
  # list locked version
  yum versionlock list
  # delete an entry
  yum versionlock delete PACKAGE
  # blow away entire versionlock file
  yum versionlock clear
