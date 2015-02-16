YUM
===

Create a group
--------------
::

  yum-groups-manager -n "My Group" --id=mygroup --save=mygroups.xml --mandatory yum glibc rpm
  createrepo -g /path/to/mygroups.xml /srv/my/repo
