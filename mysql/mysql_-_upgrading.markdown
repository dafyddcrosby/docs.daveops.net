# MySQL - Upgrading
If using InnoDB:
set global innodb_fast_shutdown=0
"With a slow shutdown, InnoDB performs a full purge and change buffer merge before shutting down,
which ensures that data files are fully prepared in case of file format differences between releases."

Run mysql_upgrade
"mysql_upgrade should not be used when the server is running with --gtid- mode=ON. See GTID mode and mysql_upgrade for more information."



