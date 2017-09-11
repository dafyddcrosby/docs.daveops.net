webERP
======

Site: http://www.weberp.org

Requires PHP multibyte extension

Tables:
stockserialitems (shows the lots)
locstock (stock levels at location)

Adding new script
-----------------
MySQL:

.. code-block:: sql

 -- add script permissions
 insert into scripts values ('ZeroLots.php', 10, '');
