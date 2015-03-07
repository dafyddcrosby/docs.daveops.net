PHP - PDO
=========
:tags: php

simple query
------------
.. code-block:: php

 <?php
 $sql = "SELECT max(id) FROM table";
 $sth = $DB->prepare($sql);
 $sth->execute();
 $max_id= $sth->fetchColumn();
 ?>

