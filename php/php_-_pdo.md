# PHP - PDO
@php

simple query
------------

	
	 <?php
	 $sql = "SELECT max(id) FROM table";
	 $sth = $DB->prepare($sql);
	 $sth->execute();
	 $max_id= $sth->fetchColumn();
	 ?>

