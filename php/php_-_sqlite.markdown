# PHP - SQLite
@php


SQLite
------


### Initializing

	
	 <?php
	 /**
	  * Simple example of extending the SQLite3 class and changing the __construct
	  * parameters, then using the open method to initialize the DB.
	  */
	 class MyDB extends SQLite3
	 {
		 function __construct()
		 {
			 $this->open('mysqlitedb.db');
		 }
	 }
	 
	 $db = new MyDB();
	 
	 $db->exec('CREATE TABLE foo (bar STRING)');
	 $db->exec("INSERT INTO foo (bar) VALUES ('This is a test')");
	 
	 $result = $db->query('SELECT bar FROM foo');
	 var_dump($result->fetchArray());
	 ?>

### Reading values

	
	 <?php
	 //read data from database
	 $query = "SELECT * FROM Movies";
	 if($result = $database->query($query, SQLITE_BOTH, $error))
	 {
	   while($row = $result->fetch())
	   {
		 print("Title: {$row['Title']} <br />" .
			   "Director: {$row['Director']} <br />".
			   "Year: {$row['Year']} <br /><br />");
	   }
	 }
	 else
	 {
	   die($error);
	 }
	 ?>

