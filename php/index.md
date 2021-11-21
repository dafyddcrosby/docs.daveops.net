---
title: PHP
---

## Run a server instance with a script

Requires PHP >= 5.4 - <http://php.net/manual/en/features.commandline.webserver.php>

```bash
php -S localhost:8000 router.php
```

## Datestamps

```php	
<?php
//mktime(hour,minute,second,month,day,year)

echo date("Y/m/d", mktime(0,0,0,11,11,2011));
//2011/11/11
?> 
```

## Autogenerate a file download

```php
<?php
header("Content-Type: text/calendar; charset=utf-8");
header("Content-Disposition: attachment; filename=test.ics");
// render file...
```

## Turn off error reporting

```php	
<?php
error_reporting(0)
?>
```

## Check if domain resolves

```php	
<?php
checkdnsrr($host)
?>
```

Authenticating users
--------------------


	
	 <?php
	 session_start();
	 // Check for session injection
	 if (isset($_REQUEST['_SESSION'])) {
		 session_destroy();
		 die('');
	 } elseif (isset($_SESSION['HTTP_USER_AGENT'])) {
		 if ($_SESSION['HTTP_USER_AGENT'] != md5($_SERVER['HTTP_USER_AGENT'])) {
			 session_destroy();
			 die('');
		 }
	 }
	 // At end of page, let's regenerate the session ID
	 if(isset($_SESSION['username'])) {
		 session_regenerate_id(TRUE);
	 } 
	 ?>


Validation
----------


### Numeric validation

	
	 <?php
	 is_int($var)
	 is_numeric($var)
	 ?>

Export to CSV
=============

	
	 <?php
	 header("Expires: 0");
	 header("Cache-control: private");
	 header("Cache-Control: must-revalidate, post-check=0, pre-check=0");
	 header("Content-Description: File Transfer");
	 header("Content-Type: text/csv");
	 header("Content-disposition: attachment; filename=rawlogs.csv");
	 ?>

Get filename
============

	
	 <?php
	 $FILE_NAME = basename($_SERVER["PHP_SELF"]);
	 ?>

Return multiple values
======================

	
	 <?php
	 function mult() {
		 return array(0, 1);
	 }
	 
	 list ($zero, $one) = mult();
	 ?>

Create an array of objects
==========================

	
	 <?php
	 $allCars=array();
	 $result = mysql_query($SQL);
	 
	 while ($rowInfo = mysql_fetch_assoc($result))
	 { 
	 	 $tempCar=new Car();
	  
	 	 $tempCar->setMake($rowInfo['car_make']);
	 	 $tempCar->setModel($rowInfo['car_model']);
	 	 $tempCar->setColor($rowInfo['car_color']);
	 
	 	 $allCars[]=$tempCar;
	 }
	 ?>
	

Include PEAR (Dreamhost)
========================

	
	 <?php
	 //Include my PEAR path
	 set_include_path("." . PATH_SEPARATOR . ($UserDir = dirname($_SERVER['DOCUMENT_ROOT'])) . "/pear/php" . PATH_SEPARATOR . get_include_path());
	 ?>

## Scrape `$_GET` parameters

If mod_rewrite or some other mechanism is preventing the filling of the `$_GET` array, use this:

```php
<?php
parse_str($_SERVER['QUERY_STRING'], $_GET);
?>
```

Convert command line arguments into GET variables
=================================================

	
	 <?php
	 parse_str(implode('&amp;', array_slice($argv, 1)), $_GET);
	 ?>

Get PHP config info
===================

```bash
# Get configuration (like phpinfo())
php -i
# Get location of php.ini
php --ini
```

Redirect to a different URL
===========================

```php	
<?php
header("Location: http://www.example.com/"); 
?>
```

## Syntax cheatsheet

```php	
<?php
if ($blah == 0) {
  // code
} elseif ($blah == 1) {
  // code
} else {
  // code
}

// null coalescing - ??
// $var = isset($_GET['var']) ? $_GET['var'] : 'default';
$var = $_GET['var'] ?? 'default';

$arr = array(1, 2, 3, 4);
foreach ($arr as $value) {
        print $value;
}

switch ($blah) {
  case 0:
        // code
        break;
  case 1:
        // code
        break;
  default:
        // code
        break;

function foo () {
  return 0;
}
?>
```

