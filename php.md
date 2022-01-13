# PHP

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

## Authenticating users


	
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


## Validation


### Numeric validation

	
	 <?php
	 is_int($var)
	 is_numeric($var)
	 ?>

# Export to CSV

	
	 <?php
	 header("Expires: 0");
	 header("Cache-control: private");
	 header("Cache-Control: must-revalidate, post-check=0, pre-check=0");
	 header("Content-Description: File Transfer");
	 header("Content-Type: text/csv");
	 header("Content-disposition: attachment; filename=rawlogs.csv");
	 ?>

# Get filename

	
	 <?php
	 $FILE_NAME = basename($_SERVER["PHP_SELF"]);
	 ?>

# Return multiple values

	
	 <?php
	 function mult() {
		 return array(0, 1);
	 }
	 
	 list ($zero, $one) = mult();
	 ?>

# Create an array of objects

	
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
	

# Include PEAR (Dreamhost)

	
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

# Convert command line arguments into GET variables

	
	 <?php
	 parse_str(implode('&amp;', array_slice($argv, 1)), $_GET);
	 ?>

# Get PHP config info

```bash
# Get configuration (like phpinfo())
php -i
# Get location of php.ini
php --ini
```

# Redirect to a different URL

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



# composer
[Composer homepage](https://getcomposer.org/)

## Require a package

```bash
composer require repo/package
```

Will drop `composer.json` and `composer.lock`

## Load vendor directory

```php
<?php
require __DIR__ . '/vendor/autoload.php';
```


# Insecurity
PHP has more than a few security pitfalls, this is just a quick list of ways it
can bite you.

## $FILES tmp_name usage

tmp_name is untrusted input and should be sanitized before doing any file
operations (like `move_uploaded_file`)

## exif_imagetype is not validation

If you're testing a file to ensure it's an image, exif_imagetype alone is
inadequate, as it can be easily bypassed with a magic string like "BM" (for
bitmap)


# PDO

```php
// simple query
<?php
$sql = "SELECT max(id) FROM table";
$sth = $DB->prepare($sql);
$sth->execute();
$max_id= $sth->fetchColumn();
?>
```	



# Versions
## 8.1

- Enums
- Read-only properties
- First-class Callable Syntax
- Pure Intersection Types
- "Never" return type (ie void function)
- Final class constants
- Explicit Octal numeral notation
- Fibers
- Array unpacking support for string-keyed arrays
- Sodium XChaCha20 functions

- <https://www.php.net/releases/8.1/en.php>


# Binary

## Read a binary file
```php
<?php
$handle = fopen('/path/to/file', 'rb');
$contents = fread($handle, filesize($filename));
fclose($handle);
?>
```	
## Binary craziness

	
	 <?php
	 bindec();
	 decbin();
	 ?>
## Playing with endianness
	
	 <?php
	 // For changing endianness, just use strrev()
	 // %016b for 16-bit, %032b for 32-bit
	 strrev(sprintf("%016b", $int));
	 ?>
## Unpacking binary file formats
	 <?php
	 // For parsing binary file formats,
	 // use pack/unpack
	 function get_gif_header($image_file)
	 {
	  
		 /* Open the image file in binary mode */
		 if(!$fp = fopen ($image_file, 'rb')) return 0;
	  
		 /* Read 20 bytes from the top of the file */
		 if(!$data = fread ($fp, 20)) return 0;
	  
		 /* Create a format specifier */
		 $header_format = 
				 'A6Version/' . # Get the first 6 bytes
				 'C2Width/' .   # Get the next 2 bytes
				 'C2Height/' .  # Get the next 2 bytes
				 'C1Flag/' .    # Get the next 1 byte
				 '@11/' .       # Jump to the 12th byte
				 'C1Aspect';    # Get the next 1 byte
	 
		 /* Unpack the header data */
		 $header = unpack ($header_format, $data);
	  
		 $ver = $header['Version'];
	  
		 if($ver == 'GIF87a' || $ver == 'GIF89a') {
			 return $header;
		 } else {
			 return 0;
		 }
	 }
	  
	 /* Run our example */
	 print_r(get_gif_header("aboutus.gif"));
	 
	 /*
	 Array
	 (
		 [Version] => GIF89a
		 [Width1] => 97
		 [Width2] => 0
		 [Height1] => 33
		 [Height2] => 0
		 [Flag] => 247
		 [Aspect] => 0
	 )
	 */
	 ?>



# Barcodes
## Get a UPC check digit

```php	
<?php
function get_ean_checkdigit($barcode){
        $sum = 0;
        for($i=(strlen($barcode));$i>0;$i--){
       	 $sum += (($i % 2) * 2 + 1 ) * substr($barcode,$i-1,1);
        }
        return (10 - ($sum % 10));
}
?>
```

## Create a barcode with Image_Barcode2 (PEAR)

```php	
<?php
require_once 'Image/Barcode2.php';
$bc = new Image_Barcode2;
$bc->draw($_GET['bctext'], "int25", "png");
?>
```



## SQLite


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

