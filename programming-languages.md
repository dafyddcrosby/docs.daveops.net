# Programming Languages
# Go

## Syntax cheatsheet

```go
package main

import (
	"fmt"
	"os"
	"bufio"
)

func read_file(path string) string {
        f, err := os.Open(filearg)
        if err != nil {
                panic(err)
        }
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}
}

func main() {
        fmt.Println("hello world")
}
```

## Go modules

```bash
go mod init <module_name>
go mod vendor
go get -u
```

## Cross-compilation

```bash
# Compile for AMD64 Linux
GOOS=linux GOARCH=amd64 go build
```

[https://golang.org/doc/install/source#environment](List of compilation targets)

## Static linking

If you're not using CGo, you can statically link with `CGO_ENABLED`.
In Fedora this appears to be [on by default](https://src.fedoraproject.org/rpms/golang//blob/rawhide/f/golang.spec)

```bash
CGO_ENABLED=0 go build
```

## Resources

```bash
godoc -http=:8080
```

- <https://golang.org/>
- [Language Specification](https://golang.org/ref/spec)
- [Go Koans](https://github.com/cdarwin/go-koans)

## Looking into

- <https://www.kablamo.com.au/blog-1/2018/12/10/just-tell-me-how-to-use-go-modules>
- <https://cryptic.io/go-http/>
- <https://medium.com/@nate510/don-t-use-go-s-default-http-client-4804cb19f779>
- [TinyGo](https://tinygo.org/)

## Templating

```
{{/* comment */}}  Defines a comment
{{.}}              Renders root element
{{.Foo}}           Renders the "Foo"-field in a nested element

{{if .Done}} {{else}} {{end}}   Defines an if-statement
{{range .Items}} {{.}} {{end}}  Loops over all “Items” and renders each using {{.}}
{{block "bar" .}} {{end}}       Defines a block with the name "bar"
```

## Go - FastCGI

```go
package main

import (
	"fmt"
	"net/http"
	"net/http/fcgi"
)

func hello(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintln(w, "Hello")
}

func main() {
	http.HandleFunc("/", hello)

	err := fcgi.Serve(nil, nil)
	if err != nil {
		panic(err)
	}
}
```


## Mage

https://magefile.org/
# Java

## Types

type    | note
---     | ---
byte    | 8-bit signed two's complement integer
short   | 16-bit signed two's complement integer
int     | 32-bit signed two's complement integer
long    | 64-bit two's complement integer
float   | single-precision 32-bit IEEE 754 floating point
double  | double-precision 64-bit IEEE 754 floating point
boolean | the size isn't precisely defined

## Numeric literals

You can use underscore characters in SE7+
example: ``long hexBytes = 0xFF_CC_DA_B5;``


## AES intrinsics

Requires Java 8 and Intel 2010+ Westmere

	-XX:+UseAES -XX:+UseAESIntrinsics

## Zero copy

Avoid copying the file data across user/kernel boundary, instead have the kernel put the file in a buffer and use DMA to pass the data directly.

<https://www.ibm.com/developerworks/linux/library/j-zerocopy/>



## SSL

### Debugging

	# print each handshake message
	java -Djavax.net.debug=ssl:handshake MyApp

### Links

* <https://docs.oracle.com/javase/7/docs/technotes/guides/security/jsse/ReadDebug.html>
* <https://docs.oracle.com/javase/7/docs/technotes/guides/security/jsse/JSSERefGuide.html#Debug>




## Maven
<https://maven.apache.org/what-is-maven.html>

# zig

<https://ziglang.org/>

Versions:

- 0.5.0 - async

## CLI

```bash
# Get Clang version
zig cc --version
# Get compilable targets
zig targets
```

# Julia

https://julialang.org/
# Forth

words | action
---   | ---
.     | pop an element off the stack
.s    | display the contents of stack
drop  | drop top of stack
\ ... | comment
see   | decompile

words | action
include <filename> | includes a forth source file

## colon definitions

```forth
: funcname ( stack effect comment )
  do stuff ;

\ local variables
: funcname { a b -- b a }
  b a ;
```
# Kotlin

<http://kotlinlang.org/>

# Prolog
<https://bernardopires.com/2013/10/try-logic-programming-a-gentle-introduction-to-prolog/>

# Tcl


## Tcl

## Syntax cheatsheet

```tcl

 if {blah == 0} {
 # blah
 } elseif {blah == 1} {
 # blah!
 } else {
 # blah?
 }
 
 switch xyz {
 a
-  #drop into next block
 b {
 format 1
 }
 default {
 format 3
 }
 }   # returns 3
 
 set false 0
 set true 1
```

## Namespaces

```tcl
namespace eval blah {}
```

## Profiling statements

```tcl
puts "unbraced: [time { expr 15 * 20 } 1000]"
puts "braced:   [time { expr {15 * 20} } 1000]"
```

## Disassemble statement

This is fun, because it shows just how much of a difference there is in optimizing statements

```tcl
::tcl::unsupported::disassemble script {expr $a eq $b}
::tcl::unsupported::disassemble script {expr {$a eq $b}}
```

## Creating tests

```tcl
package require tcltest
namespace import ::tcltest::*

# Software under test
source sum.tcl

test sum_addTwoZerosExpectZero {
Test: [sum 0 0] == 0
} -body {
sum 0 0
} -result 0

test sum_addTwoPositiveNumbers {} -body {
sum 4 9
} -result 13

test sum_addPositiveToNegative {} -body {
sum -95 72
} -result -23

cleanupTests
```

## OpenACS

### ad_proc documentation metadata

- @return
- @see
- @author
- @param
- @error

### Return a file

```tcl
set file [open $file_path "r"]
ns_set update [ns_conn outputheaders] content-disposition "attachment; filename=$filename"
ns_returnfile 200 [ns_guesstype $file] $file_path
```

### Make your OpenACS form’s checkbox be selected by default
In ad_page_contract:

```
{fries "t"}
```

In ad_form:

```
{fries:text(checkbox),optional
        {label {"Would you like fries with that?"}}
        {html {[if {$fries=="t"} {return "checked checked"}]}}
        {options {{"" t}}}
}
```

### ADP tags

```
<if @datasource.variable@ eq "blue">
      <td bgcolor=#0000ff>
</if>
<elseif @datasource.variable@ eq "red">
      <td bgcolor=red>
</elseif>
<else>
      <td bgcolor=#ffffff>
</else>
```
# Objective C

## Classes

Categories extend classes

### Interface
	/* #import is like #include, but only included once during compilation */
	#import "thing.h"
	
	@interface MyClass : SuperClass <XYZProto, AnotherProtocol> {
	  int integerInstanceVariable;
	}
	+ (void)aClassMethod;
	- (void)anInstanceMethod;
	
	@property NSString *name;
	@property NSNumber *year_a;
	@property int year_b;
	@property (readonly) int readonly_year;
	// you can change the getter method name by changing the attribute
	@property (getter=isFinished) BOOL finished;
	
	@end


### Implementation
	// If you enable modules for iOS >= 7.0 or OS X >= 10.9 projects in
	// Xcode 5 you can import frameworks with @import
	@import Foundation;
	
	#import "myclass.h"
	
	@implementation MyClass {
	  int integerInstanceVariable;
	}
	
	// If you want to change the instance variable name, use @synthesize
	@synthesize year_a = ivar_yeara;
	
	+ (void)aClassMethod {
	  NSLog(@"This is an Objective-C string literal");
	}
	- (void)anInstanceMethod {
	  // Set an instance variable
	  _name = @"Doug E Fresh";
	  // Though it's better to use accessor methods / dot syntax
	  self.name = @"Fresh Prince";
	}
	@end

	MyClass *thing = [[MyClass alloc] init]
	// or, if you're not providing arguments to initialize
	MyClass *thing = [MyClass new]


## Objects

	// setter methods are set + property name
	[person setFirstName:@"Bob"];
	// or use dot syntax
	person.firstName = @"Bob";


## Protocols


	@protocol XYZProto
	// by default methods are required
	- (NSUInteger) numOfThings;
	@optional
	// Everything after @optional is optional, unless there's a @required after that
	- (NSString *) stringThing;
	@end

	NSString *someString;
	if ([self.dataSource respondsToSelector:@selector(stringThing)]) {
	    someString = [self.dataSource stringThing];
	}

	// to inherit a protocol
	@protocol XYZProto <NSObject>
	
	@end


## Collections

### Arrays
	// Trailing nil needed
	NSArray *someArray = [NSArray arrayWithObjects:firstObject, secondObject, thirdObject, nil];
	// Easier to read literal syntax (no trailing nil needed)
	NSArray *someArray = @[firstObject, secondObject, thirdObject];
	
	if ([someArray count] > 0) {
	    NSLog(@"First item is: %@", [someArray objectAtIndex:0]);
	    // or
	    NSLog(@"First item is: %@", someArray[0]);
	}


### Dictionaries
	NSDictionary *dictionary = @{
	              @"anObject" : someObject,
	           @"helloString" : @"Hello, World!",
	           @"magicNumber" : @42,
	                @"aValue" : someValue
	};
	
	NSNumber *storedNumber = [dictionary objectForKey:@"magicNumber"];
	// or
	NSNumber *storedNumber = dictionary[@"magicNumber"];


## Blocks

Like closures/lambdas in other languages
	^{
	  NSLog(@"It's a block!");
	}


## Error Handling

[Error Handling Programming Guide for Cocoa](https://developer.apple.com/library/content/documentation/Cocoa/Conceptual/ErrorHandlingCocoa/)

	    NSString *domain = @"net.daveops.MyApplication.ErrorDomain";
	    NSString *desc = NSLocalizedString(@"Unable to…", @"");
	    NSDictionary *userInfo = @{ NSLocalizedDescriptionKey : desc };
	 
	    NSError *error = [NSError errorWithDomain:domain
	                                         code:-101
	                                     userInfo:userInfo];


### Exceptions
	    @try {
	        // do something that might throw an exception
	    }
	    @catch (NSException *exception) {
	        // deal with the exception
	    }
	    @finally {
	        // optional block of clean-up code
	        // executed whether or not an exception occurred
	    }



## i18n

NSLocalizedString - strings
NSNumberFormatter - numerical values
NSDateFormatter - format dates
NSLocale

# Perl

## Syntax Cheatsheet

```perl
#!/usr/local/bin/perl -wT
use strict

$scalar = 0;
@array = [1,2,3];

if (expr) block elsif (expr) block else block;

sub hw {
  print "Hello World"
}

hw();
```

## LWP - HTTP Get

`get($url)`

### To save to a file

`getstore($url, $file)`

### See if LWP is installed

```bash
perl -MLWP -le "print(LWP->VERSION)"
```

## Parsing JSON

```perl
use JSON;
decode_json($json);
```
# Smalltalk
<http://www.smalltalk.org>


# "R"

https://www.r-project.org/

## Syntax cheatsheet

```r
# create a vector
things <- c('foo', 'bar', 'baz')
```

## plotting

### bar chart

```r
png(file="output.png")
barplot(vector,names.arg=vector_of_names, xlab="groups", ylab="frequency")
dev.off()
```

## import a CSV

```r
dat <- read.csv(file="foobar.csv", header=TRUE, sep=",")
```

# COBOL

* [COBOL on Cogs](http://www.coboloncogs.org)


# Fortran

* <https://fortran.io/>


# ML

<https://www.cl.cam.ac.uk/~lp15/MLbook/pub-details.html>

# Erlang

* All statements must end with a period and whitespace
* Atoms are like Ruby symbols

## Shell

```erlang
help().
q().

f(Variable). % drop a variable
f(). % drop all variables
```

## Syntax cheatsheet

```erlang
% division
5 / 2.
5 div 2.
5 rem 2.

% base conversion
2#1010. % is base 2 for 10

% equality
1 =:= 1. % returns true
1 =/= 0. % returns true
```
# swift

## Benefits
Variables initialized before use
- var for variable, let for constant
- specify type after variable with <variable_name>:<type>
Check for array out-of-bounds
Check for integer overflow
Explicit nil handling
Auto memory management
Error handling


## CLI

swift - start up a Swift REPL (backed by LLDB)
swiftc - compile swift code

## Syntax cheatsheet

No semicolons needed
No main() needed, global scope is entry point
Values are never implicitly converted to another type

String interpolation: "The building is \(feet) feet tall"

```swift
let multi_line_string = """
multi
line
string
"""

let dictionary = [
  "hotpotato": 1,
  "coldpotato" 2
]

let empty_dict = [:]

if boolean_var {
  print("this is true")
}

var optionalString: String? = "Maybe hello?"
```

## Links
- <https://swift.org>

# Lisp

(This is more a directory of interesting Lisp artifacts and sites I've stumbled across, and don't want to have to find again).

- [ISLISP](http://www.islisp.info) - a Lisp standard

## Quotes

> "LISP is a high-level language, but you can still feel the bits sliding between your toes." -Guy Steele
# Scheme

* <https://www.scheme.com>
## Syntax cheatsheet

```scheme
"This is a string"
CaseSensitiveIdentifiers

(define add2 
  (lambda (n)
    (+ n 2)))
;; syntactic sugar:
(define (add2 n)
  (+ n 2))

(load "filename.ss")
```
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

# node.js

[NodeJS ECMAScript Support](http://node.green/)

## Debugging

```bash
node --inspect ...
```

Can also be turned on with SIGUSR1

Node <7 - Debugger API

Node >8 - Inspector API

### Debugger

Connect with ``node inspect HOST:PORT``

Debug directly with ``node inspect file.js``

Inserting ``debugger;`` in your code sets a breakpoint

#### Stepping

| cont, c | Continue execution
| next, n | Step next
| step, s | Step in
| out, o  | Step out
| pause   | Pause running code (like pause button in Developer Tools)


#### Breakpoints

| setBreakpoint(), sb()                    | Set breakpoint on current line
| setBreakpoint(line), sb(line)            | Set breakpoint on specific line
| setBreakpoint('fn()'), sb(...)           | Set breakpoint on a first statement in functions body
| setBreakpoint('script.js', 1), sb(...)   | Set breakpoint on first line of script.js
| clearBreakpoint('script.js', 1), cb(...) | Clear breakpoint in script.js on line 1

#### Information

| backtrace, bt | Print backtrace of current execution frame
| list(5)       | List scripts source code with 5 line context (5 lines before and after)
| watch(expr)   | Add expression to watch list
| unwatch(expr) | Remove expression from watch list
| watchers      | List all watchers and their values (automatically listed on each breakpoint)
| repl          | Open debugger's repl for evaluation in debugging script's context
| exec expr     | Execute an expression in debugging script's context


#### Execution control

| run     | Run script (automatically runs on debugger's start)
| restart | Restart script
| kill    | Kill script

#### Various

| scripts | List all loaded scripts
| version | Display V8's version

## Links

* http://blog.npmjs.org/post/141577284765/kik-left-pad-and-npm