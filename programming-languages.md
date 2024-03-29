
# Programming Languages

# C


## resources

- <http://splint.org>
- <http://c-faq.com/>
- <https://www.securecoding.cert.org/confluence/display/c/SEI+CERT+C+Coding+Standard>
- [GCC non-bugs](https://gcc.gnu.org/bugs/#nonbugs_c)\_
- <http://www.slideshare.net/olvemaudal/deep-c/24-What_will_happen_if_you>


## overview


### Memory

- Free allocated memory when you are done with it, don't assume that OS will clean up your mess.


### Casting

- Don't cast returned pointers from malloc. `(void *)` should get automatically promoted to any pointer type, and casting just makes it likely you'll get it wrong.


### keywords

- \_Alignas (C11)
- \_Alignof (C11)
- \_Atomic (C11)
- \_Bool (C99)
- \_Complex (C99)
- \_Decimal128 (C23)
- \_Decimal32 (C23)
- \_Decimal64 (C23)
- \_Generic (C11)
- \_Imaginary (C99)
- \_Noreturn (C11)
- \_Static<sub>assert</sub> (C11)
- \_Thread<sub>local</sub> (C11)
- alignas (C23)
- alignof (C23)
- auto
- bool (C23)
- break
- case
- char
- const
- constexpr (C23)
- continue
- default
- do
- double
- else
- enum
- extern
- false (C23)
- float
- for
- goto
- if
- inline (C99)
- int
- long
- nullptr (C23)
- register
- restrict (C99)
- return
- short
- signed
- sizeof
- static
- static<sub>assert</sub> (C23)
- struct
- switch
- thread<sub>local</sub> (C23)
- true (C23)
- typedef
- typeof (C23)
- typeof<sub>unqual</sub> (C23)
- union
- unsigned
- void
- volatile
- while


### Standards


#### C23

-   links

    - <https://queue.acm.org/detail.cfm?id=3588242>
    - <https://lemire.me/blog/2024/01/21/c23-a-slightly-better-c/>


### Install tooling in Fedora

```shell
sudo yum groupinstall "C Development Tools and Libraries"
```


### Open and read file

```C
const char *filename = "file.txt";
unsigned char byte;
FILE *fp;

fp = fopen(filename, "rb");

if (!fp) {
        printf("Couldn't open file\n");
        return 1;
}

while(!feof(fp)) {
        fread(&byte, sizeof(int), 1, fp);
        printf("%i\n",byte);
}

fclose(fp);
```


## libraries


### glibc


#### Get version

```shell
/lib/libc.so.6
```

or

```C
#include <stdio.h>
#include <gnu/libc-version.h>
int main (void) { puts (gnu_get_libc_version ()); return 0; }
```


# C++


## resources

<https://en.cppreference.com/>


# Go


## resources


### [official website](https://golang.org/)


### docs

```shell
godoc -http=:8080
```


### [Language Specification](https://golang.org/ref/spec)


### [Go Koans](https://github.com/cdarwin/go-koans)


## overview


### syntax cheatsheet

```
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


### Modules

```shell
go mod init $MODULE_NAME
go mod vendor
go get -u
```


### FastCGI

```
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


### Templating

{% raw %}

```
{{/* comment */}}  Defines a comment
{{.}}              Renders root element
{{.Foo}}           Renders the "Foo"-field in a nested element

{{if .Done}} {{else}} {{end}}   Defines an if-statement
{{range .Items}} {{.}} {{end}}  Loops over all “Items” and renders each using {{.}}
{{block "bar" .}} {{end}}       Defines a block with the name "bar"
```

{% endraw %}


### Static linking

If you're not using CGo, you can statically link with `CGO_ENABLED`.

In Fedora this appears to be [on by default](https://src.fedoraproject.org/rpms/golang//blob/rawhide/f/golang.spec)

```shell
CGO_ENABLED=0 go build
```


### Cross-compilation

```shell
# Compile for AMD64 Linux
GOOS=linux GOARCH=amd64 go build
```

<https://golang.org/doc/install/source#environment>


## libraries


### Mage

<https://magefile.org/>


# Java


## overview


### Types

| type    | note                                            |
|------- |----------------------------------------------- |
| byte    | 8-bit signed two's complement integer           |
| short   | 16-bit signed two's complement integer          |
| int     | 32-bit signed two's complement integer          |
| long    | 64-bit two's complement integer                 |
| float   | single-precision 32-bit IEEE 754 floating point |
| double  | double-precision 64-bit IEEE 754 floating point |
| boolean | the size isn't precisely defined                |


### SSL


#### Debugging

```
# print each handshake message
java -Djavax.net.debug=ssl:handshake MyApp
```


#### Links

- <https://docs.oracle.com/javase/7/docs/technotes/guides/security/jsse/ReadDebug.html>
- <https://docs.oracle.com/javase/7/docs/technotes/guides/security/jsse/JSSERefGuide.html#Debug>


### Numeric literals

You can use underscore characters in SE7+ example: `long hexBytes = 0xFF_CC_DA_B5;`


### Zero copy

Avoid copying the file data across user/kernel boundary, instead have the kernel put the file in a buffer and use DMA to pass the data directly.

<https://www.ibm.com/developerworks/linux/library/j-zerocopy/>


### AES intrinsics

Requires Java 8 and Intel 2010+ Westmere

```
-XX:+UseAES -XX:+UseAESIntrinsics
```


## libraries


### Maven

<https://maven.apache.org/>


# Zig


## resources


### [Official website](https://ziglang.org/)


## overview


### CLI

```shell
# Get Clang version
zig cc --version
# Get compilable targets
zig targets
```


### releases


#### 0.5.0 - async


# Julia


## resources


### [official website](https://julialang.org/)


# Forth


## overview


### syntax

| words | action                        |
|----- |----------------------------- |
| .     | pop an element off the stack  |
| .s    | display the contents of stack |
| drop  | drop top of stack             |
| \\    | comment                       |
| see   | decompile                     |

| words   | action                       |
| include | includes a forth source file |


#### colon definitions

```forth
: funcname ( stack effect comment )
  do stuff ;

\ local variables
: funcname { a b -- b a }
  b a ;
```


# Kotlin


## resources


### [official website](http://kotlinlang.org/)


# Prolog


## resources

<https://bernardopires.com/2013/10/try-logic-programming-a-gentle-introduction-to-prolog/>


# Tcl


## overview


### syntax cheatsheet

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


### profiling statements

```tcl
puts "unbraced: [time { expr 15 * 20 } 1000]"
puts "braced:   [time { expr {15 * 20} } 1000]"
```


### testing

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


### disassemble statement

This is fun, because it shows just how much of a difference there is in optimizing statements

```tcl
::tcl::unsupported::disassemble script {expr $a eq $b}
::tcl::unsupported::disassemble script {expr {$a eq $b}}
```


### namespaces

```tcl
namespace eval blah {}
```


## libraries


### OpenACS


#### `ad_proc` documentation metadata

- @return
- @see
- @author
- @param
- @error


#### Return a file

```tcl
set file [open $file_path "r"]
ns_set update [ns_conn outputheaders] content-disposition "attachment; filename=$filename"
ns_returnfile 200 [ns_guesstype $file] $file_path
```


#### Make your OpenACS form's checkbox be selected by default

In `ad_page_contract`:

```
{fries "t"}
```

In `ad_form`:

{% raw %}

```
{fries:text(checkbox),optional
        {label {"Would you like fries with that?"}}
        {html {[if {$fries=="t"} {return "checked checked"}]}}
        {options {{"" t}}}
}
```

{% endraw %}


#### ADP tags

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


## overview


### i18n

| NSLocalizedString | strings          |
| NSNumberFormatter | numerical values |
| NSDateFormatter   | format dates     |
| NSLocale          |                  |


### Classes

Categories extend classes


#### Interface

```
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
```


#### Implementation

```
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
```


### Objects

```
// setter methods are set + property name
[person setFirstName:@"Bob"];
// or use dot syntax
person.firstName = @"Bob";
```


### Protocols

```
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
```


### Collections


#### Arrays

```
// Trailing nil needed
NSArray *someArray = [NSArray arrayWithObjects:firstObject, secondObject, thirdObject, nil];
// Easier to read literal syntax (no trailing nil needed)
NSArray *someArray = @[firstObject, secondObject, thirdObject];

if ([someArray count] > 0) {
    NSLog(@"First item is: %@", [someArray objectAtIndex:0]);
    // or
    NSLog(@"First item is: %@", someArray[0]);
}
```


#### Dictionaries

```
NSDictionary *dictionary = @{
              @"anObject" : someObject,
           @"helloString" : @"Hello, World!",
           @"magicNumber" : @42,
                @"aValue" : someValue
};

NSNumber *storedNumber = [dictionary objectForKey:@"magicNumber"];
// or
NSNumber *storedNumber = dictionary[@"magicNumber"];
```


### Blocks

Like closures/lambdas in other languages

```
^{ NSLog(@"It's a block!"); }
```


### Error Handling

[Error Handling Programming Guide for Cocoa](https://developer.apple.com/library/content/documentation/Cocoa/Conceptual/ErrorHandlingCocoa/)

```
NSString *domain = @"net.daveops.MyApplication.ErrorDomain";
NSString *desc = NSLocalizedString(@"Unable to…", @"");
NSDictionary *userInfo = @{ NSLocalizedDescriptionKey : desc };

NSError *error = [NSError errorWithDomain:domain
                                     code:-101
                                 userInfo:userInfo];
```


#### Exceptions

```
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
```


# Self


## resources


### [official website](https://selflanguage.org/)


# Perl


## overview


### syntax Cheatsheet

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


### Parsing JSON

```perl
use JSON;
decode_json($json);
```


### LWP - HTTP Get

`get($url)`


#### To save to a file

`getstore($url, $file)`


#### See if LWP is installed

```shell
perl -MLWP -le "print(LWP->VERSION)"
```


# Smalltalk


## resources

<http://www.smalltalk.org>

[Strongtalk (Smalltalk with a strong static type system)](http://www.strongtalk.org/)

[Pharo](https://pharo.org)


# "R"


## resources


### [official website](https://www.r-project.org/)


## overview


### syntax cheatsheet

```R
# create a vector
things <- c('foo', 'bar', 'baz')
```


### import a CSV

```R
dat <- read.csv(file="foobar.csv", header=TRUE, sep=",")
```


### plotting


#### bar chart

```R
png(file="output.png")
barplot(vector,names.arg=vector_of_names, xlab="groups", ylab="frequency")
dev.off()
```


# COBOL


## libraries


### [COBOL on Cogs](http://www.coboloncogs.org)


# Fortran


## libraries


### [Fortran.io web framework](https://fortran.io/)


# ML

<https://www.cl.cam.ac.uk/~lp15/MLbook/pub-details.html>


# Erlang


## overview


### syntax cheatsheet

- All statements must end with a period and whitespace
- Atoms are like Ruby symbols

```
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


### Shell

```
help().
q().

f(Variable). % drop a variable
f(). % drop all variables
```


# Swift


## resources


### [official website](https://swift.org)


## overview


### syntax cheatsheet

No semicolons needed

No main() needed, global scope is entry point

Values are never implicitly converted to another type

String interpolation: "The building is (feet) feet tall"

```
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


### CLI

swift - start up a Swift REPL (backed by LLDB)

swiftc - compile swift code


### Benefits

Variables initialized before use

- var for variable, let for constant
- specify type after variable with :

- Check for array out-of-bounds
- Check for integer overflow
- Explicit nil handling
- Auto memory management
- Error handling


# PHP


## resources


### [official website](https://www.php.net/)


### [manual](https://www.php.net/manual/en/)


### [source code](https://github.com/php/php-src)

```shell
git clone git@github.com:php/php-src.git
```


## overview


### syntax cheatsheet

```
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
}

function foo () {
  return 0;
}
?>
```


### CLI


#### Get PHP config info

```shell
# Get configuration (like phpinfo())
php -i
# Get location of php.ini
php --ini
```


#### Run a server instance with a script

Requires PHP >= 5.4

<http://php.net/manual/en/features.commandline.webserver.php>

```shell
php -S localhost:8000 router.php
```


### releases


#### [8.1](https://www.php.net/releases/8.1/en.php)

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


### Datestamps

```
<?php
//mktime(hour,minute,second,month,day,year)

echo date("Y/m/d", mktime(0,0,0,11,11,2011));
//2011/11/11
?> 
```


### Autogenerate a file download

```
<?php
header("Content-Type: text/calendar; charset=utf-8");
header("Content-Disposition: attachment; filename=test.ics");
// render file...
```


### Turn off error reporting

```
<?php
error_reporting(0)
?>
```


### Check if domain resolves

```
<?php
checkdnsrr($host)
?>
```


### Authenticating users

```
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
```


### Validation

```
<?php
// Numeric validation
is_int($var)
is_numeric($var)
?>
```


### Export to CSV

```
<?php
header("Expires: 0");
header("Cache-control: private");
header("Cache-Control: must-revalidate, post-check=0, pre-check=0");
header("Content-Description: File Transfer");
header("Content-Type: text/csv");
header("Content-disposition: attachment; filename=rawlogs.csv");
?>
```


### Get filename

```
<?php
$FILE_NAME = basename($_SERVER["PHP_SELF"]);
?>
```


### Return multiple values

```
<?php
function mult() {
    return array(0, 1);
}

list ($zero, $one) = mult();
?>
```


### Create an array of objects

```
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
```


### SQLite


#### Initializing

```
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
```


#### Reading values

```
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
```


### Scrape `$_GET` parameters

If `mod_rewrite` or some other mechanism is preventing the filling of the `$_GET` array, use this:

```
<?php
parse_str($_SERVER['QUERY_STRING'], $_GET);
?>
```


### Convert command line arguments into GET variables

```
<?php
parse_str(implode('&amp;', array_slice($argv, 1)), $_GET);
?>
```


### Redirect to a different URL

```
<?php
header("Location: https://example.org/"); 
?>
```


### Insecurity

PHP has more than a few security pitfalls, this is just a quick list of ways it can bite you.


#### $FILES `tmp_name` usage

`tmp_name` is untrusted input and should be sanitized before doing any file operations (like `move_uploaded_file`)


#### `exif_imagetype` is not validation

If you're testing a file to ensure it's an image, `exif_imagetype` alone is inadequate, as it can be easily bypassed with a magic string like "BM" (for bitmap)


### PDO

```
// simple query
<?php
$sql = "SELECT max(id) FROM table";
$sth = $DB->prepare($sql);
$sth->execute();
$max_id= $sth->fetchColumn();
?>
```


### Binary


#### Read a binary file

```
<?php
$handle = fopen('/path/to/file', 'rb');
$contents = fread($handle, filesize($filename));
fclose($handle);
?>
```


#### Binary craziness

```
<?php
bindec();
decbin();
?>
```


#### Playing with endianness

```
<?php
// For changing endianness, just use strrev()
// %016b for 16-bit, %032b for 32-bit
strrev(sprintf("%016b", $int));
?>
```


#### Unpacking binary file formats

```
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
```


### Barcodes


#### Get a UPC check digit

```
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


## libraries


### composer

[Composer homepage](https://getcomposer.org/)


#### Require a package

```shell
composer require repo/package
```

Will drop `composer.json` and `composer.lock`


#### Load vendor directory

```
<?php
require __DIR__ . '/vendor/autoload.php';
```


### Create a barcode with `Image_Barcode2` (PEAR)

```
<?php
require_once 'Image/Barcode2.php';
$bc = new Image_Barcode2;
$bc->draw($_GET['bctext'], "int25", "png");
?>
```


### Include PEAR (Dreamhost)

```
<?php
//Include my PEAR path
set_include_path("." . PATH_SEPARATOR . ($UserDir = dirname($_SERVER['DOCUMENT_ROOT'])) . "/pear/php" . PATH_SEPARATOR . get_include_path());
?>
```


# node.js

[NodeJS ECMAScript Support](http://node.green/)


## overview


### Debugging

```shell
node --inspect ...
```

Can also be turned on with SIGUSR1

Node <7 - Debugger API

Node >8 - Inspector API


### Debugger

Connect with `node inspect HOST:PORT`

Debug directly with `node inspect file.js`

Inserting `debugger;` in your code sets a breakpoint


#### Stepping

| cont, c | Continue execution                                        |
| next, n | Step next                                                 |
| step, s | Step in                                                   |
| out, o  | Step out                                                  |
| pause   | Pause running code (like pause button in Developer Tools) |


#### Breakpoints

| setBreakpoint(), sb()                    | Set breakpoint on current line                        |
| setBreakpoint(line), sb(line)            | Set breakpoint on specific line                       |
| setBreakpoint('fn()'), sb(...)           | Set breakpoint on a first statement in functions body |
| setBreakpoint('script.js', 1), sb(...)   | Set breakpoint on first line of script.js             |
| clearBreakpoint('script.js', 1), cb(...) | Clear breakpoint in script.js on line 1               |


#### Information

| backtrace, bt | Print backtrace of current execution frame                                   |
| list(5)       | List scripts source code with 5 line context (5 lines before and after)      |
| watch(expr)   | Add expression to watch list                                                 |
| unwatch(expr) | Remove expression from watch list                                            |
| watchers      | List all watchers and their values (automatically listed on each breakpoint) |
| repl          | Open debugger's repl for evaluation in debugging script's context            |
| exec expr     | Execute an expression in debugging script's context                          |


#### Execution control

| run     | Run script (automatically runs on debugger's start) |
| restart | Restart script                                      |
| kill    | Kill script                                         |


#### Various

| scripts | List all loaded scripts |
| version | Display V8's version    |


### left-pad

- <http://blog.npmjs.org/post/141577284765/kik-left-pad-and-npm>


# Rust


## resources


### [official website](https://www.rust-lang.org/)


### docs website

- [Rust Programming Language Book](https://doc.rust-lang.org/stable/book/)
    - [Stack-Only Data: Copy](https://doc.rust-lang.org/stable/book/ch04-01-what-is-ownership.html#stack-only-data-copy)
- <https://doc.rust-lang.org/core/>
- <https://www.rust-lang.org/documentation.html>
- <https://doc.rust-lang.org/stable/rust-by-example/>


### editor support

- [vim plugin](https://github.com/rust-lang/rust.vim)


### Ferris the crab

<https://rustacean.net/>


## overview


### syntax cheatsheet

```
//! You can create a description for your crate using two slashes and an exclamation mark

// Add debugging info to struct
// Trait is std::fmt::Debug
/// Documentation comments use three slashes and Markdown
#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

// use impl (implementation) block to create methods
impl Rectangle {
  fn area(&self) -> u32 {
    self.width * self.height
  }
  fn is_square(&self) -> bool {
    self.width == self.height
  }
  // associated functions
  #[allow(dead_code)]
  fn square(size: u32) -> Rectangle {
    Rectangle { width: size, height: size }
  }
}

fn main() {
  let mut a = 1; // mut makes the value mutable
  println!("{}", a);
  a = 2;
  println!("{}", a);

  // shadowing
  let x = 2;
  let x = x + 2;

  eprinln!("Use this to print to stderr");

  loop {
    println!("simple loop");
    break;
  }

  println!("easy as");
  for number in 1..4 {
    println!("{}", number);
  }

  let rect = Rectangle { width: 10, height: 20 };
  // get struct details for debugging
  println!("rect is {:?}", rect);
  println!("pretty print rect is {:#?}", rect);

  // using match
  let thing = 5;
  match thing {
     5 => println!("five!"),
     6 => {
         println!("six?");
         println!("why six?");
     },
     _ => (), // wildcard, don't do anything
  }
  // or using if let
  if let 5 = thing {
    println!("still five");
  } else {
    println!("not five");
  }

  // make a vector
  let mut v = vec![1,2,3];
  for i in 4..11 {
    v.push(i);
  }
  for (j, l) in v.iter().enumerate() {
    println!("index {}: {}", j, l);
  }
}
```


### CLI


#### Cargo

```shell
# New project
cargo new project_name --bin
# Build a release
cargo build --release
# Test a build
cargo check
# Create documentation and open it in a browser
cargo doc --open
# Build the executable, run it
cargo run
```


#### rustc

```shell
# Build a library
rustc --crate-type=lib thing.rs
```


### testing

If the function passes, the test passes

```
#[test]
fn this_tests_code(){
  println!("");
  if 1 == 0 {
    fail!("This should never happen");
  }
}
```

To compile the tests and replace main with test runner:

`rustc --test test.rs`

<https://doc.rust-lang.org/stable/book/testing.html>


### Macros

| Macro            | Description                                  |
|---------------- |-------------------------------------------- |
| print!("text")   | Print a line to STDOUT                       |
| println!("text") | Print a line to STDOUT followed by a newline |


### Nightly rust

```shell
# Install nightly toolchain
rustup toolchain install nightly
# Use nightly toolchain by default
rustup default nightly
```


### Cargo


#### Cargo watch

```shell
cargo watch -x check -x test
```


### Platform support


#### Cross-compilation

- <https://blog.rust-lang.org/2016/05/13/rustup.html>
- <https://sigmaris.info/blog/2019/02/cross-compiling-rust-on-mac-os-for-an-arm-linux-router/>

```shell
rustup target list
rustup target add x86_64-unknown-linux-musl
cargo build --target x86_64-unknown-linux-musl
```

If you're working on Mac and compiling for Linux, you'll also want:

```shell
brew install x86_64-elf-binutils
```

And using that specific linker:

```
[target.x86_64-unknown-linux-musl]
linker = "x86_64-elf-ld"
```


#### iOS

```shell
# Be able to create iOS executables
cargo install cargo-lipo
# Install targets
rustup target add aarch64-apple-ios x86_64-apple-ios
```


#### Windows

[Rust on Windows](https://docs.microsoft.com/en-us/windows/dev-environment/rust/)


#### Webassembly

-   Using --target web

    ```shell
    wasm-pack build --target web
    ```
    
    ```
    <html>
      <head>
        <meta content="text/html;charset=utf-8" http-equiv="Content-Type"/>
      </head>
      <body>
        <!-- Note the usage of `type=module` here as this is an ES6 module -->
        <script type="module">
          // Use ES module import syntax to import functionality from the module
          // that we have compiled.
          //
          // Note that the `default` import is an initialization function which
          // will "boot" the module and make it ready to use. Currently browsers
          // don't support natively imported WebAssembly as an ES module, but
          // eventually the manual initialization won't be required!
          import init, { add } from './pkg/without_a_bundler.js';
    
          async function run() {
            // First up we need to actually load the wasm file, so we use the
            // default export to inform it where the wasm file is located on the
            // server, and then we wait on the returned promise to wait for the
            // wasm to be loaded.
            //
            // It may look like this: `await init('./pkg/without_a_bundler_bg.wasm');`,
            // but there is also a handy default inside `init` function, which uses
            // `import.meta` to locate the wasm file relatively to js file.
            //
            // Note that instead of a string you can also pass in any of the
            // following things:
            //
            // * `WebAssembly.Module`
            //
            // * `ArrayBuffer`
            //
            // * `Response`
            //
            // * `Promise` which returns any of the above, e.g. `fetch("./path/to/wasm")`
            //
            // This gives you complete control over how the module is loaded
            // and compiled.
            //
            // Also note that the promise, when resolved, yields the wasm module's
            // exports which is the same as importing the `*_bg` module in other
            // modes
            await init();
    
            // And afterwards we can use all the functionality defined in wasm.
            const result = add(1, 2);
            console.log(`1 + 2 = ${result}`);
            if (result !== 3)
              throw new Error("wasm addition doesn't work!");
          }
    
          run();
        </script>
      </body>
    </html>
    ```


### async

<https://areweasyncyet.rs/>


# Lua


## overview


### syntax cheatsheet

```
function function_name ( args ) body end

while exp do block end

for variable = from_exp , to_exp [, step_exp] do block end
(foreach) for var {, var} in explist do block end

repeat block until exp

if exp then block { elseif exp then block } [ else block ] end
```


### Iterate through file

```
for line in io.lines("my.txt") do print(line) end
```


# Rexx

<https://en.wikipedia.org/wiki/Rexx>


# JavaScript


## resources


### specifications

- [JavaScript specification](https://tc39.github.io/)


### [JavaScript deobfuscator](https://lelinhtinh.github.io/de4js/)


### Links

- [Esoteric variant](http://www.jsfuck.com/)


### jq

- <https://jqplay.org/>
- <https://stedolan.github.io/jq/>
- [jid - interactive JSON manipulation](https://github.com/simeji/jid)


## overview


### syntax


#### var vs. let

Use `let`, since it limits the scope to the block. `const` is also block-scoped.


#### Classes

-   Pre-ES5 classes

    ```
    function Building(x,y,z) {
      this.x = x;
      this.y = y;
      this.z = z;
    }
    
    Building.prototype.area = function () {
      return x * y * z;
    }
    
    var house = Building(20,20,10);
    ```


#### ES6 Modules

```
<script type="module">
import * as FOO from './lib/foo.js';
import { baz, bax } from './lib/bar.js';
// ...
</script>
```

All modules are parsed with strict mode

[MDN Modules](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)


### Strict Mode

```
'use strict'
```

[MDN Strict Mode docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Strict_mode)


### Debugger

```
debugger;
```


## libraries


### jQuery

<https://jquery.com>


#### No-conflict mode

```
var $j = jQuery.noConflict();
```


#### Append a node to the DOM

```
$('#thing').append('<p>blerg</p>');'</p>')
```


#### Get first element of several

CSS pseudo-selectors $(".stuff li:first");

DOM traversal $(".stuff").first();


#### Searches for the closest ancestor that matches

```
.closest()
```


#### Objects

Use $(this) instead of this - it's a jQuery object


#### Get custom data attributes from the DOM

date(name)


#### Get direct children from an element

```
$("ul").children("li");
```


#### Get parent element of a node

```
$("#thing").parent();
```


### [d3js](https://d3js.org/)


### Tangle

<http://worrydream.com/Tangle/guide.html>


### React

<https://reactjs.org/>


#### React Native

- <http://facebook.github.io/react-native/>
- <https://reactnative.dev/>


### Typescript

- <https://www.typescriptlang.org/>


### CoffeeScript

- <https://coffeescript.org/>


# Elixer

Ruby/Lisp-alike run on Erlang BEAM


# Crystal


## resources


### [official website](https://crystal-lang.org/)


## libraries


### [Kemal](https://kemalcr.com/)

Sinatra-ish web framework

<https://kemalcr.com/guide/>


# OCaml


## resources


### [official website](https://ocaml.org/)


# Clojure


## resources


### [official website](https://clojure.org/)


### [language reference](https://clojure.org/reference/reader)


### community


#### [The Clojure Toolbox](https://www.clojure-toolbox.com/)


# Starlark

A subset of Python intended as a configuration language


## resources


### specifications

- [Starlark specification](https://github.com/bazelbuild/starlark/blob/master/spec.md)


# Nim


## resources


### [official website](https://nim-lang.org)
