# Programming Languages
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
# Fortran

* <https://fortran.io/>


# COBOL

* [COBOL on Cogs](http://www.coboloncogs.org)


# Kotlin

<http://kotlinlang.org/>

# Prolog
<https://bernardopires.com/2013/10/try-logic-programming-a-gentle-introduction-to-prolog/>

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



# SSL
## Debugging

	# print each handshake message
	java -Djavax.net.debug=ssl:handshake MyApp

## Links

* <https://docs.oracle.com/javase/7/docs/technotes/guides/security/jsse/ReadDebug.html>
* <https://docs.oracle.com/javase/7/docs/technotes/guides/security/jsse/JSSERefGuide.html#Debug>




# Maven
<https://maven.apache.org/what-is-maven.html>

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

# Go - FastCGI

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


# Mage

https://magefile.org/
# ML

<https://www.cl.cam.ac.uk/~lp15/MLbook/pub-details.html>

# Tcl


# Tcl

# Syntax cheatsheet

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

#Namespaces

```tcl
namespace eval blah {}
```

# Profiling statements

```tcl
puts "unbraced: [time { expr 15 * 20 } 1000]"
puts "braced:   [time { expr {15 * 20} } 1000]"
```

# Disassemble statement

This is fun, because it shows just how much of a difference there is in optimizing statements

```tcl
::tcl::unsupported::disassemble script {expr $a eq $b}
::tcl::unsupported::disassemble script {expr {$a eq $b}}
```

# Creating tests

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

# OpenACS

## ad_proc documentation metadata

- @return
- @see
- @author
- @param
- @error

## Return a file

```tcl
set file [open $file_path "r"]
ns_set update [ns_conn outputheaders] content-disposition "attachment; filename=$filename"
ns_returnfile 200 [ns_guesstype $file] $file_path
```

## Make your OpenACS form’s checkbox be selected by default
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

## ADP tags

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



# i18n

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

