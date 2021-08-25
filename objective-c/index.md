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

