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

