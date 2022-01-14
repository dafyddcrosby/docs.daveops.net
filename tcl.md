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
