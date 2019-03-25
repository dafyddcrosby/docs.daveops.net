---
title: Perl
---

Syntax Cheatsheet
-----------------
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

LWP - HTTP Get
--------------

`get($url)`

### To save to a file

`getstore($url, $file)`

### See if LWP is installed

`perl -MLWP -le "print(LWP->VERSION)"`

Parsing JSON
------------



  use JSON;
  decode_json($json);

