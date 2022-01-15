# JavaScript

## Strict Mode

```javascript
'use strict'
```

[MDN Strict Mode docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Strict_mode)

## Debugger

```javascript
debugger;
```

## Classes

### Pre-ES5 classes

```javascript
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

## ES6 Modules

```html
<script type="module">
import * as FOO from './lib/foo.js';
import { baz, bax } from './lib/bar.js';
// ...
</script>
```

All modules are parsed with strict mode

[MDN Modules](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)

## var vs. let

Use `let`, since it limits the scope to the block. `const` is also
block-scoped.

## TODO 

* <http://matt.might.net/articles/learning-javascript-in-small-bites-part-2/>
* <http://matt.might.net/articles/learning-javascript-in-small-bites-part-3/>
* <http://matt.might.net/articles/learning-javascript-in-small-bites-part-4/>

<http://2017.js13kgames.com/>

* https://www.sweetjs.org/

## Links

* [JavaScript specification](https://tc39.github.io/)
* [JavaScript deobfuscator](https://lelinhtinh.github.io/de4js/)
* [Esoteric variant](http://www.jsfuck.com/)


# jQuery

<https://jquery.com>

## No-conflict mode

```javascript
var $j = jQuery.noConflict();
```

## Append a node to the DOM

```javascript
$('#thing').append('<p>blerg</p>');'</p>')
```

## Get first element of several

CSS pseudo-selectors   $(".stuff li:first");

DOM traversal   $(".stuff").first();

## Searches for the closest ancestor that matches

```javascript
.closest()
```

## Objects


Use $(this) instead of this - it's a jQuery object

## Get custom data attributes from the DOM

date(name)

## Get direct children from an element

```javascript
$("ul").children("li");
```

## Get parent element of a node

```javascript
$("#thing").parent();
```


# Tangle

<http://worrydream.com/Tangle/guide.html>



# CoffeeScript

<http://coffeescript.org/>
<http://www.coffeelint.org/>




# Typescript

https://www.typescriptlang.org/


# React

<https://reactjs.org/>

# React Native

<http://facebook.github.io/react-native/>
https://reactnative.dev/


# jq

* <https://jqplay.org/>
* <https://stedolan.github.io/jq/>
* [jid - interactive JSON manipulation](https://github.com/simeji/jid)



# d3js

<https://d3js.org/>

