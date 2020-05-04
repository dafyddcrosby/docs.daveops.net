---
title: jQuery
tags: ["JavaScript"]
---

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
