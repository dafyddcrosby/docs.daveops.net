# CSS
<http://www.cssbuttongenerator.com/>

Media types
-----------


* all
* braille
* embossed
* handheld
* print
* projection
* screen
* speech
* tty
* tv


Mobile browsers
---------------

```html
<link rel='stylesheet' media='all' href='base.css'
<link rel='stylesheet' media='screen and (max-width: 320px)' href='mobile.css'>
```

Use a custom font (CSS3)
------------------------

```css
@font-face {
      font-family: MyFont;
      src: url(myfont.ttf), url(myfont.eot);
}
selector { font-family: MyFont; }
```

Rounded corners (CSS3)
----------------------

```css
border: width style color;
border-radius: radius;
```

Box shadow (CSS 3)
------------------

```css
box-shadow: xpos ypos blur spread color, ...;
```

Change an element's background image (CSS 3)
--------------------------------------------

```css
selector {
  background-image: url(imagefile), ...;
}
```

Translucent elements (CSS 3)
----------------------------

```css
selector {
  opacity: value;
  filter: alpha(opacity=value);
}
```

Create custom borders (CSS 3)
-----------------------------

```css
border-image: source slice repeat;
border-width: width;
```

Center an element
-----------------

Give it a width and set the margin

```css
margin: 0 auto;
width: 939px;
```
