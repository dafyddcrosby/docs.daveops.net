# HTML

## Shim for old IE browsers

```html
<!--[if lt IE 9]>
<script src="<http://html5shim.googlecode.com/svn/trunk/html5.js>"></script>
<![endif]-->
```

Forms
-----

```html
<form name="input" action="script.php" method="get">
Username: <input type="text" name="user" />
Favorite color:
<select name="color">
  <option value="red">Red</option>
  <option value="blue">Blue</option>
</select>
<input type="submit" value="Submit" />
</form>
```

Input attributes (HTML 5)
-------------------------


* email
* color
* seach
* tel
* url
* date
* month
* week
* time
* datetime
* datetime-local (no timezone)
* number
* range (restrict to valid numeric range


use `required` to have the browser whine

```html
<input ... required>
```

Code samples
------------

```html
<code>print 'hello world'</code> prints <samp>hello world</samp>
```

Canvas (HTML 5)
---------------


### initialization

```html

 <canvas id='id' height='600' width='800'>fallback</canvas>
 <script type='text/javascript'>
   var canvas = document.getElementById(id)
   if (canvas.getContext) {
 var context = canvas.getContext(type)
   }
 </script>
```

Fixed-meter bar (HTML 5)
------------------------

```html

 <meter value='num' min='num' max='num' optimum='num'>fallback display</meter>
```

Progress bar (HTML 5)
---------------------

```html

 <progress value='num' max='num'>fallback display</progress>
```

Use JavaScript to move it around

Autofocus input (HTML 5)
------------------------

```html
 
 <input ... autofocus>
```

Patterned input (HTML 5)
------------------------

### Enter 15 digits

```html

 <input type='text' pattern='[0-9]{15}'>
```

Dropdown list for text input (HTML 5)
-------------------------------------

```html
<input type='text' ... list='listid'>
<datalist id='id'>
  <option label='label1' value='value1'>
  <option label='label2' value='value2'>
</datalist>
```

Editable content
----------------

```html
<p contenteditable="true">
```

Link for phone numbers (mobile)
-------------------------------

International calling code is required

```html

 <a href="tel:+14035555555">403-555-5555</a>
```

### To handle an extension

```html

 <a href="tel:+14035555555p23">403-555-5555 ext. 23</a>
```

### To use a fax line:

```html

 <a href="fax:+14035555555">403-555-5555</a>
```

Doctype (HTML 5)
----------------

```html
<!doctype html>
```

Semantic tags (HTML 5)
----------------------

* <section>
* <nav>
* <article>
* <hgroup> (for article tag)


```html
<article>
  <hgroup>
 <h1>Title</h1>
 <h2>Byline</h2>
  </hgroup>
</article>
```

* <p> (In HTML 5, don't use the p tag for paragraph breaks - it *must* be used for paragraphs)
* <figure> (for article tag)
* <figcaption> (for figure tag)
* <aside> (for p tags)


Good meta tags to have
----------------------

* <http://commoncrawl.org/>
* <title> - up to 70 characters of relevant text
* <meta name=”description” content=”155 characters of message matching text”>
* <link rel=”author” href=”<https://plus.google.com/[YOUR> PERSONAL G+ PROFILE HERE]”/>
* <a href=”<https://plus.google.com/[YOUR> PERSONAL G+ PROFILE NUMBER]” rel=”me”>Me on Google+</a>
* <link rel=”publisher” href=”<https://plus.google.com/[YOUR> BUSINESS G+ PROFILE HERE]”/>
* [OpenGraph](./opengraph.md)
* [Web Services:Twitter](twitter.md)


<http://www.iacquire.com/blog/18-meta-tags-every-webpage-should-have-in-2013>

HTML entities for accents
-------------------------

Modifier   | Example | HTML
---        | ---     | ---
Grave      | à       | &agrave;
Acute      | á       | &aacute;
Circumflex | â       | &acirc;
Tilde      | ã       | &atilde;
Umlaut     | ä       | &auml;
Cedil      | ç       | &ccedil;


