# Markup languages

# GeoJSON

https://en.wikipedia.org/wiki/GeoJSON

# Virtual Human Markup Language
https://en.wikipedia.org/wiki/VHML

# EmotionML
https://www.w3.org/TR/emotionml/Overview.html

# HTML

## Application cache
* <http://www.html5rocks.com/en/tutorials/appcache/beginner/>


## Preloading

- https://instant.page/
- https://github.com/instantpage/instant.page

```html
<link rel="prefetch" href="_url_">
```
## Shim for old IE browsers

```html
<!--[if lt IE 9]>
<script src="<http://html5shim.googlecode.com/svn/trunk/html5.js>"></script>
<![endif]-->
```

## Forms

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

## Input attributes (HTML 5)


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

## Code samples

```html
<code>print 'hello world'</code> prints <samp>hello world</samp>
```

## Canvas (HTML 5)


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

## Fixed-meter bar (HTML 5)

```html

 <meter value='num' min='num' max='num' optimum='num'>fallback display</meter>
```

## Progress bar (HTML 5)

```html

 <progress value='num' max='num'>fallback display</progress>
```

Use JavaScript to move it around

## Autofocus input (HTML 5)

```html
 
 <input ... autofocus>
```

## Patterned input (HTML 5)

### Enter 15 digits

```html

 <input type='text' pattern='[0-9]{15}'>
```

## Dropdown list for text input (HTML 5)

```html
<input type='text' ... list='listid'>
<datalist id='id'>
  <option label='label1' value='value1'>
  <option label='label2' value='value2'>
</datalist>
```

## Editable content

```html
<p contenteditable="true">
```

## Link for phone numbers (mobile)

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

## Doctype (HTML 5)

```html
<!doctype html>
```

## Semantic tags (HTML 5)

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


## Good meta tags to have

* <http://commoncrawl.org/>
* <title> - up to 70 characters of relevant text
* <meta name=”description” content=”155 characters of message matching text”>
* <link rel=”author” href=”<https://plus.google.com/[YOUR> PERSONAL G+ PROFILE HERE]”/>
* <a href=”<https://plus.google.com/[YOUR> PERSONAL G+ PROFILE NUMBER]” rel=”me”>Me on Google+</a>
* <link rel=”publisher” href=”<https://plus.google.com/[YOUR> BUSINESS G+ PROFILE HERE]”/>
* [OpenGraph](web_programming/opengraph.md)
* [Web Services:Twitter](twitter.md)


<http://www.iacquire.com/blog/18-meta-tags-every-webpage-should-have-in-2013>

## HTML entities for accents

Modifier   | Example | HTML
---        | ---     | ---
Grave      | à       | &agrave;
Acute      | á       | &aacute;
Circumflex | â       | &acirc;
Tilde      | ã       | &atilde;
Umlaut     | ä       | &auml;
Cedil      | ç       | &ccedil;




HAML

<http://haml.info>

## Install

```bash
gem install haml
```

## Syntax Cheatsheet

```haml
-# This is a comment line, !!! 5 does the HTML 5 doctype
!!! 5
%html
  %head
    %meta{:charset => "utf-8"}
    %title Demo HAML page
  %body
    -# you can use . or # instead of %div if you're using a div tag
    #shorthand
    %div#content
    = haml :footer
```


# Markdown

## Headings

1 to 6 # symbols

## Emphasis

```markdown
*This text will be italic*
_This will also be italic_
**This text will be bold**
__This will also be bold__
_You **can** combine them_
~~This line is using a strikethrough~~
```

## Comments

Use `<!---` and `-->`, doesn't work for inline comments

## Links

```markdown
<http://example.com> - automatic
[Example](<http://example.com>)
```



# reStructuredText

## Convert rst to HTML

```bash
rst2html FILE ...
```

## Syntax cheatsheet

```rst
*italics*
**bold**
``fixed-space literal``
.. comment
```


# xml

## Prettify

```bash
tidy -xml -i -m [file]
```

## XMLLint

```bash
# Check XML file is well-formed
xmllint --noout $FILE

# Check XML file against local DTD file
xmllint --noout --dtdvalid ./local.dtd $FILE
```

- <http://www.xmlsoft.org/>


# LaTeX

## adding quotes

```tex
\begin{quote}
...
\end{quote}
```

## Cheatsheet

```tex
\documentclass{book}
\begin{document}
\maketitle
\end{document}
```

## bibtex

```tex
% Add to the preamble:
% Load biblatex package
\usepackage{biblatex}
% Load bibTeX file
\addbibresource{file.bib}
% Add a citation somewhere in your file
Blah blah \autocite{<key>}
% In the backmatter,
\printbibliography
```

## document classes

* book
* article
* report
* letter
* slides

## ePub

* <https://www.tug.org/TUGboat/tb32-3/tb102rishi.pdf>
* <http://pandoc.org/>
* <https://tex2ebook.wordpress.com/>


## installing on Fedora

* texlive
* pdflatex
* texlive-xetex
* texlive-cm
* texlive-hyphen-base
* texlive-mfware

## Add a draft watermark

```tex
\usepackage{draftwatermark}
\SetWatermarkText{Draft}
```

## Tables

```tex
\begin{tabular}{||c || c | c ||}
\hline
Foo? & Column A & Column B
\hline
Bar & Yes & No
\hline
Bax & No & Yes
\hline
\end{tabular}
```


# RSS

## Wordpress

http://wp.example.org/feed

## Blogger / blogspot
* Atom 1.0: https://blogname.blogspot.com/feeds/posts/default
* RSS 2.0: https://blogname.blogspot.com/feeds/posts/default?alt=rss


# EPUB files

<http://code.google.com/p/epubcheck/>


 zip -X ../spm.epub mimetype css/style.css META-INF/container.xml book.ncx book.opf solplayaymar.xhtml


* ERROR: /media/UDISK/spm.epub: mimetype entry missing or not the first in archive
  * It is what it sounds like. When you create your zip archive, the first one in has to be the mimetype file.
* ERROR: /media/UDISK/spm.epub: extra field length for first filename must be 0, but was 28
  * When you run zip, use the -X argument so that there's no timestamps, etc. These are the 'extra fields'



# ASN.1 (Abstract Syntax Notation One)

https://en.wikipedia.org/wiki/Abstract_Syntax_Notation_One

Visually similar to Augmented Backus-Naur form, but is for data structures, not
syntax.


# SAML
Security Assertion Markup Language
https://samltest.id/

# Liquid templating system

https://shopify.github.io/liquid/

Uses objects, tags, and filters

Objects are `{{ }}` put within a template, that holds objects and variables.

Tags are for logic, `{% %}`

Filters are a `|` within a `{{ }}` block

