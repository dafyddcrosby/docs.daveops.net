# Markup languages

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

- email
- color
- seach
- tel
- url
- date
- month
- week
- time
- datetime
- datetime-local (no timezone)
- number
- range (restrict to valid numeric range)

use `required` to have the browser whine

```html
<input ... required>
```

## Code samples

```html
<code>print 'hello world'</code> prints <samp>hello world</samp>
```

## Canvas (HTML 5)

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
<meter value='num' min='num' max='num' optimum='num'>
fallback display
</meter>
```

## Progress bar (HTML 5)

```html
<progress value='num' max='num'>
fallback display
</progress>
```

Use JavaScript to move it around

## Autofocus input (HTML 5)

```html
<input ... autofocus>
```

## Patterned input (HTML 5)

```html
<!-- Enter 15 digits -->
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

<!-- To handle an extension -->
<a href="tel:+14035555555p23">403-555-5555 ext. 23</a>

<!-- To use a fax line -->
<a href="fax:+14035555555">403-555-5555</a>
```

## Doctype (HTML 5)

```html
<!doctype html>
```

## Semantic tags (HTML 5)

```
- <section>
- <nav>
- <article>
- <hgroup> (for article tag)
- <p> (In HTML 5, don't use the p tag for paragraph breaks - it *must* be used for paragraphs)
- <figure> (for article tag)
- <figcaption> (for figure tag)
- <aside> (for p tags)
```

```html
<article>
  <hgroup>
 <h1>Title</h1>
 <h2>Byline</h2>
  </hgroup>
</article>
```

## Good meta tags to have

- <http://commoncrawl.org/>

```html
<head>
<title>up to 70 characters of relevant text</title>
<meta name="description" content="155 characters of message matching text">
</head>
```

- [OpenGraph](web_programming/opengraph.md)
- [Web Services:Twitter](twitter.md)

<http://www.iacquire.com/blog/18-meta-tags-every-webpage-should-have-in-2013>

# HAML

<https://haml.info>

```bash
gem install haml
```

```haml
-# This is a comment line, !!! 5 does the HTML 5 doctype
!!! 5
%html
  %head
    %meta{charset: "utf-8"}
    -# You can pass variables to attributes
    %meta{variable: attribute_variable}
    %title Demo HAML page
  %body
    -# you can use . or # instead of %div if you're using a div tag
    #shorthand
    %div#content
    = haml :footer
```

# Markdown

```markdown
<!-- Comments are HTML comments, which can show up in the rendered output -->

[//]: # Non-standard way of commenting (avoiding rendering) is using using link to # (a valid URI) and blank line separation

[You can comment in the title box, and rely on :: to not find a reference elsewhere on the page (multiline support not guaranteed)]::

<!-- atx headers -->
# Heading 1
## Heading 2
###### Heading 6

<!-- Emphasis -->
*This text will be italic*
_This will also be italic_
**This text will be bold**
__This will also be bold__
_You **can** combine them_
~~This line is using a strikethrough~~
\*Use backslash for literals\*

<!-- Links -->

<http://example.com> - automatic link
[Example](<http://example.com>)
[DefineLink][]
[DefineLink]: http://example.com/later

<!-- Images -->
![Alt text](/path/to/img.jpg)

<!-- Lists -->
- unordered
- list

1. ordered
2. list

1.  Multiparagraph list

    Requires 4 spaces/tab

2.  Blockquoted items

    > Need to have the > indented as well
```

- <https://daringfireball.net/projects/markdown/syntax>

# Symbols and glyphs

## Accents

Modifier   | e.g. | HTML Entity | vim CK
---        | ---  | ---         | ---
Grave      | à    | &agrave;    | a!
Acute      | á    | &aacute;    | a'
Circumflex | â    | &acirc;     | a>
Tilde      | ã    | &atilde;    | '?
Umlaut     | ä    | &auml;      | a:
Cedil      | ç    | &ccedil;    | c,

## Set theory

desc                     | notation    | unicode | vim CK
---                      | ---         | ---     | ---
empty set                | \varnothing | 8709    | /0
set of natural numbers   | \mathbb{N}
set of integers          | \mathbb{Z}
set of rational numbers  | \mathbb{Q}
set of algebraic numbers | \mathbb{A}
set of real numbers      | \mathbb{R}
set of complex numbers   | \mathbb{C}
is member of             | \in         | 8712    | (-
is not member of         | \notin      | 8716
contains as member       | \ni         | 8715    | -)
is proper subset of      | \subset     | 8834    | (C
is subset or equal to    | \subseteq   | 8838    | (_
is proper superset of    | \supset     | 8835    | )C
is superset or equal to  | \supseteq   | 8839    | )_
set union                | \cup        | 8746    | )U
set intersection         | \cap        | 8745    | (U

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

- book
- article
- report
- letter
- slides

## ePub

- <https://www.tug.org/TUGboat/tb32-3/tb102rishi.pdf>
- <http://pandoc.org/>
- <https://tex2ebook.wordpress.com/>


## installing on Fedora

- texlive
- pdflatex
- texlive-xetex
- texlive-cm
- texlive-hyphen-base
- texlive-mfware

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

# mustache

- https://github.com/mustache/mustache

Characters escaped with standard double Mustache syntax: `& \ " < > '`

tag           | desc
---           | ---
{{name}}      | search a label in current context (HTML-escaped by default)
{{{name}}}    | unescaped variable
{{#names}}    | begin section for names (1 or more)
{{^names}}    | begin section for names (false or empty array)
{{/names}}    | end of a section
{{! comment}} | comment
{{> partial}} | partial (rendered at runtime)
{{=<% %>=}}   | set delimiters to something else (here erb delimiters)

If value is callable (ie function or lambda), text block is passed to callable unrendered and unexpanded.

http://mustache.github.io/mustache.5.html

# CSS

<http://www.cssbuttongenerator.com/>

## Flexbox

```
#thing {
  display: flex;
  justify-content: flex-start; /* horizontal alignment */
  align-items: flex-end; /* horizontal alignment */
  align-content: flex-end; /* determine spacing between the lines */
  order: 1; /* adjust the order */
  flex-wrap: wrap;
  flex-direction: row; /* direction of items - this flips justify-content and align-items */

  /* you can also use flex-flow instead of -wrap and -direction */
  flex-flow: row wrap;
}

.item {
  align-self: flex-end; /* move an item */
}
```

## Media types

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

## Mobile browsers

```html
<link rel='stylesheet' media='all' href='base.css'
<link rel='stylesheet' media='screen and (max-width: 320px)' href='mobile.css'>
```

## Use a custom font (CSS3)

```css
@font-face {
      font-family: MyFont;
      src: url(myfont.ttf), url(myfont.eot);
}
selector { font-family: MyFont; }
```

## Rounded corners (CSS3)

```css
border: width style color;
border-radius: radius;
```

## Box shadow (CSS 3)

```css
box-shadow: xpos ypos blur spread color, ...;
```

## Change an element's background image (CSS 3)

```css
selector {
  background-image: url(imagefile), ...;
}
```

## Translucent elements (CSS 3)

```css
selector {
  opacity: value;
  filter: alpha(opacity=value);
}
```

## Center an element

Give it a width and set the margin

```css
margin: 0 auto;
width: 939px;
```

## Create custom borders (CSS 3)

```css
border-image: source slice repeat;
border-width: width;
```

## Resources

* <https://flexboxfroggy.com/>
* <http://cssgridgarden.com/>
* <https://css-tricks.com/snippets/css/complete-guide-grid/>
# RSS

Defaults for getting feed link:

```
# Wordpress
http://wp.example.org/feed

# Blogger / blogspot
Atom 1.0: https://blogname.blogspot.com/feeds/posts/default
RSS 2.0: https://blogname.blogspot.com/feeds/posts/default?alt=rss
```

# reStructuredText

Syntax cheatsheet:

```rst
*italics*
**bold**
``fixed-space literal``
.. comment
```

```bash
# Convert rst to HTML
rst2html FILE ...
```

# Liquid templating system

https://shopify.github.io/liquid/

# EPUB files

<http://code.google.com/p/epubcheck/>

```bash
zip -X ../spm.epub mimetype css/style.css META-INF/container.xml book.ncx book.opf solplayaymar.xhtml
```

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

# EmotionML

https://www.w3.org/TR/emotionml/Overview.html

# GeoJSON

https://en.wikipedia.org/wiki/GeoJSON

# Virtual Human Markup Language

https://en.wikipedia.org/wiki/VHML

