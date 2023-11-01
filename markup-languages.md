
# Markup languages

# HTML


## Entities

| Tag        | Name                       | Description                                                                                                                                                                                                                                                                 |
|---------- |-------------------------- |--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| <!-->      | comment                    | Used for commenting out code                                                                                                                                                                                                                                                |
| <html>     | HTML                       | Represents the root (top-level element) of an HTML document, so it is also referred to as the root element. All other elements must be descendants of this element.                                                                                                         |
| <head>     | head                       | contains machine-readable information (metadata) about the document, like its title, scripts, and style sheets.                                                                                                                                                             |
| <meta>     | meta                       | represents metadata that cannot be represented by other HTML meta-related elements, like base, link, script, style or title.                                                                                                                                                |
| <style>    | style                      | contains style information for a document, or part of a document.                                                                                                                                                                                                           |
| <title>    | title                      | defines the document's title that is shown in a browser's title bar or a page's tab                                                                                                                                                                                         |
| <body>     | body                       | represents the content of an HTML document. There can be only one <body> element in a document.                                                                                                                                                                             |
| <h1>-<h6>  | h1-h6                      | represent six levels of section headings. <h1> is the highest section level and <h6> is the lowest.                                                                                                                                                                         |
| <nav>      | nav                        | represents a section of a page whose purpose is to provide navigation links, either within the current document or to other documents. Common examples of navigation sections are menus, tables of contents, and indexes.                                                   |
| <div>      | content division           | the generic container for flow content. It has no effect on the content or layout until styled in some way using CSS (e.g. styling is directly applied to it, or some kind of layout model like Flexbox is applied to its parent element).                                  |
| <hr>       | horizontal rule            | represents a thematic break between paragraph-level elements: for example, a change of scene in a story, or a shift of topic within a section.                                                                                                                              |
| <li>       | list item                  | used to represent an item in a list                                                                                                                                                                                                                                         |
| <ol>       | ordered list               | represents an ordered list of items — typically rendered as a numbered list.                                                                                                                                                                                                |
| <ul>       | unordered list             | represents an unordered list of items, typically rendered as a bulleted list.                                                                                                                                                                                               |
| <p>        | paragraph                  | represents a paragraph                                                                                                                                                                                                                                                      |
| <pre>      | preformatted text          | represents preformatted text which is to be presented exactly as written in the HTML file                                                                                                                                                                                   |
| <a>        | anchor                     | with its href attribute, creates a hyperlink to web pages, files, email addresses, locations in the same page, or anything else a URL can address                                                                                                                           |
| <b>        | Bring attention to         | used to draw the reader's attention to the element's contents, which are not otherwise granted special importance                                                                                                                                                           |
| <br>       | line break                 | produces a line break in text (carriage-return). It is useful for writing a poem or an address, where the division of lines is significant.                                                                                                                                 |
| <code>     | code                       | displays its contents styled in a fashion intended to indicate that the text is a short fragment of computer code                                                                                                                                                           |
| <i>        | idiomatic                  | represents a range of text that is set off from the normal text for some reason, such as idiomatic text, technical terms, taxonomical designations, among others                                                                                                            |
| <kbd>      | keyboard input             | represents a span of inline text denoting textual user input from a keyboard, voice input, or any other text entry device                                                                                                                                                   |
| <s>        | strikethrough              | renders text with a strikethrough, or a line through it. Use the <s> element to represent things that are no longer relevant or no longer accurate. However, <s> is not appropriate when indicating document edits; for that, use the del and ins elements, as appropriate. |
| <audio>    | audio                      | used to embed sound content in documents. It may contain one or more audio sources, represented using the src attribute or the source element: the browser will choose the most suitable one. It can also be the destination for streamed media, using a MediaStream.       |
| <img>      | image                      | embeds an image into the document                                                                                                                                                                                                                                           |
| <video>    | video                      | embeds a media player which supports video playback into the document. You can use <video> for audio content as well, but the audio element may provide a more appropriate user experience.                                                                                 |
| <iframe>   | inline frame               | represents a nested browsing context, embedding another HTML page into the current one                                                                                                                                                                                      |
| <script>   | script                     | used to embed executable code or data; this is typically used to embed or refer to JavaScript code.                                                                                                                                                                         |
| <table>    | table                      | represents tabular data — that is, information presented in a two-dimensional table comprised of rows and columns of cells containing data.                                                                                                                                 |
| <th>       | th                         | defines a cell as header of a group of table cells. The exact nature of this group is defined by the scope and headers attributes.                                                                                                                                          |
| <tr>       | table row                  | defines a row of cells in a table. The row's cells can then be established using a mix of td (data cell) and th (header cell) elements.                                                                                                                                     |
| <td>       | table data cell            | defines a cell of a table that contains data. It participates in the table model.                                                                                                                                                                                           |
| <form>     | form                       | represents a document section containing interactive controls for submitting information                                                                                                                                                                                    |
| <textarea> | text area                  | represents a multi-line plain-text editing control, useful when you want to allow users to enter a sizeable amount of free-form text, for example a comment on a review or feedback form.                                                                                   |
| <blink>    | blink                      | Deprecated. Causes the enclosed text to flash slowly.                                                                                                                                                                                                                       |
| <bgsound>  | Background sound           | Deprecated, IE only. Sets up a sound file to play in the background while the page is used; use audio instead.                                                                                                                                                              |
| <frameset> | frameset                   | Deprecated. Used to hold frames                                                                                                                                                                                                                                             |
| <frame>    | frame                      | an HTML element which defines a particular area in which another HTML document can be displayed. A frame should be used within a frameset                                                                                                                                   |
| <marquee>  | scrolling text             | Deprecated. Used to insert a scrolling area of text. You can control what happens when the text reaches the edges of its content area using its attributes.                                                                                                                 |
| <noframes> | No frames / Frame fallback | Provides content to be presented in browsers that don't support (or have disabled support for) the frame element                                                                                                                                                            |


## Application cache

- <http://www.html5rocks.com/en/tutorials/appcache/beginner/>


## Preloading

- <https://instant.page/>
- <https://github.com/instantpage/instant.page>

```
<link rel="prefetch" href="_url_">
```


## Shim for old IE browsers

```
<!--[if lt IE 9]>
<script src="<http://html5shim.googlecode.com/svn/trunk/html5.js>"></script>
<![endif]-->
```


## Forms

```
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

```
<input ... required>
```


## Code samples

```
<code>print 'hello world'</code> prints <samp>hello world</samp>
```


## Canvas (HTML 5)

```
<canvas id='id' height='600' width='800'>fallback</canvas>
<script type='text/javascript'>
  var canvas = document.getElementById(id)
  if (canvas.getContext) {
    var context = canvas.getContext(type)
  }
</script>
```


## Fixed-meter bar (HTML 5)

```
<meter value='num' min='num' max='num' optimum='num'>
fallback display
</meter>
```


## Progress bar (HTML 5)

```
<progress value='num' max='num'>
fallback display
</progress>
```

Use JavaScript to move it around


## Autofocus input (HTML 5)

```
<input ... autofocus>
```


## Patterned input (HTML 5)

```
<!-- Enter 15 digits -->
<input type='text' pattern='[0-9]{15}'>
```


## Dropdown list for text input (HTML 5)

```
<input type='text' ... list='listid'>
<datalist id='id'>
  <option label='label1' value='value1'>
  <option label='label2' value='value2'>
</datalist>
```


## Editable content

```
<p contenteditable="true">
```


## Link for phone numbers (mobile)

International calling code is required

```
<a href="tel:+14035555555">403-555-5555</a>

<!-- To handle an extension -->
<a href="tel:+14035555555p23">403-555-5555 ext. 23</a>

<!-- To use a fax line -->
<a href="fax:+14035555555">403-555-5555</a>
```


## Doctype (HTML 5)

```
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

```
<article>
  <hgroup>
 <h1>Title</h1>
 <h2>Byline</h2>
  </hgroup>
</article>
```


## Good meta tags to have

- <http://commoncrawl.org/>

```
<head>
<title>up to 70 characters of relevant text</title>
<meta name="description" content="155 characters of message matching text">
</head>
```

- [OpenGraph](web-programming.md)
- [Web Services:Twitter](web-programming.md)

<http://www.iacquire.com/blog/18-meta-tags-every-webpage-should-have-in-2013>


# HAML

<https://haml.info>

```shell
gem install haml
```

```
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

```
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

| Modifier   | e.g. | HTML Entity | vim CK |
|---------- |---- |----------- |------ |
| Grave      | à    | &agrave;    | a!     |
| Acute      | á    | &aacute;    | a'     |
| Circumflex | â    | &acirc;     | a>     |
| Tilde      | ã    | &atilde;    | '?     |
| Umlaut     | ä    | &auml;      | a:     |
| Cedil      | ç    | &ccedil;    | c,     |


## Set theory

| desc                     | notation    | unicode | vim CK |
|------------------------ |----------- |------- |------ |
| empty set                | \varnothing | 8709    | /0     |
| set of natural numbers   | \mathbb{N}  |         |        |
| set of integers          | \mathbb{Z}  |         |        |
| set of rational numbers  | \mathbb{Q}  |         |        |
| set of algebraic numbers | \mathbb{A}  |         |        |
| set of real numbers      | \mathbb{R}  |         |        |
| set of complex numbers   | \mathbb{C}  |         |        |
| is member of             | &isin;      | 8712    | (-     |
| is not member of         | &notin;     | 8716    |        |
| contains as member       | &ni;        | 8715    | -)     |
| is proper subset of      | &sub;       | 8834    | (C     |
| is subset or equal to    | \subseteq   | 8838    | (\_    |
| is proper superset of    | &sup;       | 8835    | )C     |
| is superset or equal to  | \supseteq   | 8839    | )\_    |
| set union                | &cup;       | 8746    | )U     |
| set intersection         | &cap;       | 8745    | (U     |


# xml


## Prettify

```shell
tidy -xml -i -m [file]
```


## XMLLint

```shell
# Check XML file is well-formed
xmllint --noout $FILE

# Check XML file against local DTD file
xmllint --noout --dtdvalid ./local.dtd $FILE
```

- <http://www.xmlsoft.org/>


# LaTeX


## Cheatsheet

| Description                    | Command         | LaTeX                 |
|------------------------------ |--------------- |--------------------- |
| Show a fraction                | \frac{1}{2}     | [$]\frac{1}{2}[/$]    |
| Show multiplication            | &times;         | [$]\times[/$]         |
| Show a square root             | \sqrt{abc}      | [$]\sqrt{abc}[/$]     |
| Infinity symbol                | &infin;         | [$]\infty[/$]         |
| Plus-minus symbol              | &plusmn;        | [$]\pm[/$]            |
| Pi symbol                      | &pi;            | [$]\pi[/$]            |
| Sum symbol                     | &sum;           | [$]\sum[/$]           |
| Show x to the nth root         | \sqrt[n]{x}     | [$]\sqrt[n]{x}[/$]    |
| Create an exponent             | x<sup>y-1</sup> | [$]x^{y-1}[/$]        |
| Symbol to show proportionality | &prop;          | [$]\propto[/$]        |
| Approximate symbol             | &asymp;         | [$]\approx[/$]        |
| LaTeX logo                     | \LaTeX          | [latex]\LaTeX[/latex] |


## adding quotes

```
\begin{quote}
...
\end{quote}
```


## Cheatsheet

```
\documentclass{book}
\begin{document}
\maketitle
\end{document}
```


## bibtex

```
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

```
\usepackage{draftwatermark}
\SetWatermarkText{Draft}
```


## Tables

```
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

- <https://github.com/mustache/mustache>

Characters escaped with standard double Mustache syntax: `& \ " < > '`

{% raw %}

```
| tag           | desc                                                        |
|---------------+-------------------------------------------------------------|
| {{name}}      | search a label in current context (HTML-escaped by default) |
| {{{name}}}    | unescaped variable                                          |
| {{#names}}    | begin section for names (1 or more)                         |
| {{^names}}    | begin section for names (false or empty array)              |
| {{/names}}    | end of a section                                            |
| {{! comment}} | comment                                                     |
| {{> partial}} | partial (rendered at runtime)                               |
| {{=<% %>=}}   | set delimiters to something else (here erb delimiters)      |
```

{% endraw %}

If value is callable (ie function or lambda), text block is passed to callable unrendered and unexpanded.

<http://mustache.github.io/mustache.5.html>


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

- all
- braille
- embossed
- handheld
- print
- projection
- screen
- speech
- tty
- tv


## Mobile browsers

```
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

- <https://flexboxfroggy.com/>
- <http://cssgridgarden.com/>
- <https://css-tricks.com/snippets/css/complete-guide-grid/>


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

```
*italics*
**bold**
``fixed-space literal``
.. comment
```

```shell
# Convert rst to HTML
rst2html FILE ...
```


# Liquid templating system

<https://shopify.github.io/liquid/>


# EPUB files

<http://code.google.com/p/epubcheck/>

```shell
zip -X ../spm.epub mimetype css/style.css META-INF/container.xml book.ncx book.opf solplayaymar.xhtml
```

- ERROR: /media/UDISK/spm.epub: mimetype entry missing or not the first in archive
    - It is what it sounds like. When you create your zip archive, the first one in has to be the mimetype file.

- ERROR: /media/UDISK/spm.epub: extra field length for first filename must be 0, but was 28
    - When you run zip, use the -X argument so that there's no timestamps, etc. These are the 'extra fields'


# ASN.1 (Abstract Syntax Notation One)

<https://en.wikipedia.org/wiki/Abstract_Syntax_Notation_One>

Visually similar to Augmented Backus-Naur form, but is for data structures, not syntax.


# SAML

Security Assertion Markup Language <https://samltest.id/>


# EmotionML

<https://www.w3.org/TR/emotionml/Overview.html>


# GeoJSON

<https://en.wikipedia.org/wiki/GeoJSON>


# Virtual Human Markup Language

<https://en.wikipedia.org/wiki/VHML>


# Mermaid

<https://mermaid-js.github.io/mermaid/>

```
<div class="mermaid">
journey
    title My working day
    section Go to work
      Make tea: 5: Me
      Go upstairs: 3: Me
      Do work: 1: Me, Cat
    section Go home
      Go downstairs: 5: Me
      Sit down: 5: Me
</div>
```

```
gantt
    title A Gantt Diagram
    dateFormat  YYYY-MM-DD
    section Section
    A task           :a1, 2014-01-01, 30d
    Another task     :after a1  , 20d
    section Another
    Task in sec      :2014-01-12  , 12d
    another task      : 24d
```