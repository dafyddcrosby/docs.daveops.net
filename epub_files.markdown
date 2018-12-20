# EPUB files
<http://code.google.com/p/epubcheck/>



 zip -X ../spm.epub mimetype css/style.css META-INF/container.xml book.ncx book.opf solplayaymar.xhtml


* ERROR: /media/UDISK/spm.epub: mimetype entry missing or not the first in archive

  - It is what it sounds like. When you create your zip archive, the first one in has to be the mimetype file.

* ERROR: /media/UDISK/spm.epub: extra field length for first filename must be 0, but was 28

  - When you run zip, use the -X argument so that there's no timestamps, etc. These are the 'extra fields'



