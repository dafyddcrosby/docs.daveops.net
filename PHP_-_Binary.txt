PHP - Binary
------------
:tags: php


Read a binary file
==============================
.. code-block:: php

 <?php
 $handle = fopen('/path/to/file', 'rb');
 $contents = fread($handle, filesize($filename));
 fclose($handle);
 ?>

Binary craziness
==============================
.. code-block:: php

 <?php
 bindec();
 decbin();
 ?>

Playing with endianness
==============================
.. code-block:: php

 <?php
 // For changing endianness, just use strrev()
 // %016b for 16-bit, %032b for 32-bit
 strrev(sprintf("%016b", $int));
 ?>

Unpacking binary file formats
==============================
.. code-block:: php

 <?php
 // For parsing binary file formats,
 // use pack/unpack
 function get_gif_header($image_file)
 {
  
     /* Open the image file in binary mode */
     if(!$fp = fopen ($image_file, 'rb')) return 0;
  
     /* Read 20 bytes from the top of the file */
     if(!$data = fread ($fp, 20)) return 0;
  
     /* Create a format specifier */
     $header_format = 
             'A6Version/' . # Get the first 6 bytes
             'C2Width/' .   # Get the next 2 bytes
             'C2Height/' .  # Get the next 2 bytes
             'C1Flag/' .    # Get the next 1 byte
             '@11/' .       # Jump to the 12th byte
             'C1Aspect';    # Get the next 1 byte
 
     /* Unpack the header data */
     $header = unpack ($header_format, $data);
  
     $ver = $header['Version'];
  
     if($ver == 'GIF87a' || $ver == 'GIF89a') {
         return $header;
     } else {
         return 0;
     }
 }
  
 /* Run our example */
 print_r(get_gif_header("aboutus.gif"));
 
 /*
 Array
 (
     [Version] => GIF89a
     [Width1] => 97
     [Width2] => 0
     [Height1] => 33
     [Height2] => 0
     [Flag] => 247
     [Aspect] => 0
 )
 */
 ?>
