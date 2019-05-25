---
title: PHP - Barcodes
---

## Get a UPC check digit

```php	
<?php
function get_ean_checkdigit($barcode){
        $sum = 0;
        for($i=(strlen($barcode));$i>0;$i--){
       	 $sum += (($i % 2) * 2 + 1 ) * substr($barcode,$i-1,1);
        }
        return (10 - ($sum % 10));
}
?>
```

## Create a barcode with Image_Barcode2 (PEAR)

```php	
<?php
require_once 'Image/Barcode2.php';
$bc = new Image_Barcode2;
$bc->draw($_GET['bctext'], "int25", "png");
?>
```
