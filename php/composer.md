---
title: Composer
---

[Composer homepage](https://getcomposer.org/)

## Require a package
```bash
composer require repo/package
```

Will drop `composer.json` and `composer.lock`

## Load vendor directory
```php
<?php
require __DIR__ . '/vendor/autoload.php';
```
