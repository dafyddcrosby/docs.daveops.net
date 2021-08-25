---
title: Java
---

## Types

| type    | note                                            |
|---------|-------------------------------------------------|
| byte    | 8-bit signed two's complement integer           |
| short   | 16-bit signed two's complement integer          |
| int     | 32-bit signed two's complement integer          |
| long    | 64-bit two's complement integer                 |
| float   | single-precision 32-bit IEEE 754 floating point |
| double  | double-precision 64-bit IEEE 754 floating point |
| boolean | the size isn't precisely defined                |

## Numeric literals

You can use underscore characters in SE7+
example: ``long hexBytes = 0xFF_CC_DA_B5;``


## AES intrinsics

Requires Java 8 and Intel 2010+ Westmere

	-XX:+UseAES -XX:+UseAESIntrinsics

## Zero copy

Avoid copying the file data across user/kernel boundary, instead have the kernel put the file in a buffer and use DMA to pass the data directly.

<https://www.ibm.com/developerworks/linux/library/j-zerocopy/>

