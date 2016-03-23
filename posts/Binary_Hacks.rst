Binary Hacks
============


Determine if integer is odd
---------------------------
.. code-block:: c

 if ((x & 1) == 0) {
   //x is even
 } else {
   //x is odd
 }

determine if nth bit is set
---------------------------
.. code-block:: c

 if (x & (1<<n)) {
   // n-th bit is set
 } else {
   //  n-th bit is not set
 }

Set the nth bit
---------------
.. code-block:: c
 
 y = x | (1<<n)


Unset the nth bit
-----------------
.. code-block:: c

 y = x & ~(1<<n)

Toggle the nth bit
------------------
.. code-block:: c

 y = x ^ (1<<n)


