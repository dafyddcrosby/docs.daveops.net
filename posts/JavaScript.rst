JavaScript
----------


Sweet libraries
==============================
http://jquery.com
http://code.google.com/p/iui/

Classes
==============================
.. code-block:: javascript
 function Building(x,y,z) {
   this.x = x;
   this.y = y;
   this.z = z;
 }
 
 Building.prototype.area = function () {
   return x * y * z;
 }
 
 var house = Building(20,20,10);
