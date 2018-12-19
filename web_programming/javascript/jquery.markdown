# jQuery
No-conflict mode
----------------

.. code-block:: javascript

 var $j = jQuery.noConflict();

Append a node to the DOM
------------------------

.. code-block:: javascript

 $('#thing').append('<p>blerg</p>');'</p>')

Get first element of several
----------------------------


CSS pseudo-selectors   $(".stuff li:first");

DOM traversal   $(".stuff").first();

Searches for the closest ancestor that matches
----------------------------------------------


.closest()

Objects
-------


Use $(this) instead of this - it's a jQuery object

Get custom data attributes from the DOM
---------------------------------------

date(name)

Get direct children from an element
-----------------------------------


$("ul").children("li");

Get parent element of a node
----------------------------


$("#thing").parent();

