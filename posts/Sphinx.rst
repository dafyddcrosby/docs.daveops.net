Sphinx
------
:tags: Python

Create new project
==============================
::

 sphinx-quickstart

Removing extra blank pages from PDF
===================================
.. code-block:: python

 latex_elements = {
   'classoptions': ',openany,oneside',
   'babel': '\\usepackage[english]{babel}',
 }
