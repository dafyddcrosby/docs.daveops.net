iOS web programming
===================
:date: 2016-03-04

Application Name
----------------
.. code-block:: html

 <title>WebApp</title>

Launcher icon (iOS 1.1.3+)
--------------------------
Rounded corners, no added shiny (iOS 4.2):
.. code-block:: html

 <link rel="apple-touch-icon-precomposed" href="apple-touch-icon-precomposed.png"/>

Added shiny:
.. code-block:: html

 <link rel="apple-touch-icon" href="touch-icon-iphone.png" />
 <link rel="apple-touch-icon" sizes="72x72" href="touch-icon-ipad.png" />
 <link rel="apple-touch-icon" sizes="114x114" href="touch-icon-iphone4.png" />

With no sizes the default is 57x57 px
(Multiple sizes iOS 4.2+)

Startup Image (iOS 3.0+)
------------------------
.. code-block:: html

 <link rel="apple-touch-startup-image" href="/startup.png">

320x460 px, portrait

Have standalone look
--------------------
.. code-block:: html

 <meta name="apple-mobile-web-app-capable" content="yes" />

Hide top status bar
-------------------
NB - must have standalone mode on.
.. code-block:: html

 <meta name="apple-mobile-web-app-status-bar-style" content="black" />

Prevent zooming
---------------
.. code-block:: html

 <meta name="viewport" content="initial-scale=1.0">
 <meta name="viewport" content="maximum-scale=1.0">
 <meta name="viewport" content="user-scalable=no">
 <meta name="viewport" content="width=device-width">

