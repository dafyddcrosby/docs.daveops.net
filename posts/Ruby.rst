Ruby
====
:tags: Ruby
:date: 2015-04-05

Running the profiler
---------------------

``ruby -rprofile script.rb``

get machine instructions
------------------------

``ruby --dump insns script.rb``

See how commands are parsed
---------------------------

``ruby --dump parsetree_with_comment script.rb``

Abort on thread errors
----------------------
.. code-block:: ruby

 Thread.abort_on_exception = true
 
 Thread.new do
   fail 'Cannot continue'
 end
 
 loop do
   sleep
 end

Gem source
----------
.. TODO - move to gem page
::

 # Add a source
 gem source -a http://example.com
 # Delete a source
 gem source -r http://example.com

Syntax cheatsheet
-----------------
.. TODO - flesh out
.. code-block:: ruby

 if val = 42
   #do stuff
 elsif val = 33
   #do stuff
 else
   #do stuff
 end

 # Ternary operator
 exp ? true : false

 begin
   # try to do this
 rescue Exception
   # oh crap
 else
   # whatever else
 ensure
   # do this no matter what
 end

 case thing
 when 3
   puts 'fizz'
 when 5
   puts 'buzz'
 else
   puts thing
 end
