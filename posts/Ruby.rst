Ruby
====
:tags: ruby
:date: 2015-02-14

TODO
----
- http://camping.rubyforge.org/

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

Syntax cheatsheet
-----------------
.. code-block:: ruby

 if val = 42
   #do stuff
 elsif val = 33
   #do stuff
 else
   #do stuff
 end
 
 begin
   # try to do this
 rescue Exception
   # oh crap
 else
   # whatever else
 ensure
   # do this no matter what
 end
