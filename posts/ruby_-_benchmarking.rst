Ruby - Benchmarking
===================
:date: 2015-06-10

.. code-block:: ruby

   require 'benchmark'

   n = 50000

   # this gives you a Benchmark::Tms object
   tms = Benchmark.measure { for i in 1..n; a = "1"; end }

   # Returns [@label, @utime, @stime, @cutime, @cstime, @real]
   tms.to_a
