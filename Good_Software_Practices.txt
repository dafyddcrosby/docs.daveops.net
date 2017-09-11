Good Software Practices
=======================
:date: 2016-10-11
:tags: advice 

Don't reinvent the wheel
------------------------
The world does not need another XML parser. Use what's already been written and
tested. Often what you think sucks only needs a few patches to be adequate.

Code without a written plan is just asking for trouble
------------------------------------------------------
When I hear someone brag that they've got the whole program in their head, I
know it will turn out like garbage. Exceptions to this are rare. There's always
something found in the process of writing the idea down that makes the program
better, and it also becomes a form of documentation.

Don't repeat yourself
---------------------
Try to remove duplication in code by refactoring, so that it's easier to extend
and debug the program later on.

Your build system should *never* be more than 1 step
----------------------------------------------------
``configure && make && make install`` should be the most complicated build
setup you have.

Get it to one-step-to-build - instant feedback is awesome, and the time saved
will pay off when "I need a build for the customer in 5 minutes!" For that
matter, have automated testing that the master always builds clean.

Keep it simple
--------------
I once replaced an elaborate Java-based build system with 2 shell scripts. Mine
worked faster, better, and was simpler to read.

Prototypes!
-----------
A simple prototype done in a few hours can save a week's worth of speculation, and
often you can plunder the prototype for technical insights and even code.

Business rules and logic don't mix well
---------------------------------------
Business rules will change. It's in your best interest to make
them easily configurable (read: not compiled into the binary). Even better,
make it simple enough that Bob in accounting can manage the rules.

Code can be the best documentation
----------------------------------
Descriptive variables and functions are better than comments. When you actually
need non-code documentation, make sure that the docs are automatically
pulled together from the code (a la doxygen, etc.).

Keep LOC down to keep bugs down
-------------------------------
.. TODO - What was the study? COMODO?

Code Complete said that the industry average is 15-50 bugs per 1000 lines of
code. By reducing the lines of code and keeping functions simple, your life
will be much easier.

ALWAYS validate your inputs
---------------------------
Because 13-year-olds know enough to fuzz your text box. SQL injections are one
of the most common bugs.

Be correct first, then be fast.
-------------------------------
Debugging a fast program that's segfaulting is slow work...

Always review your releases
---------------------------
Code audits are a cheap way of ensuring quality (though they may not seem so at
the time). Lost customers and lost time to troubleshooting problems are
expensive. As well, keep metrics on defects from release to release, so that
you can adjust practices as necessary. Use the "Infinite Hows" technique to
examine release/deployment issues - deployment issues should be dealt with
once, *permanently*.

1 hour of inspection can save up to 100 hours of related work
-------------------------------------------------------------
IBM study found "each hour of inspection prevented about 100 hours of related
work (testing and defect correction)"

Testing - it's worth the effort
-------------------------------
When you learn a new language, learn how to test its code as early as possible.
Time spent writing tests is rarely wasted. When you use test cases, you can
refactor without (much) fear. When you need to debug quickly, having tests make
a huge difference.

It's easier to be secure when you bake it in at the start
---------------------------------------------------------
It sucks to break backwards compatibility, or worse, get caught with your pants
down, because *someone* didn't bother with security when they hacked the
program together.

Stop with 'good enough'
-----------------------
To keep from going crazy and spending too much time on irrelevant crap, set
specific and quantitative goals for your project at the start. When you've met
a goal, move on to the next one.

Source control everything
-------------------------
Seriously. Learn git. It'll take you just a couple hours, and you'll make it
back in no time at all.

Don't marry your framework
--------------------------
A framework is a great way to ensure you don't reinvent the wheel, but if
possible keep your business logic external to it. That way if something sexier
comes along you're not fully invested in the old crap. Decoupling can also mean
faster tests.

Practice defensive programming
------------------------------
Use assertions to ensure the state is correct, and use tests to simulate faults.

A bad process will lead to lots of preventable headaches
--------------------------------------------------------
Take an active role in perfecting the process wherever you work.

Nail the basic process to avoid certain failure
-----------------------------------------------
- Do an analysis, capture the requirements, and create a design document
- Review the design - catch errors before the coding stage
- Code review
- Use automated code analysis
- Unit and component tests to stress-test and ensure edge cases are properly handled

Velocity is a good measure of project health
--------------------------------------------
Good code allows you to fix bugs and add features quickly.

Keep a bug database
-------------------
Even something as simple as BUGS.txt eliminates bus factor, and makes it
easier to approach a project months later.
