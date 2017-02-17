Site Reliability Engineering (book)
===================================
:date: 2017-02-16
:tags: books

The book: https://landing.google.com/sre/book/

Foreward
--------
::

  The tribal nature of IT culture often entrenches practitioners in dogmatic positions that hold the industry back.

Just a *touch* self-serving. But I guess if Google pays you to write a book, you kind of have to glorify it just a smidge.

Preface
-------

::

   As Margaret says, "a thorough understanding of how to operate the systems was not enough to prevent human errors," and the change request to add error detection and recovery software to the prelaunch program P01 was approved shortly afterwards. 


Introduction
------------

"Hope is not a strategy." -Traditional SRE saying

Historical sysadmin approach - run the service, respond to events, ensure stability. The want of a sysadmin can run counter to dev, who want to push features.

So their big thing is 'write code instead of doing stuff manually.' Duh. (says the dev-turned-op)

Google SRE is an eng with a few extra skills, usually UNIX + L1-L3 networking

Google SRE's also write application code, to prevent dev/ops split

The SRE Team is responsible for:

- availability
- latency
- performance
- efficiency
- change management
- monitoring
- emergency response
- capacity planning

Misc
----

`Dan Luu's notes <https://danluu.com/google-sre-book/>`_

