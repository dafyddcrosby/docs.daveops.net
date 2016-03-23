C
=
:date: 2015-05-28
:tags: C

Resources
---------
* http://splint.org
* http://c-faq.com/
* https://www.securecoding.cert.org/confluence/display/c/SEI+CERT+C+Coding+Standard
* `GCC non-bugs <https://gcc.gnu.org/bugs/#nonbugs_c>`_

Misc Notes
----------
* Don't cast returned pointers from malloc. (void \*) should get automatically promoted to any pointer type, and casting just makes it likely you'll get it wrong.
* Free allocated memory when you are done with it, don't assume that OS will clean up your mess.

