C
-


Resources
==============================
* http://splint.org
* http://c-faq.com/

Misc Notes
==============================
* Don't cast returned pointers from malloc. (void \*) should get automatically promoted to any pointer type, and casting just makes it likely you'll get it wrong.
* Free allocated memory when you are done with it, don't assume that OS will clean up your mess.

