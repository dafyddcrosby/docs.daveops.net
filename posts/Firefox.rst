Firefox
=======
:date: 2015-04-05

Debug logging
-------------
https://developer.mozilla.org/en-US/docs/Mozilla/Debugging/HTTP_logging

::

 export NSPR_LOG_MODULES=timestamp,nsHttp:5,nsSocketTransport:5,nsStreamPump:5,nsHostResolver:5
 export NSPR_LOG_FILE=~/Desktop/log.txt
 ./firefox
