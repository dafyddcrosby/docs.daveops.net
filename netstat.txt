netstat
=======
:date: 2016-09-02

+-------------+-----------------------------------------------------------------------------------------------------------+
|State        | Description                                                                                               |
+=============+===========================================================================================================+
| LISTEN      | accepting connections                                                                                     |
+-------------+-----------------------------------------------------------------------------------------------------------+
| ESTABLISHED | connection up and passing data                                                                            |
+-------------+-----------------------------------------------------------------------------------------------------------+
| SYN_SENT    | TCP; session has been requested by us; waiting for reply from remote endpoint                             |
+-------------+-----------------------------------------------------------------------------------------------------------+
| SYN_RECV    | TCP; session has been requested by a remote endpoint for a socket on which we were listening              |
+-------------+-----------------------------------------------------------------------------------------------------------+
| LAST_ACK    | TCP; our socket is closed; remote endpoint has also shut down; we are waiting for a final acknowledgement |
+-------------+-----------------------------------------------------------------------------------------------------------+
| CLOSE_WAIT  | TCP; remote endpoint has shut down; the kernel is waiting for the application to close the socket         |
+-------------+-----------------------------------------------------------------------------------------------------------+
| TIME_WAIT   | TCP; socket is waiting after closing for any packets left on the network                                  |
+-------------+-----------------------------------------------------------------------------------------------------------+
| CLOSING TCP | our socket is shut down; remote endpoint is shut down; not all data has been sent                         |
+-------------+-----------------------------------------------------------------------------------------------------------+
| FIN_WAIT1   | TCP; our socket has closed; we are in the process of tearing down the connection                          |
+-------------+-----------------------------------------------------------------------------------------------------------+
| FIN_WAIT2   | TCP; the connection has been closed; our socket is waiting for the remote endpoint to shut down           |
+-------------+-----------------------------------------------------------------------------------------------------------+
