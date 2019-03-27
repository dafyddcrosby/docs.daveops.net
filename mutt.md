# Mutt
[Manual](http://www.mutt.org/doc/manual/)

Add header to messages
----------------------



 my_hdr X-Operating-System: TempleOS

Whitelist TLS fingerprint
-------------------------



 tls on
 tls_fingerprint <fingerprint>

Search
------

| command        | desc                                            |
|----------------|-------------------------------------------------|
| ~b EXPR        | Search for messages containing EXPR in the body |
| ~C EXPR        | To: or CC:                                      |
| ~f EXPR        | From:                                           |
| ~d [MIN]-[MAX] | messages with date-sent in date range           |
| ~l             | mailing lists                                   |
| ~s             | Subject:                                        |


