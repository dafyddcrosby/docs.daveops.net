---
title: OpenLDAP
---

## ldapsearch

flag           | desc
---            | ---
-LLL           | LDIF, with no comments or version
-Z             | Use StartTLS
-H $HOST       | $HOST is server
-b $SEARCHBASE | $SEARCHBASE is start point for search
-x             | Simple auth (not SASL)
-W             | Prompt for simple auth
-D $BINDDN     | Use $BINDDN to bind to LDAP directory

## Links

* <http://www.openldap.org/>
* [Admin Guide (2.4)](http://www.openldap.org/doc/admin24/guide.html)

## RFCs
* 2251
* 4510-4519
