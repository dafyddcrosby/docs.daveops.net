---
title: Kerberos
---

## Terminology

### Key Distribution Center 

Holds the key database, Authentication Server, Ticket Granting Server. This
might all be handled by one service.  It's super sensitive, treat is as such.
Since it handles authentication, at least one KDC should be running at all times

<!--
K:TDG says there's no standard synchronization mechanism
-->

### Authentication Server

Issues the Ticket Granting Ticket. Only the correct password decrypts the TGT.
Once the TGT is decrypted, it can be used to request individual service tickets.

The strength of the ticket is the strength of the password! (ie rotate PW
regularly, use hard passwords).

### Ticket Granting Server

Issues individual service tickets.

<!--
key version number (kvno) is important for services
-->

### Realm

A sort of namespace for principals (ie users, services).

## KRB4 vs. KRB5

Kerberos 4 uses 56-bit DES (yikes!)

Kerberos 5 has credential forwarding

## Resources
* *Kerberos: The Definitive Guide* by Jason Garman

<!--
Roger Needham / Michael Schroeder - "Using encryption for authentication in
large networks of computers"
  - Thought MITM was an "extreme view"
-->
