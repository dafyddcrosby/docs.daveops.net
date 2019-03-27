---
title: Lokkit
tags: ["Red Hat", "firewalls"]
---

## Get list of services

 lokkit --list-services

## Open port

 lokkit --selinux=disabled --update --enabled -p [port]:[tcp|udp]
 # or to open a service
 lokkit -s [service]

