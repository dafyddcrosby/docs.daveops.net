---
title: SOAP
---

Simple Object Access Protocol

## Use curl to send a request

```bash
curl -d @request.xml -H "Content-Type: application/soap+xml;charset=UTF-8" http://localhost:9090/thing
```
