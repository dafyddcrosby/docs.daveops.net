---
title: curl
---

```bash
# Use client cert
curl --cert client.crt --key client.key URL

# Get the cookie
curl -c JAR ...
# Send the cookie
curl -b JAR ...

# Override referer
curl -e ...

# Send form data with curl
curl -F "key=val" ...

curl -F "uploadedfile=@FILE_TO_UPLOAD;filename=FILENAME_TO_SEND"
```
