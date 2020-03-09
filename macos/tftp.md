---
title: MacOS TFTP
---

Folder is `/private/tftpboot`, but can be changed by adjusting
`/System/Library/LaunchDaemons/tftp.plist`

```bash
sudo launchctl load -F /System/Library/LaunchDaemons/tftp.plist
sudo launchctl start com.apple.tftpd
```
