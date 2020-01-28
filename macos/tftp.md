---
title: MacOS TFTP
---

Folder is /private/tftpboot

```bash
sudo launchctl load -F /System/Library/LaunchDaemons/tftp.plist
sudo launchctl start com.apple.tftpd
```
