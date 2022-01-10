---
title: Windows (Microsoft)
slug: windows
---

## Export certificates

On Win7

```cmd
# From Start Menu
certmgr.msc
# Find cert, right-click, point to All Tasks, click Export
```

## Recursive copy of files

On XP

```cmd
xcopy /E <src> <dest>
```

## Hosts file

c:\Windows\System32\Drivers\etc\hosts

## Writing a batch file

Save as FILENAME.bat

## Command Prompt

| command | description          |
|---------|----------------------|
| move    | move a file          |
| xcopy   | copy files + folders |

<https://ss64.com/nt/>

## Debuggers

* https://x64dbg.com/
* http://www.ollydbg.de/

## Cool links

* [Cleaning NTFS artifacts with
  FSCTL_CLEAN_VOLUME_METADATA](https://medium.com/@grzegorztworek/cleaning-ntfs-artifacts-with-fsctl-clean-volume-metadata-bd29afef290c)


## Sign a Windows binary in Linux with osslsigncode

```bash
osslsigncode -certs signing.cert -key signing.key -readpass passphrase_file -in unsigned_binary.exe -out signed_binary.exe -h sha256
```



## Extract files from WISE installer

```shell
installer.exe /x
```
