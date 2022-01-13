# macOS

## Keyboard shortcuts

Shortcut              | Desc
---                   | ---
cmd + option + escape | bring up 'force quit applications' menu
cmd + shift + 3       | take screenshot of all screens
cmd + shift + 4       | take a partial screenshot
cmd + shift + eject   | lock screen


## Open application bundle

```bash
open -a APPLICATION
```

## kernel extensions
```bash
# list kernel extensions
kextstat -l
# unload kernel extensions
kextunload -b <id>
```

## Update software
```bash
softwareupdate -h
```

## Search help
apple key + ? , search for the help menu

## Increase maxfiles for session
```bash
ulimit -n 4096
```

## Remove launch agents
```bash
# get launch list
launchctl list
# remove item
launchctl remove <svc>
```

## Use particular nameservers for a domain

Create hosts-style file in `/etc/resolver/<domain>`

See `man 5 resolver`

## Change password on encrypted disk
```bash
hdiutil chpass /path/to/disk
```

## Burn ISO
```bash
hdiutil burn <image>
```

## List disks
```bash
diskutil list
```

## Get linked libraries/object files
```bash
# List shared libraries
otool -L <executable>
```
<!---
TODO look more into otool's operations
-->

## Create a RAM disk
```bash
# Replace XXXXX with MB * 2048 (eg a 4 gig is 8388608 (4096 * 2048))
diskutil erasevolume HFS+ 'RAM Disk' `hdiutil attach -nomount <ram://XXXXX>`
```

## Boot Options

keypress             | action
---                  | ---
Cmd + r              | Recovery Mode
Option + Cmd + r     | Upgrade to latest compatible macOS
Cmd + v              | Verbose Mode
Cmd + s              | Single-user Mode
Shift                | Safe Mode
D                    | Apple Diagnostics / Hardware Test
C                    | Boot removable device
N                    | Boot from network
Option               | Startup Manager
Cmd + Option + P + R | Reset NVRAM

## Wireless diagnostics

All the neat tools for diagnosing busy channels, noise, etc. are in the 'Window' tab

```bash
/System/Library/CoreServices/Applications/Wireless\ Diagnostics.app/Contents/MacOS/Wireless\ Diagnostics
```

```bash
# List APs
/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -s
```

## Virtual Memory Stats

```bash
vmstat
```

## Power management

``man pmset``

Use ``caffeinate`` to prevent the system from sleeping

## Power report

A bunch of dtrace under the hood

```bash
/usr/bin/power_report.sh
```

## System/Application defaults

/Library/Preferences and ~/Library/Preferences

``man defaults``

## launch services database

```bash
# dump database
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -dump

#Remove Open With entries
lsregister -kill -r -domain local -domain system -domain user
```

## System Preference Panes
PreferencePanes framework

/System/Library/PreferencePanes

## System config
scutil - system config utility
- help command in the shell has lots of goodies
- use ``n.add KEY`` + ``n.watch`` to get notified of config changes

## Look for memory leaks

leaks(1)

## System Integrity Protection

csrutil(1)

### Installing fonts

copy to ~/Library/Fonts

## Get a call stack of a running process

```bash
sample PID -file OUTPUT
```

## Time Machine snapshots

```bash
tmutil listlocalsnapshotdates
tmutil deletelocalsnapshots DATE
```

## Do Bad Things to your Mac

Note: Voids your warranty, haven't tried it myself

```bash
# Disable GateKeeper
sudo spctl --master-disable

# Disable Library Validation
sudo defaults write /Library/Preferences/com.apple.security.libraryvalidation.plist DisableLibraryValidation -bool true

# (From Recovery Mode) Disable System Integrity Protection
csrutil disable

# (From Recovery Mode) Disable Apple Mobile File Integrity
nvram boot-args="amfi_get_out_of_my_way=1"
```

## Type accent marks

- á — Option + e, a
- Á — Option + e, Shift + a
- é — Option + e, e
- É — Option + e, Shift + e
- í — Option + e, i
- Í — Option + e, Shift + i
- ñ — Option + n, n
- Ñ — Option + n, Shift + n
- ó — Option + e, o
- Ó — Option + e, Shift + o
- ú — Option + e, u
- Ú — Option + e, Shift + u
- ü — Option + u, u
- Ü — Option + u, Shift + u
- ¿ — Shift + Option + ?
- ¡ — Option + 1
- « — Option + \
- » — Shift + Option + \
- quotation dash (—) — Shift + Option + -

## Nice graphical diff

```bash
opendiff foo bar
```

## Custom keyboard layouts

`~/Library/Keyboard Layouts`

* <https://web.archive.org/web/20140812170917/https://developer.apple.com/library/mac/documentation/TextFonts/Reference/TextInputSourcesReference/Reference/reference.html>
* <https://web.archive.org/web/20160318180309/https://developer.apple.com/library/mac/technotes/tn2056/_index.html>

## Links

* [Wiping Intel-based macs](https://support.apple.com/en-us/HT208496)
* <https://opensource.apple.com/>
* <https://github.com/herrbischoff/awesome-osx-command-line>
* <https://www.raywenderlich.com/151741/macos-development-beginners-part-1>



# macOS Catalina

Notes: 32-bit support is dropped

```bash
defaults write -g CGFontRenderingFontSmoothingDisabled -bool NO
# then reboot
```

* [Fix blurry fonts on non-retina screens](https://discussions.apple.com/thread/250730319)


# quicklook
Quicklook

Creates thumbnails in the Finder

Can see the plugins in /System/Library/QuickLook and ~/Library/QuickLook

Not a standalone executable, implements QuickLookGeneratorPluginFactory

Uses the quicklookd daemon

``qlmanage`` is used to maintain the plugins and the daemon



# macOS ld

```bash
# Link files into macho64 for use on macOS
ld -macosx_version_min 10.7.0 -lSystem  file.o
```


# Apple System Log
backwards compatible with syslog

logs are binary, not textual

``log`` - access system log messages
``aslmanager``


# Launchd

```bash
# Get system resource limits
launchctl limit
```

## periodic

Called by launchd to execute shell scripts (very cron)

/etc/periodic/ - scripts

/etc/periodic.conf - override the default periodic config


## Resources

<http://ss64.com/osx/launchctl.html>
<https://developer.apple.com/library/mac/documentation/Darwin/Reference/ManPages/man1/launchctl.1.html>
<http://launchd.info/>



# spotlight
Spotlight

mdutil - manages the metadata for spotlight
mdfind - query the spotlight indexes
mdimport - test/list spotlight plugins
mdls - list the metadata of a file
mddiagnose - diagnose Spotlight issues

## Ignore a directory for indexing
touch the file ``.metadata_never_index``



# Sandboxing
sandbox-exec - deprecated

examples - /usr/share/sandbox and /System/Library/Sandbox/Profiles/
TinySCHEME

asctl - App sandbox control tool

[App Sandbox Design Guide](https://developer.apple.com/library/content/documentation/Security/Conceptual/AppSandboxDesignGuide/)



# notifications

notifyd
notifyutil



# Frameworks to look into
- AddressBook
- AppKit
- AudioToolBox
- ApplicationServices
- Automator
- CalendarStore
- Cocoa
- CoreAudio
- CoreFoundation
- CoreLocation
- CoreMotion
- DirectoryService
- EventKit
- Foundation
- GSS
- iAd
- ImageIO
- InstantMessage
- IOKit
- Kernal
- LDAP -> OpenDirectory
- Message
- PreferencePanes
- QuickLook
- SceneKit
- ScreenSaver
- Scripting (deprecated?)
- Security
- ServerNotification
- PubSub (RSS/Atom support!)
- StoreKit
- SyncServices


# macOS kernel

* [kernel
  syscalls](https://opensource.apple.com/source/xnu/xnu-1504.3.12/bsd/kern/syscalls.master)



# Darwin
darwinup - manage archives for the system



# Filesystems



## See filesystem attributes

	ls -lO



# Apple File System

Replaces HFS+

OS X 10.12+

## Features



* Container is base storage unit, generally 1:1 mapping to GPT
* 64-bit inaode numbers
* Nanosecond timestamp granularity
* Extensible block allocation
* Sparse files
* COW metadata
* Extended attributes
* TRIM operations
* Native encryption support, with multi-key encryption support
* SMB compatible


## Links


<https://developer.apple.com/library/prerelease/content/documentation/FileManagement/Conceptual/APFS_Guide/GeneralCharacteristics/GeneralCharacteristics.html#//apple_ref/doc/uid/TP40016999-CH2-SW1>




# FSEvents
(like inotify for mac)
/dev/fsevents

Better to use Spotlight, apparently *shrug*



# HFS+
The old filesystem for macOS

Could be case sensitive/insensitive
Could be journaled/unjournaled




# Dumb Fun

# boot in text console mode

Uncomment the /usr/libexec/getty console line in ``/etc/ttys``

# Universal binaries

```
arch
lipo
```

# Record a terminal session

```bash
script -r
# do whatever, then exit
script -p typescript
```


# bundles
Bundles

## Layout

Can be accessed by NSBundle

```
Contents/
  CodeResources/
  Info.plist     Main package manifest
  MacOS/         Binary contents
  PkgInfo        Eight character identifier of package
  Resources/     GUI + project files
  Version.plist
  _CodeSignature/
CodeResources
```

## Framework layout

/System/Library/Frameworks (and also /System/Library/PrivateFrameworks)

```
Contents/
  Headers/    - .h files
  Modules/
  Resources/
  Versions/
A/
Current/  - symlink to current version
``
`
TODO check out -framework compiler switch

# dns

path                                                       | description
---                                                        | ---
/etc/resolv.conf                                           | not used
/private/etc/hosts                                         | force resolution
/Library/Preferences/SystemConfiguration/preferences.plist | preferences
/etc/resolver/                                             | domain nameserver overrides


## Flush DNS cache

```bash
# On Yosemite+
sudo discoveryutil mdnsflushcache
# 10.10.4+
sudo dscacheutil -flushcache; sudo killall -HUP mDNSResponder
```

## Resolution

```bash
# Use Mac resolution service
dns-sd -G v4 example.com

# Query via DirectoryService
dscacheutil -q host -a name example.org

# Don't use Mac resolution
nslookup example.com
```

## DNS settings

``scutil --dns``

## mDNSResponder

listens port 5353

## helpful man pages

resolver(5)



# auditing

audit logs stored in /var/audit

``audit`` - utility to control the auditing system

Use ``praudit`` to output log in human readable form

Use ``praudit /dev/auditpipe`` to access the logs in real time

Use ``auditreduce`` to filter records from the file



# AppleScript

/Library/Scripts

Applescript Editor

Uses AppleEvents

Use osascript and Automator

## View AppleEvents output

export AEDebugSends=1 AEDebugReceives=1



# opendirectoryd

dscl - directory service command line utility

dscl . -read /Users/`whoami`

odutil - examine/change state of opendirectoryd



# Property List Format
``man plist``

plutil - manipulate/check plist files



# MacOS TFTP

Folder is `/private/tftpboot`, but can be changed by adjusting
`/System/Library/LaunchDaemons/tftp.plist`

```bash
sudo launchctl load -F /System/Library/LaunchDaemons/tftp.plist
sudo launchctl start com.apple.tftpd
```


# Tunnelblick
CLI
	# Connect to a single VPN
	osascript -e 'Tell app "Tunnelblick" to connect "example-vpn"'
	# Disconnect all VPNs
	osascript -e 'Tell app "Tunnelblick" to disconnect all'



# widgets
/Library/Widgets

Made of HTML+JS :-P

Dashboard disabled in 10.10 (Yosemite)
- Moved to Notification Center



# XCode

<https://developer.apple.com/library/content/documentation/DeveloperTools/Conceptual/debugging_with_xcode/chapters/about_debugging_w_xcode.html#//apple_ref/doc/uid/TP40015022>

## Using old SDKs

Edit MinimumSDKVersion in
/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Info.plist

From:
<https://stackoverflow.com/questions/11424920/how-to-point-xcode-to-an-old-sdk-so-it-can-be-used-as-a-base-sdk/11424966#11424966>
<https://stackoverflow.com/questions/18423896/is-it-possible-to-install-ios-6-sdk-on-xcode-5>
<https://gist.github.com/rnapier/3370649>



# Cocoapods
[Cocoapods](https://cocoapods.org/)



# Code signing
security(1) - dump keychains
codesign(1) - create/manipulate code signatures
csreq(1)

code signing is optional in macOS
unsigned code is killed by the kernel in iOS



# Speech Synthesis

```bash
say "talking from the command line"
```

## Objective-C
[Speech Synthesis Programming Guide](https://developer.apple.com/library/content/documentation/UserExperience/Conceptual/SpeechSynthesisProgrammingGuide/)

speech synthesizers located in ``/System/Library/Speech/Synthesizers``
voices located in ``/System/Library/Speech/Voices``

### Cocoa
[NSSpeechSynthesizer](https://developer.apple.com/documentation/appkit/nsspeechsynthesizer) class in AppKit

	NSSpeechSynthesizer *synvox = [NSSpeechSynthesizer new];
	NSString *hw = @"Hello world";
	[synvox startSpeakingString:hw];


### Carbon
The Carbon API provides more programmatic control if you need it



# security

Note: this is for the CLI to the keychains and Security framework, not a general
page.

```bash
# Print password
security find-generic-password -a "${account}" -s "${name}" -w
```


