# Filesystems
Created Sunday 31 December 2017



See filesystem attributes
-------------------------

	ls -lO



# Apple File System
@filesystems, @macOS

Replaces HFS+

OS X 10.12+

Features
--------



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


Links
-----


<https://developer.apple.com/library/prerelease/content/documentation/FileManagement/Conceptual/APFS_Guide/GeneralCharacteristics/GeneralCharacteristics.html#//apple_ref/doc/uid/TP40016999-CH2-SW1>




# FSEvents
(like inotify for mac)
/dev/fsevents

Better to use Spotlight, apparently *shrug*



# HFS+
The old filesystem for macOS

Could be case sensitive/insensitive
Could be journaled/unjournaled


