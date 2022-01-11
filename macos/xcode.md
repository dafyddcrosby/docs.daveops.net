# XCode
Created Wednesday 24 January 2018

<https://developer.apple.com/library/content/documentation/DeveloperTools/Conceptual/debugging_with_xcode/chapters/about_debugging_w_xcode.html#//apple_ref/doc/uid/TP40015022>

Using old SDKs
--------------

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

