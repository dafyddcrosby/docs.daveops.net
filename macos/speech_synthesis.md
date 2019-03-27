# Speech Synthesis
Created Sunday 31 December 2017

CLI
---

``say "talking from the command line``

Objective-C
-----------
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

