# Software Security
# Viruses

# History

Name        | Description                               | When             | Author
---         | ---                                       | ---              | ---
Elk Cloner  | First virus on a microcomputer (Apple II) | 1982             | Rich Skrenta
Morris Worm |                                           | November 2, 1988 | Bob Morris, Jr.




# The Art of Computer Virus Research and Defense by Peter Szor

Von Neumann machine - no difference between code + data

* A Universal Machine
* A Universal Constructor
* Information on tape


[Core War](core_war.md)

``Instead of writing computer viruses, I strongly recommend playing this harmless and interesting game. In fact, if worms fascinate you, a new version of Corw Wars can be created to link battles in different networks and allow warrior programs to jump from one battle to another to fight new enemies on those machines. Evolving the game to be more networked allows for simulating worm-like warrior programs.``

[Security:Viruses - history](viruses.md)

## Books
__Kinematic Self-Replicating Machines__ by Merkle + Freitas

* ecophagy - like the 'grey goo'?


__Games with Computer__ by Antal Csakany + Ferenc Vajda

"Computer Recreation" *Scientific American* Dewdney

# Core War

## History
Created by Robert Morris Sr. (NSA Chief Scientist), Victor Vyssotsky and Dennis Ritchie (Bell Labs)

Originally called *Darwin*,  ran on a PDP-1 in Bell Labs

## Redcode Assembly Language

10 instructions in original set, 14 in 1999

<!-- TODO fill desc -->

opcode | desc
---    | ---
DAT    | no-op
MOV    |
ADD    |
SUB    |
MUL    |
DIV    |
MOD    |
JMP    |
JMZ    |
JMN    |
DJN    |
CMP    |
SLT    |
SPL    |

- <http://www.koth.org/info/icws94.html>
## Memory Array Redcode Simulator (MARS)

## Types of programs

### Imp
Moves 1 address forward each cycle

## Sources

- [The Art of Computer Virus Research and Defense by Peter Szor](security/viruses.md#the-art-of-computer-virus-research-and-defense-by-peter-szor)
- <http://www.corewar.info/>
# wargames

Learning how to break software helps you build more robust software.

- [Microcorruption](https://microcorruption.com)
- [OverTheWire](http://overthewire.org/wargames/)
- [Stockfighter](https://www.stockfighter.io/#jailbreak)
- [Cryptopals](https://cryptopals.com/)
- <https://pwn.college/>
- <https://ctftime.org/>
# Concepts

## Confidentiality, Integrity, Availability

Understand the issues, risks

Assess, plan, design/architect

## Principle of Least Privilege

When designing a security policy, be it a firewall rule, or filesystem
permissions, never give more than the necessary permissions to get the job
done. Doing so reduces the attack surface, and weakens (though does not eliminate)
the potency of compromise. It's easier to loosen rules than to tighten them later.

## Appropriateness

The appropriateness of a security architecture is that it meeds the
confidentiality/integrity/availability needs of an organization. It balances
security, risk mitigation, usability, and costs.

## Non-repudiation

Where an action cannot be denied, proof of data integrity.

## Business continuity

One of the chief goals of security is that business continuity is ensured.
Beyond simple security practices, this is having systems in place that can
tolerate failure so that business continues with little/no affect.

## Hardening

A hardened system has these characteristics:

* Minimal amount of software (and hardware) installed and running - *only* what is needed
* Regular updates
* Privileges only for what is needed

## Computer Security Incident Response Teams (CSIRT)

Team responsible for receiving, reviewing, and responding to computer security
incident reports and activity

## Security incident and event management

Monitors security-related events from network devices, servers, etc

Logs and alerts on anomalies, malicious activity, puts it into "single pane of glass"

## References / citations

* SP800-12 "An Introduction to Computer Security: The NIST Handbook
* SP800-14 "Generally Accepted Principles and Practices for Securing Information Technology Systems"
* [OECD Digital Security Risk Management](https://www.oecd.org/sti/ieconomy/digital-security-risk-management.htm)
* Generally Accepted System Security Principles by International Information Security Foundation

* S. Bhatt, P. K. Manadhata and L. Zomlot, "The Operational Role of Security
  Information and Event Management Systems," in IEEE Security & Privacy, vol.
  12, no. 5, pp. 35-41, Sept.-Oct. 2014, doi: 10.1109/MSP.2014.103.

# Return-Oriented Programming
Looks for 'gadgets' in code, snippets of code in libc and elsewhere in lieu of providing the code itself.

## Other links

* [AntiJOP](http://zsmith.co/antijop.html)


# BeyondCorp

https://www.beyondcorp.com/

BeyondCorp is a zero-trust approach to building infrastructure, where instead
of an internal network accessible by VPN, each service is publicly available
and limited to known users.
# Security Life

- At a certain point, you'll look into the void, and realize that there is no way to secure software, only raise the cost of exploitation, and that there's always someone willing to pay that cost. Learn to be fine with that.

# Resources

- http://lcamtuf.blogspot.com/2016/08/so-you-want-to-work-in-security-but-are.html

# Security Conferences

- DefCon
- BlackHat
- ShmooCon
- HOPE
- CanSecWest

# Resources
* [Have I Been Pwned?](https://haveibeenpwned.com/)
<http://seclists.org>
<http://insecure.org>

## OpenSCAP

<https://www.open-scap.org/>

## STIG
<https://www.stigviewer.com/stigs>

