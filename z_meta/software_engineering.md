# Bodies of knowledge


## IEEE SWEBOK

ISO created the [Software Engineering Body of Knowledge](https://ieeecs-media.computer.org/media/education/swebok/swebok-v3.pdf), which can be used to determine gaps in software knowledge.

<https://www.computer.org/education/bodies-of-knowledge/software-engineering>


## Australian Computer Society

<https://www.acs.org.au/content/dam/acs/acs-skills/The-ACS-Core-Body-of-Knowledge-for-ICT-Professionals-CBOK.pdf>


## SFIA

Skills Framework for the Information Age (SFIA) promotes [existing BoKs](https://sfia-online.org/en/tools-and-resources/bodies-of-knowledge/list-of-bodies-of-knowledge) as opposed to creating a new one

<https://sfia-online.org/en/tools-and-resources/bodies-of-knowledge>


# Pretotyping

<http://www.pretotyping.org>


# Advice for new software devs

- Build the right *it* before you build it *right*. Do a [pretotype](http://www.pretotyping.org) before a prototype to see if it's even worth the effort (you'll find often it's not).
- When you're stuck, ask a more experienced developer. Don't let pride keep you from getting the work done.
- If you see something broken, resist the immediate temptation to fix it.
    - Mark it as TODO, and return to it when you've got free time.

- Learn how to write really good bug reports
    - Steps to duplicate (in ordered form)
    - Desired behaviour
    - Current behaviour
    - Bonus points if you can explain how it affects the end user

- You'll spend more time reading code and documentation than writing it, so make sure the stuff you write is readable
- Your "soft skills" will carry you much further than your coding skills
    - Public speaking
    - Pleasant conversations
    - Writing skills

- Take notes of new stuff, and have a system to organize the notes (e.g. a wiki)
    - You will want a digital system for this. The faster you can retrieve your notes, the better.

- Always try to work with exceptionally good developers
- What makes software 'legacy software' is that you will grow smarter and more experienced, and you'll just know better ways to organize and your code. If you try to learn lessons from legacy software, your time will be well-rewarded.
- Look at the source code of the programs you use regularly. You might just be able to directly improve your own tools.
- Don't celebrate sleep-deprivation. It's stupid. Get 8-9 hours of [good sleep](life_stuff.md) a day, so that your mind can actually do good work.
- Don't break backwards compatibility unless the idea behind breaking it is exceptionally good (e.g. the program runs 5x faster, takes 1/10th the memory, or something else materially worthwhile for your end users)
- Don't work in a job just for the money - the money will dry up, but those memories won't. Your life is too valuable to waste on stupid crap.
- Learn how to write tests
    - ...but don't go overboard. Find the right amount of testing so that it's actually *helpful*. Code-to-test ratio shouldn't be over 1.2, and writing tests shouldn't take more than 1/3 of your time.

- Know what 'good enough' is in any project. There's seldom a need to optimize, don't build up technical debt needlessly.
- Legacy software is the perception shift that happens when you become a better programmer. Legacy software *keeps you honest*.
- [Your users might never tell you something is broken](https://pointersgonewild.com/2019/11/02/they-might-never-tell-you-its-broken/)


# Jet Propulsion Laboratory


## Pi

3.141592653589793

From <http://www.jpl.nasa.gov/edu/news/2016/3/16/how-many-decimals-of-pi-do-we-really-need/>


# Capability Maturity Model

<https://en.wikipedia.org/wiki/Capability_Maturity_Model>

- Level 1 - Initial
- Level 2 - Repeatable
- Level 3 - Defined
- Level 4 - Managed (Capable)
- Level 5 - Optimizing (Efficient)


# Computer Associations


## IEEE

- [Certification](https://www.computer.org/education/certifications)

Standards:

| no.  | desc                        |
|---- |--------------------------- |
| 1003 | POSIX                       |
| 1016 | Software design description |


## ACM


## [Canadian Information Processing Society (CIPS)](http://cips.ca)


# Good Software Practices


## Don't reinvent the wheel

The world does not need another XML parser. Use what's already been written and tested. Often what you think sucks only needs a few patches to be adequate.


## Code without a written plan is just asking for trouble

When I hear someone brag that they've got the whole program in their head, I know it will turn out like garbage. Exceptions to this are rare. There's always something found in the process of writing the idea down that makes the program better, and it also becomes a form of documentation.


## Don't repeat yourself

Try to remove duplication in code by refactoring, so that it's easier to extend and debug the program later on.


## Your build system should *never* be more than 1 step

`configure && make && make install` should be the most complicated build setup you have.

Get it to one-step-to-build - instant feedback is awesome, and the time saved will pay off when "I need a build for the customer in 5 minutes!" For that matter, have automated testing that the master always builds clean.


## Keep it simple

I once replaced an elaborate Java-based build system with 2 shell scripts. Mine worked faster, better, and was simpler to read.


## Prototypes!

A simple prototype done in a few hours can save a week's worth of speculation, and often you can plunder the prototype for technical insights and even code.


## Business rules and logic don't mix well

Business rules will change. It's in your best interest to make them easily configurable (read: not compiled into the binary). Even better, make it simple enough that Bob in accounting can manage the rules.


## Code can be the best documentation

Descriptive variables and functions are better than comments. When you actually need non-code documentation, make sure that the docs are automatically pulled together from the code (a la doxygen, etc.).


## Keep LOC down to keep bugs down

Code Complete said that the industry average is 15-50 bugs per 1000 lines of code. By reducing the lines of code and keeping functions simple, your life will be much easier.


## ALWAYS validate your inputs

Because 13-year-olds know enough to fuzz your text box. SQL injections are one of the most common bugs.


## Be correct first, then be fast.

Debugging a fast program that's segfaulting is slow work...


## Always review your releases

Code audits are a cheap way of ensuring quality (though they may not seem so at the time). Lost customers and lost time to troubleshooting problems are expensive. As well, keep metrics on defects from release to release, so that you can adjust practices as necessary. Use the "Infinite Hows" technique to examine release/deployment issues - deployment issues should be dealt with once, *permanently*.


## 1 hour of inspection can save up to 100 hours of related work

IBM study found "each hour of inspection prevented about 100 hours of related work (testing and defect correction)"


## Testing - it's worth the effort

When you learn a new language, learn how to test its code as early as possible. Time spent writing tests is rarely wasted. When you use test cases, you can refactor without (much) fear. When you need to debug quickly, having tests make a huge difference.


## It's easier to be secure when you bake it in at the start

It sucks to break backwards compatibility, or worse, get caught with your pants down, because *someone* didn't bother with security when they hacked the program together.


## Stop with 'good enough'

To keep from going crazy and spending too much time on irrelevant crap, set specific and quantitative goals for your project at the start. When you've met a goal, move on to the next one.


## Source control everything

Seriously. Learn git. It'll take you just a couple hours, and you'll make it back in no time at all.


## Don't marry your framework

A framework is a great way to ensure you don't reinvent the wheel, but if possible keep your business logic external to it. That way if something sexier comes along you're not fully invested in the old crap. Decoupling can also mean faster tests.


## Practice defensive programming

Use assertions to ensure the state is correct, and use tests to simulate faults.


## A bad process will lead to lots of preventable headaches

Take an active role in perfecting the process wherever you work.


## Nail the basic process to avoid certain failure

- Do an analysis, capture the requirements, and create a design document
- Review the design - catch errors before the coding stage
- Code review
- Use automated code analysis
- Unit and component tests to stress-test and ensure edge cases are properly handled


## Velocity is a good measure of project health

Good code allows you to fix bugs and add features quickly.


## Keep a bug database

Even something as simple as BUGS.txt eliminates bus factor, and makes it easier to approach a project months later.


## Keep a changelog

This doesn't have to be fancy, and there's [thoughtful guidelines you can follow](https://keepachangelog.com/en/1.0.0/). Just remember that this is for humans, not machines.


# Chaos Engineering

- [Principles of Chaos Engineering](https://principlesofchaos.org/)


# System Design


## Software System Design Cycle

1. Determine feature set
    - Functional requirements (how should it work?)
    - Non-functional requirements (latency, availability)
    - Extended requirements (analytics, monitoring, integration)

2. Back of the envelope estimations
    - Ratios between reads, writes
    - \# actions/month, queries per second
    - storage, memory, bandwidth estimates

3. System interface definition
    - API signatures
    - Design against API abuse

4. Define the data model
    - Database schema
    - Relational vs non-relational

5. High-level design (basic block diagram)
6. Detailed design
7. Identify/resolve bottlenecks, justify design
    - Look for SPOFs
    - How is partition/machine loss handled?
    - Where should caches reside?
    - How to monitor?


## Vocab


### Stovepipe system

System that solves a specific problem, but shares nothing with larger system (eg maintains own user/password system)


# Theory of Inventive Problem Solving

Teoriya Resheniya Izobreatatelskikh Zadatch

- [TRIZ Journal](https://triz-journal.com)


## Theory

From ["What is TRIZ?"](https://triz-journal.com/what-is-triz/)

1. That the same problems and solutions appear again and again across different industries, but that most organisations tend to re-invent the wheel rather than look outside their own experiences or the experiences of their direct competitors.
2. That the most powerful solutions are the ones that successfully eliminate the compromises and trade-offs conventionally viewed as inherent in systems.
3. That there are only a small number of possible strategies for overcoming such contradictions.
4. That the most powerful solutions also make maximum use of resources. Most organisations are highly inclined to solve problems by adding things rather than making the current things work more effectively, or transforming the things viewed as harmful into something useful.
5. That technology evolution trends follow highly predictable paths.

The Ideal Final Result (IFR) is when the product has no costs/harms to the customer.

Five Pillars:

- Ideality
- Contradictions


# YAGNI exceptions

This is a non-exhaustive list of features that are worth implementing early because the cost is significantly greater later in the process.

- API
    - client kill-switch ("you need to update your client to use the new API")
    - pagination
    - detailed logging
    - versioning

- Automated deployment
- Testing framework
- zero-one-many applications
- timestamps (especially `created_at`)
- basic documentation / operations playbook

- <https://lukeplant.me.uk/blog/posts/yagni-exceptions/>
- <https://simonwillison.net/2021/Jul/1/pagnis/>


# Good Videos

This is a list of talks, etc. that are quite good

- [Is it really "Complex"? Or did we just make it "Complicated" (Alan Kay)](https://www.youtube.com/watch?v=ubaX1Smg6pY)
- [17th Century Shipbuilding and Your Failed Software Project (Pete Cheslock)](http://confreaks.tv/videos/monitorama2014-monitorama-pdx-2014-lightning-talk-pete-cheslock)

- Gerry Sussman - We Really Don't Know How To Compute
- Alan Kay - The Computer Revolution Hasn't Happened Yet


# Software Disasters

This is a list of software disasters where bugs had real-world repercussions beyond typical annoyance.


## Mariner 1 rocket

Transcription mistake sent rocket off-course


## Hartford Coliseum Collapse

CAD software didn't take into account wet snow


## Therac-25

Race condition gave lethal doses of radiation


## 1983 Soviet early-warning ballistic detection system

Software did not account for false detections from clouds, nearly started WW3


## Knight Capital Trading glitch

Insufficient cleanup of program artifacts, lack of testing, and lack of monitoring led to $440 million loss in 30 minutes.


## 1990 AT&T Outage

Single line of code knocked out a single switch, shuts down national phone network


## 1991 Patriot system

Miscalculation of trajectory because of rounding error


## Pentium FPU (f00f bug)

Floating point instruction halts processor

- <https://en.wikipedia.org/wiki/Pentium_F00F_bug>
- <https://willamette.edu/~mjaneba/pentprob.html>


## Cyrix Coma bug

Unprivileged instructions send processor into infinite loop, requiring reboot

<https://en.wikipedia.org/wiki/Cyrix_coma_bug>


## Arianne 5

64-bit to 16-bit conversion, causing an overflow that knocked out the guidance system. Backup system had identical problem.


## Y2K bug

Shortening year to two digits causes overflow at turn of century


## 2038 bug

32-bit `time_t` overflows in year 2038


## Mars Orbiter

Mismatch of metric and imperial measurements puts orbiter wildly off-course.


## Virtual Case File

Less a case of any one bug, and more program mismanagement. $170 million spent before program scrapped. A case study in why following software engineering best practices matters.

<https://en.wikipedia.org/wiki/Virtual_Case_File>


# Blogs

- [Coding Horror](http://www.codinghorror.com/blog/)
- [Joel on Software](http://www.joelonsoftware.com/)
- [37signals Company Blog](https://37signals.com/svn)
- [Steve Yegge's Drunken Rants](https://sites.google.com/site/steveyegge2/blog-rants)
- [Programming in the 21st Century](http://prog21.dadgum.com/)
- [Kernel Who?](https://kernelwho.wordpress.com)
- [Papers We Love](http://paperswelove.org/)


## Windows / Microsoft

- [Mark Russinovich](https://blogs.technet.microsoft.com/markrussinovich/)
- [The Old New Thing (Raymond Chen)](https://blogs.msdn.microsoft.com/oldnewthing/)


# Hypothesis-Driven Delivery

```
We believe that
  [building this feature]
  [for these people]
  will achieve [this outcome].
We will know we are successful when we see
  [this signal from the market].
```


# Reliability-Centered Maintenance

1. What is the item supposed to do and its associated performance standards?
2. In what ways can it fail to provide the required functions?
3. What are the events that cause each failure?
4. What happens when each failure occurs?
5. In what way does each failure matter?
6. What systematic task can be performed proactively to prevent, or to diminish to a satisfactory degree, the consequences of the failure?
7. What must be done if a suitable preventive task cannot be found?


# Laws of Computing


## Marick's Law

In software, anything of the form "X's Law" is better understood by replacing the word "Law" with "Fervent Desire".


## Hanlon's Razor

Don't attribute to malice that which is adequately explained by carelessness/ignorance/stupidity.


# Practice programming

<https://jasonrudolph.com/blog/2011/08/09/programming-achievements-how-to-level-up-as-a-developer/>


## Puzzles

- <http://codekata.pragprog.com>
- [Project Euler](http://projecteuler.net/)
- <http://programmingpraxis.com/>
- <http://www.rubyquiz.com/>
- <http://www.pythonchallenge.com/>
- <http://www.gowrikumar.com/c/index.html>
- <http://rosettacode.org/>
- <https://sites.google.com/site/prologsite/prolog-problems>
- [Regex Golf](https://alf.nu/RegexGolf)


## Competitions

- <http://topcoder.com>
- <http://vimgolf.com/>
- <http://puzzlenode.com/>
- <http://codebrawl.com/>
- <http://railsrumble.com/> (September)
- <http://www.codewa.rs/>


## Problems

- Write a program to do the pop count of a byte
- Write a program to check a string for duplicate characters


# Software bugs


## Design Bugs

- Don't Repeat Yourself violation
- Bad API usage
- Using deprecated functions
- Incorrect variable manipulation
- Incorrect regular expression use
- Incorrect protocol implementation
- Unauthorized user access
- Conceptual Error
- Incorrect documentation
- Incorrect return value
- Incorrect database query
- Incorrect business rules


## UI Bugs

- Missing/bad information
- Insufficient performance
- Inappropriate error message
- Expectation failure


## Code Bugs

- Syntax error
- Off-by-one
- Division-by-zero
- Loss of arithmetic precision
- Injection attack
- Overflow
- Underflow
- Infinite loop
- Uninitialized variable
- Incorrect initialization of variable
- Resource leak
- Excessive recursion
- Null pointer dereference
- Race condition
- Doesn't return memory when finished
- incorrect data type handling
- Incorrect file handling


## Testing Errors

- Did not provide steps to reproduce
- Misunderstanding of documentation
- Failed to notice or report problem
- Failed to run automated tests first
- Failure to verify fixes


# Time bugs

[Wikipedia list of time bugs](https://en.wikipedia.org/wiki/Time_formatting_and_storage_bugs)


## TOPS-10

Has a clock for <span class="timestamp-wrapper"><span class="timestamp">[1964-01-01 Wed] </span></span> to <span class="timestamp-wrapper"><span class="timestamp">[1975-01-04 Sat]</span></span>. It was given another 3 bits to go to <span class="timestamp-wrapper"><span class="timestamp">[2052-02-01 Thu]</span></span>, but this crept into other data structures in use causing weird bugs


## NTP overflow


## Unix 32-bit

2038


## World Computer Corporation

2028


## GPS epoch

Last was in 2019 - happens every 1024 weeks


# Academic Papers


## On Reading Papers

- [Should I Read Papers?](http://michaelrbernste.in/2014/10/21/should-i-read-papers.html)
- <https://violentmetaphors.com/2013/08/25/how-to-read-and-understand-a-scientific-paper-2/>
- <https://organizationsandmarkets.com/2010/08/31/how-to-read-an-academic-article/>


## How to read a paper

- Read intro and conclusion first
- Summarize the background and specific questions
- Determine purpose, structure, and direction
- Write down every word you don't understand
- Main points? Strengths? Weaknesses? Do the results answer the questions?
- Does the abstract jive with the rest of the paper?
- Look into citations
- What do other researchers say about the paper?


## Sources For Papers

- [The Morning Paper](https://blog.acolyer.org/)
- [Papers We Love](https://github.com/papers-we-love/papers-we-love)


# Versioning


## [Semantic Versioning](https://semver.org/)

> Given a version number MAJOR.MINOR.PATCH, increment the:
> 
> MAJOR version when you make incompatible API changes MINOR version when you add functionality in a backward compatible manner PATCH version when you make backward compatible bug fixes
> 
> Additional labels for pre-release and build metadata are available as extensions to the MAJOR.MINOR.PATCH format.


## [Calendar Versioning](https://calver.org/)

Use eg YYYY-MM-DD for versioning
