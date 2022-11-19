# Operations


# "The Practice of Cloud System Administration"

What are the well-defined business objectives? Work backwards.

- 99.99% uptime?
- Latency requirements?
- Process x million QPS?
- Regular introduction of features?
- Integration with other products?
- How quickly to fix major bugs?


## Operational requirements

There are aspects outside of the product itself that, if not dealt with adequately, make the product difficult/impossible to maintain

- Configuration
    - Use version control
    - API for configuration output? Ensure it's complete!

- Startup and shutdown
    - Perform data validation in event of crash
    - Document time to start/stop services
    - For HA systems, consider "crash-only" instead of orderly start/stop
        - Only way to start is crash-recovery, only way to stop is crash
        - This exercises the crash-recovery path, gives it more QA time

- Queue draining
- Software upgrades
- Backups and restores
- Redundancy
- Replicated databases
- Hot swaps
- Toggles for individual features
- Graceful degradation
- Access controls and rate limits
- Data import controls
- Monitoring
- Auditing
- Debug instrumentation
- Exception collection # Practice of System and Network Administration

Ticket tracking system for WIP OS installation should be fully automatic Configuration should be fully automatic

Software delivery life cycle - integration, delivery, deployment

Treat as much infrastructure as code as possible

Identify process bottlenecks

> Small changes need less approval. Big changes require a PowerPoint presentation, agreement from the team, and three levels of management. Small changes merit a casual suggestion that is approved with a nod. Pick the smallest increment of work that creates meaningful change.

Anything painful should be done more frequently - this is how automation gets written.

Minimum Viable Product - launch early and often

> Preferably the systems we deal with are fungible resources: Any one unit can substitute for any other.
> 
> A related metaphor is the snowflake. A snowflake is even more unique than a pet. It is one of a kind. A system may have started out similar to others, but it was customized, modified, and eventually becomes unlike any other system. Or maybe it started out unique and had very little chance of being properly brought into line with the others. A snowflake requires special operational procedures. Rebooting it requires extra care. Upgrades require special testing. As Martin Fowler (2012) wrote, a snowflake is "good for a ski resort, bad for a datacenter."

> Defaults are powerful. If you announce an OS configuration change that all IT subteams are required to make, you'll get angry push-back from your loudest and most vocal co-workers. You will get very little participation. In fact, there may be enough push-back that you withdraw the request. Often a tyranny of a few loud complainers prevents the majority from receiving a beneficial change.


## Cattle versus pets

> The more state a machine holds, the more irreplaceable it is---that is, the more pet-like it is. Cattle are generic because we can rebuild one easily thanks to the fact that cattle contain no state, or only state that can be copied from elsewhere.

Move variations in process to the end, keep as much stuff standard as long as possible.

Set aside time for automation - even if you don't need it right now

Make brokenness visible - "everyone knows about that problem" is bad

Workstation updates:

- Repeat early and iterate
- Have a transition interval
- Ratchet - forward progress only
- Set a cut off date


## Onboarding

- How soon can a new employee make meaningful contributions?
- How much effort to set up their workstation?


## Three ways of operational improvement

- Focus on the process itself
    - consistent, repeatable

- Focus on flow of information
    - feedback needs to flow upstream

- Focus on experimentation and learning
    - Try new things, learn from failure


## Launch Readiness Review and Launch Readiness Criteria

- Service is monitored
- Alerts are configured
- Backups and restores are working
- Auth and access control are tested and working
- SLA is defined
- Service request model in place
- User documentation is complete
- Operational documentation is complete
- User training complete
- Load testing complete
- Test service can handle ten-times expected traffic

Don't let the LRC become a burden by adding too many items. Automate the items, standardize parts where you can.

"The difference between a problem and a crisis is preparation."


# Operations checklist

This is a checklist of things that I think an operations team needs to get to a pretty good state. By no means exhaustive, and a bit opinionated. What I think is most important is at the top, but really should try to have all of them.

- Bug tracker/task manager
    - How do you track WIP (work-in-progress) tasks?
    - Customer can easily create a useful ticket?

- Version control
    - Can you see when a file was changed, and by whom? What you choose isn't necessarily important (e.g. git), but you'll likely never change the system once you start using it, so choose carefully.

- Configuration management
- **Fully** automatic installs
- CI/CD system
    - Deployment to production is fully automated

- Monitoring systems
- Inventory system
    - Can you, in less than 5 minutes, say how many machines (physical and virtual) you have running?

- Have an update process

> It is extremely important to have everything patched and up-to-date. This includes all hardware, networking devices, you name it. You'll want to know where and how to download the updates, how to install the updates, how to back out of a bad update, and a rough estimate of how long an update typically takes.


# Site Reliability Engineering (book)

The book: <https://landing.google.com/sre/book/>


## Foreward

> The tribal nature of IT culture often entrenches practitioners in dogmatic positions that hold the industry back.

Just a *touch* self-serving. But I guess if Google pays you to write a book, you kind of have to glorify it just a smidge.


## Preface

> As Margaret says, "a thorough understanding of how to operate the systems was not enough to prevent human errors," and the change request to add error detection and recovery software to the prelaunch program P01 was approved shortly afterwards.


## Introduction

"Hope is not a strategy." -Traditional SRE saying

Historical sysadmin approach - run the service, respond to events, ensure stability. The want of a sysadmin can run counter to dev, who want to push features.

So their big thing is 'write code instead of doing stuff manually.' Duh. (says the dev-turned-op)

Google SRE is an eng with a few extra skills, usually UNIX + L1-L3 networking

Google SRE's also write application code, to prevent dev/ops split

The SRE Team is responsible for:

- availability
- latency
- performance
- efficiency
- change management
- monitoring
- emergency response
- capacity planning

> The use of an error budget resolves the structural conflict of incentives between development and SRE. SRE's goal is no longer "zero outages"; rather, SREs and product developers aim to spend the error budget getting maximum feature velocity. This change makes all the difference. An outage is no longer a "bad" thing---it is an expected part of the process of innovation, and an occurrence that both development and SRE teams manage rather than fear.


## Monitoring

> Monitoring should never require a human to interpret any part of the alerting domain. Instead, software should do the interpreting, and humans should be notified only when they need to take action.

> Humans add latency. Even if a given system experiences more actual failures, a system that can avoid emergencies that require human intervention will have higher availability than a system that requires hands-on intervention. When humans are necessary, we have found that thinking through and recording the best practices ahead of time in a "playbook" produces roughly a 3x improvement in MTTR as compared to the strategy of "winging it." The hero jack-of-all-trades on-call engineer does work, but the practiced on-call engineer armed with a playbook works much better.


## Uptime

Aiming for 100% uptime doesn't make sense outside of life-critical systems. Determine the tolerance, and create a period-based error budget (ie 99.95% per-quarter). Then both development and operations are on the same page for how to gauge feature velocity. (GCE has a published 99.95% availability).

Missing a Service-Level Objective (SLO) doesn't mean a lawsuit - but it's an understanding of system stability.

> Choose just enough SLOs to provide good coverage of your system's attributes. Defend the SLOs you pick: if you can't ever win a conversation about priorities by quoting a particular SLO, it's probably not worth having that SLO. However, not all product attributes are amenable to SLOs: it's hard to specify "user delight" with an SLO.


## Reducing Toil

Automate tasks that are repetitive, no enduring value (ie don't help the service beyond after the work is done), are reactive in nature (ie pages), O(n) with service growth.

> If the work involved in a task scales up linearly with service size, traffic volume, or user count, that task is probably toil. An ideally managed and designed service can grow by at least one order of magnitude with zero additional work, other than some one-time efforts to add resources.


## Service Reliability Hierarchy

- Monitoring
- Incident Response
- Postmortem / Root Cause Analysis
- Testing + Release Procedures
- Capacity Planning
- Development
- Product

(Not sure the argument that this makes a pyramid holds up...)


## Managing Incidents

An incident is when something:

- involves a second team
- is customer-facing
- goes unsolved after an hour of concentrated analysis

Declare early before details get hazy

Distinct roles:

> Incident Command The incident commander holds the high-level state about the incident. They structure the incident response task force, assigning responsibilities according to need and priority. De facto, the commander holds all positions that they have not delegated. If appropriate, they can remove roadblocks that prevent Ops from working most effectively.
> 
> Operational Work The Ops lead works with the incident commander to respond to the incident by applying operational tools to the task at hand. The operations team should be the only group modifying the system during an incident.
> 
> Communication This person is the public face of the incident response task force. Their duties most definitely include issuing periodic updates to the incident response team and stakeholders (usually via email), and may extend to tasks such as keeping the incident document accurate and up to date.
> 
> Planning The planning role supports Ops by dealing with longer-term issues, such as filing bugs, ordering dinner, arranging handoffs, and tracking how the system has diverged from the norm so it can be reverted once the incident is resolved.

Incident commander hand-off is explicitly confirmed by person taking over.


## Misc

[Dan Luu's notes](https://danluu.com/google-sre-book/)


# DevOps

- DevOps is not something you can buy or force down someone's throat
- DevOps does not require special tools. It requires a mindset of deployment automation and frequent builds
- Failure is inevitable, work to reduce MTTR
- Developers and ops should be working together