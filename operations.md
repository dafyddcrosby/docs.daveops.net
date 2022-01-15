# Operations
# "The Practice of Cloud System Administration"

What are the well-defined business objectives? Work backwards.

* 99.99% uptime?
* Latency requirements?
* Process x million QPS?
* Regular introduction of features?
* Integration with other products?
* How quickly to fix major bugs?

## Operational requirements

There are aspects outside of the product itself that, if not dealt with
adequately, make the product difficult/impossible to maintain

* Configuration
  * Use version control
  * API for configuration output? Ensure it's complete!
* Startup and shutdown
  * Perform data validation in event of crash
  * Document time to start/stop services
  * For HA systems, consider "crash-only" instead of orderly start/stop
    * Only way to start is crash-recovery, only way to stop is crash
    * This exercises the crash-recovery path, gives it more QA time
* Queue draining
* Software upgrades
* Backups and restores
* Redundancy
* Replicated databases
* Hot swaps
* Toggles for individual features
* Graceful degradation
* Access controls and rate limits
* Data import controls
* Monitoring
* Auditing
* Debug instrumentation
* Exception collection
# Practice of System and Network Administration

Ticket tracking system for WIP
OS installation should be fully automatic
Configuration should be fully automatic

Software delivery life cycle - integration, delivery, deployment

Treat as much infrastructure as code as possible

Identify process bottlenecks

> Small changes need less approval. Big changes require a PowerPoint
> presentation, agreement from the team, and three levels of management. Small
> changes merit a casual suggestion that is approved with a nod. Pick the smallest
> increment of work that creates meaningful change.

Anything painful should be done more frequently - this is how automation gets
written.

Minimum Viable Product - launch early and often

> Preferably the systems we deal with are fungible resources: Any one unit can
> substitute for any other.
> 
> A related metaphor is the snowflake. A snowflake is
> even more unique than a pet. It is one of a kind. A system may have started out
> similar to others, but it was customized, modified, and eventually becomes
> unlike any other system. Or maybe it started out unique and had very little
> chance of being properly brought into line with the others. A snowflake requires
> special operational procedures. Rebooting it requires extra care. Upgrades
> require special testing. As Martin Fowler (2012) wrote, a snowflake is “good for
> a ski resort, bad for a datacenter.”

> Defaults are powerful. If you announce an OS configuration change that all IT
> subteams are required to make, you’ll get angry push-back from your loudest and
> most vocal co-workers. You will get very little participation. In fact, there
> may be enough push-back that you withdraw the request. Often a tyranny of a few
> loud complainers prevents the majority from receiving a beneficial change.

## Cattle versus pets

> The more state a machine holds, the more irreplaceable it is—that is, the more
> pet-like it is. Cattle are generic because we can rebuild one easily thanks to
> the fact that cattle contain no state, or only state that can be copied from
> elsewhere.

Move variations in process to the end, keep as much stuff standard as long as
possible.

Set aside time for automation - even if you don't need it right now

Make brokenness visible - "everyone knows about that problem" is bad

Workstation updates
* Repeat early and iterate
* Have a transition interval
* Ratchet - forward progress only
* Set a cut off date

## Onboarding

* How soon can a new employee make meaningful contributions?
* How much effort to set up their workstation?

## Three ways of operational improvement
* Focus on the process itself
  * consistent, repeatable
* Focus on flow of information
  * feedback needs to flow upstream
* Focus on experimentation and learning
  * Try new things, learn from failure

## Launch Readiness Review and Launch Readiness Criteria

* Service is monitored
* Alerts are configured
* Backups and restores are working
* Auth and access control are tested and working
* SLA is defined
* Service request model in place
* User documentation is complete
* Operational documentation is complete
* User training complete
* Load testing complete
* Test service can handle ten-times expected traffic

Don't let the LRC become a burden by adding too many items. Automate the items,
standardize parts where you can.

"The difference between a problem and a crisis is preparation."
# Operations checklist

This is a checklist of things that I think an operations team needs to get to a
pretty good state. By no means exhaustive, and a bit opinionated. What I think
is most important is at the top, but really should try to have all of them.

* Bug tracker/task manager
  * How do you track WIP (work-in-progress) tasks?
  * Customer can easily create a useful ticket?
* Version control
  * Can you see when a file was changed, and by whom? What you choose isn't necessarily important (e.g. git), but you'll likely never change the system once you start using it, so choose carefully. 
* Configuration management
* **Fully** automatic installs
* CI/CD system
  * Deployment to production is fully automated
* Monitoring systems
* Inventory system
  * Can you, in less than 5 minutes, say how many machines (physical and virtual) you have running?
* Have an update process

> It is extremely important to have everything patched and up-to-date. This
> includes all hardware, networking devices, you name it. You'll want to know
> where and how to download the updates, how to install the updates, how to back
> out of a bad update, and a rough estimate of how long an update typically
> takes.

