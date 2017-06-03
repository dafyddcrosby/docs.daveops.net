Site Reliability Engineering (book)
===================================
:date: 2017-02-16
:modified: 2017-06-02
:tags: books

The book: https://landing.google.com/sre/book/

Foreward
--------
::

  The tribal nature of IT culture often entrenches practitioners in dogmatic positions that hold the industry back.

Just a *touch* self-serving. But I guess if Google pays you to write a book, you kind of have to glorify it just a smidge.

Preface
-------

::

   As Margaret says, "a thorough understanding of how to operate the systems was not enough to prevent human errors," and the change request to add error detection and recovery software to the prelaunch program P01 was approved shortly afterwards. 


Introduction
------------

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

::
 The use of an error budget resolves the structural conflict of incentives
 between development and SRE. SRE’s goal is no longer "zero outages"; rather,
 SREs and product developers aim to spend the error budget getting maximum
 feature velocity. This change makes all the difference. An outage is no longer
 a "bad" thing—it is an expected part of the process of innovation, and an
 occurrence that both development and SRE teams manage rather than fear. 

Monitoring
----------
::

 Monitoring should never require a human to interpret any part of the alerting
 domain. Instead, software should do the interpreting, and humans should be
 notified only when they need to take action.

::

 Humans add latency. Even if a given system experiences more actual failures, a
 system that can avoid emergencies that require human intervention will have
 higher availability than a system that requires hands-on intervention. When
 humans are necessary, we have found that thinking through and recording the
 best practices ahead of time in a "playbook" produces roughly a 3x improvement
 in MTTR as compared to the strategy of "winging it." The hero
 jack-of-all-trades on-call engineer does work, but the practiced on-call
 engineer armed with a playbook works much better.

Uptime
------

Aiming for 100% uptime doesn't make sense outside of life-critical systems.
Determine the tolerance, and create a period-based error budget (ie 99.95%
per-quarter). Then both development and operations are on the same page for how
to gauge feature velocity. (GCE has a published 99.95% availability).

Missing a Service-Level Objective (SLO) doesn't mean a lawsuit - but it's an
understanding of system stability.

::

 Choose just enough SLOs to provide good coverage of your system’s attributes.
 Defend the SLOs you pick: if you can’t ever win a conversation about
 priorities by quoting a particular SLO, it’s probably not worth having that
 SLO. However, not all product attributes are amenable to SLOs: it’s hard to
 specify "user delight" with an SLO.

Reducing Toil
-------------

Automate tasks that are repetitive, no enduring value (ie don't help the
service beyond after the work is done), are reactive in nature (ie pages), O(n)
with service growth.

::

 If the work involved in a task scales up linearly with service size, traffic
 volume, or user count, that task is probably toil. An ideally managed and
 designed service can grow by at least one order of magnitude with zero
 additional work, other than some one-time efforts to add resources. 

Service Reliability Hierarchy
-----------------------------

- Monitoring
- Incident Response
- Postmortem / Root Cause Analysis
- Testing + Release Procedures
- Capacity Planning
- Development
- Product

(Not sure the argument that this makes a pyramid holds up...)

Managing Incidents
------------------

An incident is when something:
- involves a second team
- is customer-facing
- goes unsolved after an hour of concentrated analysis

Declare early before details get hazy

Distinct roles:

::

  Incident Command
  The incident commander holds the high-level state about the incident. They structure the incident response task force, assigning responsibilities according to need and priority. De facto, the commander holds all positions that they have not delegated. If appropriate, they can remove roadblocks that prevent Ops from working most effectively.

  Operational Work
  The Ops lead works with the incident commander to respond to the incident by applying operational tools to the task at hand. The operations team should be the only group modifying the system during an incident.

  Communication
  This person is the public face of the incident response task force. Their duties most definitely include issuing periodic updates to the incident response team and stakeholders (usually via email), and may extend to tasks such as keeping the incident document accurate and up to date.

  Planning
  The planning role supports Ops by dealing with longer-term issues, such as filing bugs, ordering dinner, arranging handoffs, and tracking how the system has diverged from the norm so it can be reverted once the incident is resolved.

Incident commander hand-off is explicitly confirmed by person taking over.

Misc
----

`Dan Luu's notes <https://danluu.com/google-sre-book/>`_

