# System Design


## Software System Design Cycle

1.  Determine feature set
    -   Functional requirements (how should it work?)
    -   Non-functional requirements (latency, availability)
    -   Extended requirements (analytics, monitoring, integration)

2.  Back of the envelope estimations
    -   Ratios between reads, writes
    -   \# actions/month, queries per second
    -   storage, memory, bandwidth estimates

3.  System interface definition
    -   API signatures
    -   Design against API abuse

4.  Define the data model
    -   Database schema
    -   Relational vs non-relational

5.  High-level design (basic block diagram)
6.  Detailed design
7.  Identify/resolve bottlenecks, justify design
    -   Look for SPOFs
    -   How is partition/machine loss handled?
    -   Where should caches reside?
    -   How to monitor?


## Vocab


### Stovepipe system

System that solves a specific problem, but shares nothing with larger system (eg maintains own user/password system)


# Theory of Inventive Problem Solving

Teoriya Resheniya Izobreatatelskikh Zadatch


## Theory

From ["What is TRIZ?"](https://triz-journal.com/what-is-triz/)

1.  That the same problems and solutions appear again and again across different industries, but that most organisations tend to re-invent the wheel rather than look outside their own experiences or the experiences of their direct competitors.
2.  That the most powerful solutions are the ones that successfully eliminate the compromises and trade-offs conventionally viewed as inherent in systems.
3.  That there are only a small number of possible strategies for overcoming such contradictions.
4.  That the most powerful solutions also make maximum use of resources. Most organisations are highly inclined to solve problems by adding things rather than making the current things work more effectively, or transforming the things viewed as harmful into something useful.
5.  That technology evolution trends follow highly predictable paths.

The Ideal Final Result (IFR) is when the product has no costs/harms to the customer.

Five Pillars:

-   Ideality
-   Contradictions


## Links

-   [TRIZ Journal](https://triz-journal.com)


# YAGNI exceptions

This is a non-exhaustive list of features that are worth implementing early because the cost is significantly greater later in the process.

-   API
    -   client kill-switch ("you need to update your client to use the new API"
    -   pagination
    -   detailed logging
    -   versioning

-   Automated deployment
-   Testing framework
-   zero-one-many applications
-   timestamps (especially created<sub>at</sub>)
-   basic documentation / operations playbook

-   <https://lukeplant.me.uk/blog/posts/yagni-exceptions/>
-   <https://simonwillison.net/2021/Jul/1/pagnis/>