# System Design

## Software System Design Cycle

1. Determine feature set
   - Functional requirements (how should it work?)
   - Non-functional requirements (latency, availability)
   - Extended requirements (analytics, monitoring, integration)
2. Back of the envelope estimations
   - Ratios between reads, writes
   - # actions/month, queries per second
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
