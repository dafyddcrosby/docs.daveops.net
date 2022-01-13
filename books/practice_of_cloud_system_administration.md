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
