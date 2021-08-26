---
title: 12 Factor App
---

* <http://www.12factor.net/>
* <https://medium.com/@kelseyhightower/12-fractured-apps-1080c73d481c>

* Codebase
  * One codebase tracked in revision control, many deploys (i.e. it's all in Git)
* Dependencies
  * Explicitly declare and isolate dependencies. Don't rely on system tools - app should be self-contained.
* Config
  * Store config using environment variables (which won't be accidentally committed). Ask yourself "could this be open-sourced **right now**?"
* Backing Services
  * Treat backing services as attached resources
* Build, release, run
  * Strictly separate build and run stages
* Processes
  * Execute the app as one or more stateless processes
* Port binding
  * Export services via port binding
* Concurrency
  * Scale out via the process model
* Disposability
  * Maximize robustness with fast startup and graceful shutdown
* Dev/prod parity
  * Keep development, staging, and production as similar as possible
* Logs
  * Treat logs as event streams
* Admin processes
  * Run admin/management tasks as one-off processes


