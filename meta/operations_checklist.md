---
title: Operations checklist
---

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

  It is extremely important to have everything patched and up-to-date. This
  includes all hardware, networking devices, you name it. You'll want to know
  where and how to download the updates, how to install the updates, how to back
  out of a bad update, and a rough estimate of how long an update typically
  takes.

