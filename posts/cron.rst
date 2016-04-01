cron
====
:date: 2016-04-01

Command line
------------
.. code-block::bash

  # List crontab
  crontab -l 
  # Edit crontab
  crontab -e
  # Edit another user's crontab
  crontab -e -u USER

Syntax
------
::

 ┌───────────── min (0 - 59) 
 │ ┌────────────── hour (0 - 23)
 │ │ ┌─────────────── day of month (1 - 31)
 │ │ │ ┌──────────────── month (1 - 12)
 │ │ │ │ ┌───────────────── day of week (0 - 6) (0 to 6 are Sunday to Saturday, or use names; 7 is Sunday, the same as 0)
 │ │ │ │ │
 │ │ │ │ │
 * * * * *  command to execute
