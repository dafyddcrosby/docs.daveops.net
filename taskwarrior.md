---
title: taskwarrior
---

```bash
# List tasks
task list

# Add a task
task add Creating a task
# Add a task to a project
task add project:foo Creating a task

# Add a tag
task $ID modify +mytag
# Add a priority tag
task $ID +next

# Show tasks finished in the last week
task end.after:today-1wk completed

# Mark a task done
task done $ID

# List contexts
task context list
# Define context
task context define work +office or +work
# Show current context
task context show
# Clear context
task context none
```

## vit

key | desc
--- | ---
a   | add a task
A   | annotate a task
b   | start/stop task
d   | task done
D   | delete a task
e   | edit a task
P   | task priority
p   | task project
T   | task tags

## Links

* https://taskwarrior.org
