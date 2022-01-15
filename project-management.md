# Project Management

# TaskJuggler

[Manual](https://taskjuggler.org/tj3/manual/index.html)
# taskwarrior

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
# taskpaper

## Omnifocus tags

tag                     | desc
---                     | ---
@autodone(bool)         | whether the item automatically completes itself when its children are complete (true) or not (false). Named to match @done.
@tags(string1, string2) | the tags to assign
@defer(date)            | defer until date, e.g. 2016-04-19 5pm or next Thursday -3d
@done(date)             | completed on date
@due(date)              | due on date
@estimate(time span)    | time estimate, e.g. 2h for 2 hours or 3w for 3 weeks.
@flagged                | present when an item is flagged
@parallel(bool)         | whether children are parallel (true) or sequential (false)
@repeat-method(method)  | the repeat method: fixed, start-after-completion, or due-after-completion
@repeat-rule(rule)      | an ICS repeat rule (see RFC244557), e.g. FREQ=WEEKLY;INTERVAL=1

* https://support.omnigroup.com/omnifocus-taskpaper-reference/
* https://omni-automation.com/omnifocus/taskpaper.html
