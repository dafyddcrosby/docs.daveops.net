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
# JIRA

## JQL search queries

```
# Search for unresolved tickets with label foo
resolution=unresolved and labels=foo

# Ticket is unassigned
assignee is EMPTY
```

* <https://www.atlassian.com/blog/jira-software/jql-the-most-flexible-way-to-search-jira-14>
* [Advanced search
  queries](https://confluence.atlassian.com/jirasoftwarecloud/advanced-searching-764478330.html)

### Common Fields
* Affected version
* Assignee
* Attachments
* Comment
* Component
* Created
* Creator
* Description
* Due
* Epic link
* Filter
* Fix version
* Issue key
* Labels
* Last viewed
* Priority
* Project
* Reporter
* Resolved
* Sprint
* Status
* Summary
* Text
* Time spent
* Voter
* Watcher
# Agile

[Agile Manifesto](http://agilemanifesto.org/)

Start with the principles, and then look at the practices

## Scrum
Keep each speaker to a very limited question set.

* What did you accomplish since the last meeting?
* What are you currently working on?
* What are you blocked by?
* What do you need from the rest of the team?

## Books

* A Practical Approach to Large-Scale Agile Development: How HP Transformed
  LaserJet FutureSmart Firmware by Mike Young, Gary Gruver, Pat Fulghum
# LiquidPlanner

## API

```bash
curl -H "Authorization: Bearer ${API_TOKEN}" https://app.liquidplanner.com/api/v1/workspaces/46891/tasks/${TASK_ITEM}
``

* https://developer.liquidplanner.com/docs/getting-started
* https://developer.liquidplanner.com/v1.0/reference
* https://app.liquidplanner.com/api/help/urls
# Project Management

* Initiation - where do you want to go?
  * Why is this project important?
  * What does the project entail?
  * What are the goals?
  * Who/what resources needs to be committed?
  * What does success look like?
* Planning - how do we do this?
  * Figure out the task order
  * Break the tasks down
  * Figure out the costs
  * Identify risks
  * Make a schedule
* Execution
* Monitoring and Control
  * Review the progress regularly
* Closure
  * Get signoff that everything is done
  * Hand over everything
  * Do a retrospective
