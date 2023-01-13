# Expert systems

Systems that use rules, facts, and inferrence to determine an outcome. Their success is debatable, with knowledge acquisition and actually getting the experts to write down the facts/rules being the chief problems. This could be useful for personal projects, but ultimately requires competence and time.


# Books

- Introduction To Expert Systems (3rd Edition) 3rd Edition by Peter Jackson
- Expert Systems: Principles and Programming, Fourth Edition by Joseph C. Giarratano


# Declarative programming


## When/Then vs If/Then

When/Then is where you state what needs to be done, not how it needs to be done. This allows delegation of the ordering and figuring out dependencies to another system.

```
When
    Conditions
Then
    Actions
```

Rules are represented by the **antecedent** (aka left-handed side) which is the conditions of a rule. The **consequent** (aka right hand side) are the actions of the rule.


## Forward-chaining and backwards-chaining

**Forward-chaining systems** start with a premises and work forward to the conclusions supported by those premises. **Backward-chaining systems** start with a conclusion to be proved and work backward to the premises that would support the conclusion. There are also systems that are a hybrid of both.