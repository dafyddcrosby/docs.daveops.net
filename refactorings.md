---
title: refactorings

---

# Refactorings

* Extract method -> move block of code into a separate function
* Move method -> move function to class that uses it most, removing or delegating it in the old class
* Replace temp with query -> Replace a temporary variable with a query
* Form template method -> Get the steps of two similar subclasses into methods with the same signature (to reduce duplication)
* Replace type with state/strategy -> Replace type code with a state object
* Replace conditional with polymorphism -> Move each leg of the conditional to an overriding method in a subclass. Make the original method abstract.
* Self encapsulate field -> Using get and set methods to access variables


