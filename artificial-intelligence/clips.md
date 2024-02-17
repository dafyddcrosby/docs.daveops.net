
# CLIPS

<https://www.clipsrules.net/>

[source downloads](https://sourceforge.net/projects/clipsrules/files/CLIPS)

Invented in NASA in 1985

Written in C, provides a LISPy language called COOL for rule definitions

[CLIPS wikipedia](https://en.wikipedia.org/wiki/CLIPS)


# Starting up Emacs CLIPS mode + REPL

```elisp
(require 'inf-clips)
(setq inferior-clips-program "clips")
```

| M-x run-clips | start clips in a buffer |
| C-c C-l       | load lisp file          |
| C-c r         | get rules               |
| C-c f         | get facts               |
| C-c i         | reset                   |
| C-c g         | run                     |
| C-c .         | previous argument       |

C-x C-e will evaluate last sexp in a buffer with clips mode


# high-level architecture

1. fact-list and instance-list
2. knowledge-base (rule-base)
3. inference engine

```clips
; exit program
(exit)

; assert a fact 
(assert (duck))

; get list of facts
(facts)

; clear memory
(clear)

; run rules against the facts
(run)

; watch new facts roll in/out (disable w/ (unwatch facts))
(watch facts)
; can also watch instances/slots/rules/activations/messages
; message-handlers/generic-functions/methods/deffunctions
; compilations/statistics/globals/focus and all

; get activations
(agenda)
```


# vocabulary

**then symbol** is =>

**salience** is the order/priority

**activations** are rules that match a pattern entity

**conflict resolution** is when the highest salience activation fires


## refraction

**refraction** is when a rule cannot fire again after a period of time


## truth maintenance

truth maintenance is when information is altered to conform to reality, and minimize conflicts with the real world.


# facts

```clips
(assert (duck)) ; assert a fact 

(facts) ; get list of facts

; clear memory
(clear)

; reset just the facts, running any deffacts+definstances
(reset)

; deffacts are a way of adding a bunch of facts
(deffacts walk "Some facts about walking"
      ; status fact to be asserted
      (status walking)
      ; walk-sign fact to be asserted
      (walk-sign walk))

; remove walk deffacts from memory
(undeffacts walk)

(list-deffacts)
(ppdeffacts)
```

A **control fact** is a fact that's used to control another rule. If you want to re-assert a changed list, you would need to retract the old list.


# fields

CLIPS is case-sensitive.


## symbols

Special characters: " ( ) & | <~ ; ? $

The "&", "|", and "~" may not be used as stand-alone symbols or as any part of a symbol.

Some characters act as delimiters by ending a symbol. The following characters act as delimiters for symbols.

- any non-printable ASCII character, including spaces, carriage returns, tabs, and linefeeds
- double quotes, "
- opening and closing parentheses, ()
- ampersand, &
- vertical bar, |
- less than, <. Note that this may be the first character of a symbol
- tilde, ~
- semicolon, ; indicates start of a comment, a carriage return ends it
- ? and $? may not begin a symbol but may be inside it

The semicolon acts as the start of a comment in CLIPS.


## strings

"foobar"


## numeric fields

integer and float

floats are double-precision


# relations

It's good practice to have the first field describe the relationship of the following fields

```clips
(animal-is chicken)
(animal-is rooster)
(animals chicken rooster)
```


# retraction

```clips
; retract 3rd fact
(retract 3)
; retract 1st and 2nd fact
(retract 1 2)
```


# defrules

```clips
(rules) ; list rules

(defrule foo
      (animal-is duck) ; pattern
      (animal-is ~boar) ; ~ constraint checks is a logical NOT
      (animal-is duck|goose) ; | is a logical OR
      (animal-is ?bird&duck|goose) ; & is a logical AND that is used for capturing value
							       ; in this case, ?bird would be duck or goose
      (animal-is ?bird&~mammal&~amphibian) ; & is good for ~ as well
      => ; you need this arrow!
      (assert (sound-is quack))) ; action

; for arguments
(defrule addition
      (numbers ?x ?y)
      =>
      (assert (answer-plus (+ ?x ?y)))
      (bind ?answer (+ ?x ?y)) ; if you need to bind a variable (not create a fact),
						       ; you can use bind
)

; pretty-prints the rule
(ppdefrule foo)

; remove defrule from memory
(undefrule foo)

; see what facts match the rule
(matches foo)
```

pattern entity - a fact or instance of a user-defined class

rule names operate in a global namespace, but there are modules


# system

```clips
; Files
(save "file.clp") ; save rules to file
(load "file.clp") ; load file
(bsave "bfile.clp") ; binary save
(bload "bfile.clp") ; binary load

(save-facts "data.clp") ; save facts
(load-facts "data.clp") ; load facts

(batch) ; TODO
(system) ; TODO

; print to STDOUT
(printout t "hello world" crlf)

; turn logs on and off
(dribble-on "logfile")
(dribble-off "logfile")
```


# strategy

CLIPS offers seven different modes of conflict resolution: depth, breadth, LEX, MEA, complexity, simplicity, and random. It’s difficult to say that one is clearly better than another without considering the specific application. Even then, it may be difficult to judge which is “best.”

The depth strategy is the standard default strategy of CLIPS.


# breakpoints

```clips
; break on rule foo
(set break foo)
(remove-break foo)
(show-breaks)
```


# variables

```clips
(assert (food snacks))
(assert (food veggies))
(defrule eat-snacks
      ?factnum <- (food snacks)
      =>
      (printout t "ate snacks" crlf)
      (retract ?factnum)) ; remove a fact with a variable

(defrule eat-anything
      ?factnum <- (food ?yum)
      =>
      (printout t "ate " ?yum crlf)
      (retract ?factnum))
; you can also do a single-filed wildcard with `?`
; multi-field wildcards are `$?`
```

A variable is only used within a rule, so if ?name is used in multiple patterns, it is the same value.


# deftemplate

Like a struct, with named fields called **slots**

unordered (use named fields), sort of like structs

multislot with single value != a single-slot

```clips
; name of deftemplate relation
(deftemplate prospect
      ; optional comment in quotes
      "vital information"
      ; name of field
      (slot name
	; type of field
	(type STRING)
	; default value of field name
	(default ?DERIVE)) ; derives default value for type
      ; name of field
      (slot assets
	; type of field
	(type SYMBOL)
	; default value of field assets
	(allowed-symbols
	      poor rich wealthy loaded)
	(default rich))
      ; name of field
      (slot age
	; type. NUMBER can be INTEGER or FLOAT
	(type NUMBER)
	; default value of field age
	(default 80)))
```

| allowed-symbols  | rich filthy-rich loaded |
| allowed-strings  | "Dopey" "Dorky" "Dicky" |
| allowed-numbers  | 1 2 3 4.5 -2.001 1.3e-4 |
| allowed-integers | -100 53                 |
| allowed-floats   | -2.3 1.0 300.00056      |
| allowed-values   | "Dopey" rich 99 1.e9    |


# deffunction

deffunctions are plain ol' functions

```clips
(deffunction foo
      "this is a comment"
      (?arg1 ?arg2 $?argN) ; arguments
      (+ ?arg1 ?arg2)      ; actions
      )

(list-deffunctions) ; list deffunctions
(ppdeffunction foo) ; pretty-print deffunction
(undeffunction foo) ; undefine deffunction
```


# reading input

```clips
(defrule ask-color
 "Ask for favorite color"
 =>
	      (printout t "favorite color?" crlf)
	      (assert (favorite-color (read))))

(defrule ask-food
 =>
      (printout t "favorite food?" crlf)
      (bind ?string (readline))
      (assert-string (str-cat "(" ?string ")")))
```


# Rete Algorithm

Latin for *net*.

To be efficient with Rete algorithm, in order of efficiency:

1. Control facts
2. Fewest matching facts
3. Most specific patterns
4. Unbound variables/wildcards
5. Patterns that are often retracted and asserted (aka **volatile patterns**)


# logical values and functions

TRUE and FALSE (must be capitalized)

`and`, `or`, `not` are functions


## predicate functions

| (evenp <arg>)       | even number           |
| (floatp <arg>)      | floating-point number |
| (integerp <arg>)    | integer               |
| (lexemep <arg>)     | symbol or string      |
| (multifieldp <arg>) | multifield value      |
| (numberp <arg>)     | float or integer      |
| (oddp <arg>)        | odd number            |
| (pointerp <arg>)    | external address      |
| (stringp <arg>)     | string                |
| (symbolp <arg>)     | symbol                |


# dependencies

```clips
; TODO actual examples
(dependencies)
(dependents)
```


# defglobal

```clips
(defglobal pi 3.14) ; this would be more efficient than a fact
```


# randomness

```clips
(random) ; returns a pseudorandom number
```


# CLIPS Object-Oriented Language (COOL)

CLIPS supports abstraction, inheritance, encapsulation, polymorphism, and dynamic binding

Root class is OBJECT, USER is a subclass of OBJECT.

Generally speaking, all user-defined classes should be a subclass of USER, as it has all the needed handlers

CLIPS supports only `is-a` links

Behavior is inherited, which reduces **Verification and Validation** of handlers

```clips
(defclass WRESTLER (is-a USER)) ; USER is the direct superclass
								; (which means OBJECT is indirect superclass)

(defclass VEHICLE (is-a USER))
(defclass BIKE (is-a VEHICLE)
      (slot color (default blue)) ; you can add slot facets to a class
      (slot ID (default (gensym*))) ; gensym creates IDs unique through entire CLIPS run
      (multislot bellsound (default ding ding))
      )
(defclass SWITCHBLADE (is-a USER))
(defclass FOLDINGBIKE (is-a BIKE SWITCHBLADE)) ; multiple inheritance

(subclassp BIKE OBJECT) ; return TRUE
(superclassp VEHICLE BIKE) ; return TRUE

(list-defclasses) ; list all defclasses
(browse-classes) ; hierarchical list of classes, * means multiple inheritance
(ppdefclass) ; pretty print internal structure
(describe-class) ; additional info on class
(class-abstractp) ; predicate function returns TRUE if abstract

; An instance name has brackets
(symbol-to-instance-name Dorky_Duck) ; return [Dorky_Duck]
(symbol-to-instance-name (sym-cat Dorky "_" Duck)) ; ditto
(instance-name-to-symbol [Dorky_Duck]) ; return Dorky_Duck

(make-instance [Asama] of BIKE) ; returns [Asama]
(instances) ; get all available instances

; definstances works like deffacts
(definstances BIKE_OBJECTS
		      "My Bikes"
		      (Asama of BIKE)
		      (Brompton of BIKE))

(send [Brompton] unfold) ; send unfold message to [Brompton] instance

; accessors
(send [Brompton] put-serial "123")
(send [Brompton] get-serial)

```

You can't modify the slots of a class, if you want to change it you need to delete all instances of the class and use the defclass with the same name.

Class etiquette

1. hierarchy should be in specialized logical increments using is-a links
2. a class is unnecessary if it only has one instance
3. class should not be named for an instance and vice versa

| class-slot-exists | Returns TRUE if the class slot exists                             |
| slot-existp       | Returns TRUE if the instance slot exists                          |
| slot-boundp       | Returns TRUE if the specified slot has a value                    |
| instance-address  | Returns machine address at which the specified instance is stored |
| instance-name     | Returns the name given an instance                                |
| instancep         | Returns TRUE if its argument is an instance                       |
| instance-addressp | Returns TRUE if its argument is an instance address               |
| instance-namep    | Returns TRUE if its argument is an instance name                  |
| instance-existp   | Returns TRUE if the instance exists                               |
| list-definstances | Lists all the definstances                                        |
| ppdefinstances    | Pretty-prints the definstance                                     |
| watch instances   | Allows you to watch instances being created and deleted           |
| unwatch instances | Turns off watching instances                                      |
| save-instances    | Saves instances to a file                                         |
| load-instances    | Loads instances from a file                                       |
| undefinstances    | Deletes the named definstance                                     |


## facets

| default and default-dynamic | Set initial values for slots                   |
| cardinality                 | Number of multifield values                    |
| access                      | Read-write, read-only, initialize-only access  |
| storage                     | Local slot in instance or shared slot in class |
| propagation                 | Inherit or no inherit slots                    |
| source                      | Composite or exclusive inheritance             |
| override-message            | Indicates message to send for slot override    |
| create-accessor             | Create put- and get- handler                   |
| visibility                  | Public or private to defining class only       |
| reactive                    | Changes to a slot trigger pattern-matching     |

functions:

| slot-replace$ | Replace the specified range |
| slot-insert$  | Insert the specified range  |
| slot-delete$  | Delete the specified range  |

```clips
(slot-writablep) ; returns TRUE if writable
(slot-initablep) ; returns TRUE if initializable
```

(reset) is **cold-initialization** since only definstances are created

(initialize-instance) (which resets an instances default values) is **warm-initialization**


# message handlers

> (defmessage-handler <class-name> <message-name> [handler-type] [comment] (<parameters>\* [wildcard-parameter]) <action>\*)

```clips
(defclass BIKE (is-a USER))
(defmessage-handler BIKE ring-bell ()
	      (printout t "ding ding" crlf))
(make-instance [Asama] of BIKE)
(send [Asama] ring-bell) ; returns "ding ding\n"
(preview-send BIKE ring-bell) ; see what message handlers could potentially be involved

; handler to print slots
(defmessage-handler USER print-slots () (ppinstance))
```

You can read dynamically read a slot's value in a message handler with ?self:<slot<sub>name</sub>>


# OOP classes

Inheritance by Specialization -> analysis Inheritance by Generalization -> synthesis (ie composition)

```clips
(defclass LINE (is-a POINT)
      (slot point1
	(default-dynamic
	      (make-instance (gensym*) of POINT)) ; acts as pointer to different POINT instances
	(propagation no-inherit)) ; no-inherit means only direct instances has class slot
      (slot point2
	(default-dynamic
	      (make-instance (gensym*) of POINT))
	(propagation no-inherit))
      (message-handler find-point)
      (message-handler print-points)
      (message-handler find-distance)
      (message-handler print-distance))
```

| any-instancep                | Determines if one or more instance-sets satisfy a query                                    |
| find-instance                | Returns the first instance-set that satisfies a query                                      |
| find-all-instances           | Groups and returns all instance-sets which satisfy a query                                 |
| do-for-instance              | Performs an action for the first instance-set which satisfies a query                      |
| do-for-all-instances         | Performs an action for every instance-set which satisfies a query as they are found        |
| delayed-do-for-all-instances | Groups all instance-sets which satisfy a query and then iterates an action over this group |


# defmodules

> (defmodule <module-name> [<comment>] <port-specification>\*)
> 
> <port-specification> ::= (export <port-item>) | (import <module-name> <port-item>) <port-item>::= ?ALL | ?NONE | <port-construct> ?ALL | <port-construct> ?NONE | <port-construct> <construct-name>+
> 
> <port-construct>::= deftemplate | defclass | defglobal | deffunction | defgeneric
