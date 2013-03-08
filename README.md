# QwalKeko

QwalKeko, formerly known as Scrapper, provides an extension of the logic program querying language [Ekeko](https://github.com/cderoove/damp.ekeko).
It allows developers and researchers to query the history and evolution of software projects stored in a VCS. 
To this end, QwalKeko constructs a graph of versions, in which each node represents a version, and successive versions are connected. Moving throughout this graph
is done using [Qwal](https://github.com/ReinoutStevens/damp.qwal), a graph query language implementing regular path expressions.
Ekeko is used to express the characteristics that have to hold inside each version. Last, QwalKeko provides its own set of "history predicates". For example,
the predicate ``same/2`` is a predicate that, given a program entity, finds the corresponding program entity in the current version.

Note that QwalKeko is a research artifact, and is subjected to sudden changes, bugs and features.


## Examples

### Pulled-up Method
In the first example we find a method ?method that has been moved to ?other-method in a later version. The two first lines provide the setup for our query.
The query starts in start-version, a clojure variable bound to a version. It ends in ?successive-version, a logic variable that will be bound after the query.
Lines 3 and 4 are executed in the startversion, and bind ?method to any method in that version. Line 5 marks a transition to a successor of the start version.
Lines 6 and 7 use a predefined rule that detects methods that are moved to another place.

````clojure
(logic/run* [?method ?other-method ?successive-version]
   (qwal/qwal graph start-version ?successive-version []
     (sessions/vcurrent [curr] 
       (jdt/ast :MethodDeclaration ?method))
     q=>
     (sessions/vcurrent [curr]
       (refactorings/method-moved ?method ?other-method))))
````

The implementation of the method-moved rule is written in regular core.logic.

````clojure
(defn method-moved [?moved ?to]
  (logic/all
    (is-removed ?moved)
    (jdt/ast :MethodDeclaration ?to)
    (logic/== ?moved ?to) ;;same signature
    (has-similar-body ?moved ?to)))
````

### Collaborating Authors
In the second example we are looking for sequences of versions in which two authors commit repeatedly commit one after the other. 


````clojure
(logic/run* [ ?end ?authorA ?authorB]
  (qwal/qwal graph root ?end []
    (qwal/q=>*) ;;skip an arbitrary number of versions
    (qwal/qtimes 2 ;;the following has to succeed at least 2 times
      ;;we use qcurrent instead of vcurrent as we only need meta-data, for which we dont have to checkout the version
      (qwal/qcurrent [curr] 
        (logic/== ?authorA (.getAuthor curr)))
      qwal/q=>
      (qwal/qcurrent [curr]
        (logic/== ?authorB (.getAuthor curr))
        (logic/!= ?authorA ?authorB)) ;;ensure both authors are different
      qwal/q=>)))
````

The query itself is not that difficult. We begin in the root version of our project.
We skip an arbitrary number of versions, as we do not know where the collaboration may be.
After skipping some versions, we look for a pattern that has to hold a number of times using the `qtimes` operator.
The pattern binds the author of the current version to ?authorA, transitions to the next version and binds that author to ?authorB.
We also ensure that both authors are different. Finally we transition to the next version.

## Installation
Installing QwalKeko is not for the faint of heart, and should/will be streamlined in the future. If you really want to get it up and running here is a brief guide
of what you have to do, but it is probably easier just to send me an email.

You can clone this repository and import it as an Eclipse plugin project. Note that you will also have to install [Ekeko](https://github.com/cderoove/damp.ekeko/),
[Keko](https://github.com/ReinoutStevens/damp.keko) and [PPA](http://www.sable.mcgill.ca/ppa/).


## Implementation
QwalKeko is implemented on Clojure, and combines [Qwal](https://github.com/ReinoutStevens/damp.qwal) with [Ekeko](https://github.com/cderoove/damp.ekeko).
We have opted for Clojure, as it provides high-level functional functions, has a declarative reasoning engine in [core.logic](https://github.com/clojure/core.logic)
and it is tightly integrated with the JVM. The latter allows us to easily query Eclipse projects and the corresponding JDT models.


### Evaluating statements in a specific version
QwalKeko provides a way to evaluate expressions in the "current" version, where the current version is specified using Qwal.
There are some ways to accomplish this:

We could provide an extra argument to the Ekeko-predicates that indicates the version in which the predicate needs to be evaluated.
The user simply passes the correct version as an argument.
The advantage of this approach is that the solution is very clean and easy to understand.
It also adheres to the declarative style, as the solutions of a goal are only dependent on the provided arguments.
The downside is that the user must always pass the version as an argument, while he already specified the version in Qwal.
Next, Ekeko is created to reason over a predefined set of projects, and thus these rules do not have a project (in our case version) as argument.
Automatically converting this would be difficult. Manually duplicating the functionality of Ekeko would just be silly.


We have opted for a solution that is harder to implement, but easier for the end user (who, in the end, should not care how the tool works).
Ekeko provides a dynamic variable to specify what projects are being queried. We have provided a Qwal-goal `vcurrent` similar to `qcurrent`,
except that it also sets this variable to the current version. This approach works for as long as no backtracking is required.
Backtracking may result in moving back to a previous version and re-evaluating a goal. This goal would be evaluated in an incorrect version,
as the dynamic variable is no longer correctly set. In order to solve this issue the goal `vcurrent` also adds a goal which is executed at the end
of each version (and thus this goal is called first upon backtracking). This goal initially succeeds, but upon backtracking
(and thus when we presumably come from a different version) it re-sets the dynamic variable and fails, ensuring further backtracking will occur.
 



## Additional Reading
This README still needs to be improved. We have published papers describing our work using QwalKeko, and prior work using Absinthe (a similar idea implemented in SmallTalk, extending SOUL).


* [Reasoning over the Evolution of Source Code using Quantiﬁed Regular Path Expressions](http://soft.vub.ac.be/Publications/2011/vub-soft-tr-11-13.pdf)
* [A History Querying Tool and its Application to Detect Multi-version Refactorings](http://soft.vub.ac.be/Publications/2013/vub-soft-tr-13-02.pdf)