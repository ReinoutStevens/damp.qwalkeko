# QwalKeko

QwalKeko, formerly known as Scrapper, provides an extension of the logic program querying language [Ekeko](https://github.com/cderoove/damp.ekeko).
It allows developers and researchers to query the history and evolution of software projects stored in a VCS. 
To this end, QwalKeko constructs a graph of versions, in which each node represents a version, and successive versions are connected. Moving throughout this graph
is done using [Qwal](https://github.com/ReinoutStevens/damp.qwal), a graph query language implementing regular path expressions.
Ekeko is used to express the characteristics that have to hold inside each version. Last, QwalKeko provides its own set of "history predicates". For example,
the predicate ``same/2`` is a predicate that, given a program entity, finds the corresponding program entity in the current version.

Note that QwalKeko is a research artifact, and is subjected to sudden changes, bugs and features.


## Examples

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


## Implementation

TODO :)


## Additional Reading
This README still has to be improved. We have published papers describing our work using QwalKeko, and prior work using Absinthe (a similar idea implemented in SmallTalk, extending SOUL).

* [Reasoning over the Evolution of Source Code using Quantiﬁed Regular Path Expressions](http://soft.vub.ac.be/Publications/2011/vub-soft-tr-11-13.pdf)
* A History Querying Tool and its Application to Detect Multi-version Refactorings, To be published, accepted on CSMR