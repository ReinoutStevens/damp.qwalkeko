# QwalKeko

QwalKeko provides an extension of the logic program querying language [Ekeko](https://github.com/cderoove/damp.ekeko).
It allows developers and researchers to query the history and evolution of software projects stored in a VCS. 
To this end, QwalKeko constructs a graph of versions, in which each node represents a version, and successive versions are connected. Moving throughout this graph
is done using [Qwal](https://github.com/ReinoutStevens/damp.qwal), a graph query language implementing regular path expressions.
Ekeko is used to express the characteristics that have to hold inside each version. 
QwalKeko integrates nicely with [ChangeNodes](https://github.com/ReinoutStevens/changenodes), a tree differencer that outputs a minimal edit script.
It is used in QwalKeko to distill changes made to the source code of the queried software project.

Note that QwalKeko is a research artifact, and is subjected to sudden changes, bugs and features.


## Examples


### Pulled-up Method
In the first example we find a method ?method that has been moved to ?other-method in a later version. The two first lines provide the setup for our query.
The query starts in start-version, a clojure variable bound to a version. It ends in ?successive-version, a logic variable that will be bound after the query.
Lines 3 and 4 are executed in the startversion, and bind ?method to any method in that version. Line 5 marks a transition to a successor of the start version.
Lines 6 and 7 use a predefined rule that detects methods that are moved to another place.

````clojure
(l/qwalkeko* [?method ?other-method ?successive-version]
   (qwal/qwal graph start-version ?successive-version []
     (l/in-source-code [curr] 
       (jdt/ast :MethodDeclaration ?method))
     q=>
     (l/in-source-code [curr]
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
      (l/in-git-info [curr] 
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


### More examples
Examples can be found in the experiments and demo folder.
The two files you want to take a look at are [selenium.clj](https://github.com/ReinoutStevens/damp.qwalkeko/blob/master/src/qwalkeko/experiments/selenium.clj) and [icsme-selenium.clj](https://github.com/ReinoutStevens/damp.qwalkeko/blob/master/src/qwalkeko/demo/icsme_selenium.clj).



## Installation

You can find installation instructions [here](https://github.com/ReinoutStevens/damp.qwalkeko/wiki/Installation).

## Video Demonstration

You can find a video demonstration [here](https://github.com/ReinoutStevens/damp.qwalkeko/wiki/Video-Demonstration).

## Additional Reading
We have published papers describing our work using QwalKeko, and prior work using Absinthe (a similar idea implemented in SmallTalk, extending SOUL).

* 
* [Reasoning over the Evolution of Source Code using Quantified Regular Path Expressions](http://soft.vub.ac.be/Publications/2011/vub-soft-tr-11-13.pdf)
* [A History Querying Tool and its Application to Detect Multi-version Refactorings](http://soft.vub.ac.be/Publications/2013/vub-soft-tr-13-02.pdf)
* Prevalence and Maintenance of Automated Functional Tests for Web Application (to be published)
 
## Slides
You can find most of my presentations on [Slideshare](http://www.slideshare.net/stevensreinout).
