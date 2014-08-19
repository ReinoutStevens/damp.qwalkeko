(ns 
  ^{:doc "Integration tests for QwalKeko."
    :author "Reinout Stevens, Coen De Roover"}
  test.damp.qwalkeko.integration
  (:refer-clojure :exclude [== type declare record?])
  (:require [clojure.core.logic :exclude [is] :as cl])
  (:require [qwalkeko.clj 
             [logic :as l]
             [reification :as r]
             [graph :as graph]
             [ast :as ast]
             [changenodes :as change]
             ]) ;TODO: should define one top-level qwalkeko file that does this and includes an example repl sessions
  (:require [test.damp [ekeko :as test]])
  (:use clojure.test))


;; Example tests

;(deftest
;  ^{:doc "For all nodes n, n should be included in the matches for snippet(n)."}
;  exactmatch-node 
;  (let [nodes 
;        (map first 
;             (damp.ekeko/ekeko [?ast] (l/fresh [?kind] (ast/ast ?kind ?ast))))
;        snippets
;        (map matching/jdt-node-as-snippet nodes)]
;    (doseq [[node snippet] (map vector nodes snippets)]
;      (is (some #{node} (map first (snippets/query-by-snippet snippet)))))))
    

(deftest
  dummytest
  (is 1 1))
                    
;; Test suite
;; ----------

(deftest
   test-suite 
   (let [testproject "TestCase-JDT-CompositeVisitor"]
     ;(test/against-project-named testproject false exactmatch-node)
     (dummytest)))

(defn 
  test-ns-hook 
  []
  (test/with-ekeko-disabled test-suite))


(comment  
  ;;Example repl session 
 (run-tests))
 