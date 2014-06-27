(ns qwalkeko.demo.icsme-selenium
  (:require [clojure.core.logic :as logic])
   (:require [qwalkeko.clj.logic :as l])
   (:require [qwalkeko.clj.reification :as r])
   (:require [qwalkeko.clj.graph :as graph])
   (:require [qwalkeko.clj.ast :as ast])
   (:require [qwalkeko.clj.changenodes :as change])
   (:require [damp.ekeko.jdt
              [ast :as jdt]
              [convenience :as conv]])
   (:require [damp.qwal :as qwal]))



;;;;Ensure we have properly setup a project
;; we will use the motech project for this example
;; it can be found here: https://github.com/INCF/eeg-database
;; its clone url is: https://github.com/INCF/eeg-database.git
;; it's not the most interesting repository, but it does not have that many commits so queries should not take
;; too long for a demo.


;; clone the repository locally, and add it to eclipse by going to the qwalkeko menu and selecting the .git folder
;; make sure to first add the history nature before adding the ekeko nature, as the history nature changes
;; what kind of model is being created



;; first of all we need to get a graph object we can use
(def a-model (first (damp.ekeko.ekekomodel/all-project-models)))
(def a-graph (graph/convert-model-to-graph a-model))
(def a-root (first (:roots a-graph)))


;; identify selenium files

