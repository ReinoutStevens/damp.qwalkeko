(ns qwalkeko.clj.logic
  (:refer-clojure :exclude [== type])
  (:require [clojure.core.logic :as logic])
  (:require [qwalkeko.clj.reification :as reification] )
  (:require [damp.ekeko.workspace.reification :as workspace])
  (:require [damp.ekeko.jdt.reification :as jdt])
  (:import [qwalkeko HistoryProjectModel])
  (:require [damp.qwal :as qwal]))



;;Declarative layer on top of reification
;;Can probably be sped up a bit
(defn rooto [root]
  "Logic goal that unifies root with a root version"
  (logic/all
    (logic/membero root (reification/all-roots))))

(defn versiono [version]
  "Logic goal that unifies version with a metaversion"
  (logic/all
    (logic/membero version (reification/all-versions))))



(defn was-branchedo [version]
  (logic/all
    (logic/== true (reification/was-branched version))))


(defn was-mergedo [version]
  (logic/all
    (logic/== true (reification/was-merged version))))

(defn endversiono [version]
  "Logic goal that unifies version with an endversion"
  (logic/all
    (versiono version)
    (logic/project [version]
      (logic/== true (reification/endversion? version)))))


(defn ensure-checkouto [version]
  "Logic goals that checks out the given version.
   Version must be grounded."
  (logic/project [version]
    (logic/== nil (reification/ensure-checkout version))))

(defn ensure-deleteo [version]
  "Logic goal that deletes the given version.
   Version must be grounded"
  (logic/project [version]
    (logic/== nil (reification/ensure-delete version))))


;;QWAL and Logic
(defn predecessoro [version pred]
  (logic/all
    (logic/== pred (reification/predecessors version))))

(defn successoro [version succ]
  (logic/all
    (logic/== succ (reification/successors version))))



(defn make-graph [project-model]
  {:predecessors predecessoro
   :successors successoro
   :project project-model
   })

(defn qendversiono [graph start end]
  (logic/all
    (endversiono start)
    (logic/== start end)))

(defn qrooto [graph start end]
  (logic/all
    (rooto start)
    (logic/== start end)))


(defn current-version []
  (damp.ekeko.ekekomodel/queried-project-models))
   