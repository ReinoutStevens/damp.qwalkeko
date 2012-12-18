(ns qwalkeko.clj.logic
  (:refer-clojure :exclude [== type])
  (:use [clojure.core.logic :as logic])
  (:use [qwalkeko.clj.reification :as reification] )
  (:use [damp.ekeko.workspace.reification :as workspace])
  (:require [damp.ekeko.jdt.reification :as jdt])
  (:import [qwalkeko HistoryProjectModel])
  (:use damp.qwal))



;;Declarative layer on top of reification
;;Can probably be sped up a bit
(defn rooto [root]
  "Logic goal that unifies root with a root version"
  (all
    (membero root (all-roots))))

(defn versiono [version]
  "Logic goal that unifies version with a metaversion"
  (all
    (membero version (all-versions))))


(defn endversiono [version]
  "Logic goal that unifies version with an endversion"
  (all
    (versiono version)
    (project [version]
      (== true (endversion? version)))))


(defn ensure-checkouto [version]
  "Logic goals that checks out the given version.
   Version must be grounded."
  (project [version]
    (== nil (ensure-checkout version))))

(defn ensure-deleteo [version]
  "Logic goal that deletes the given version.
   Version must be grounded"
  (project [version]
    (== nil (ensure-delete version))))


;;QWAL and Logic
(defn predecessoro [version pred]
  (all
    (== pred (predecessors version))))

(defn successoro [version succ]
  (all
    (== succ (successors version))))

(defn make-graph [project-model]
  {:predecessors predecessoro
   :successors successoro
   :project project-model
   })

(defn qendversiono [graph start end]
  (all
    (endversiono start)
    (== start end)))

(defn qrooto [graph start end]
  (all
    (rooto start)
    (== start end)))

(defn current-version []
  (damp.ekeko.ekekomodel/queried-project-models))
   