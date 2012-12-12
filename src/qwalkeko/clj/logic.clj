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
  (all
    (membero root (all-roots))))

(defn versiono [version]
  (all
    (membero version (all-versions))))


(defn endversiono [version]
  (all
    (versiono version)
    (project [version]
             (== true (endversion? version)))))


(defn ensure-checkouto [version]
  (== nil (ensure-checkout version)))

(defn ensure-deleteo [version]
  (== nil (ensure-delete version)))


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
   :goal-solver
   (fn [graph current next goal]
     (binding [damp.ekeko.ekekomodel/*queried-project-models* current]
       (all
         (goal graph current next))))})


(defn qendversiono [graph start end]
  (all
    (endversiono start)
    (== start end)))

(defn qrooto [graph start end]
  (all
    (rooto start)
    (== start end)))

(defn current-version []
  damp.ekeko.ekekomodel/*queried-project-models*)

   