(ns scrapperplugin.clj.logic
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:use scrapperplugin.clj.reification)
  (:use scrapperplugin.clj.mli)
  (:use damp.ekeko.workspace.reification)
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
   :nodes (versions project-model)
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


(def ^:dynamic *current-session*)

(defn open-session []
  #{})

(defn close-session [session]
  (doall
    (map ensure-delete session)))


(defmacro in-session [ & body]
  (let [result (gensym "result")]
  `(binding [*current-session* (open-session)]
     (let [~result (do ~@body)]
       (close-session *current-session*)
       ~result))))


(defmacro scurrent [[version] & goals]
  (let [graph (gensym "graph")
        next (gensym "next")]
  `(fn [~graph ~version ~next]
     (when (not (bound? #'*current-session*))
       (throw (new scrapperplugin.SessionUnboundException)))
     (project [~version]
       (all
         (ensure-checkouto ~version)
         (== nil ;;isnt she pretty?
             (do 
               (set! *current-session* (conj *current-session* ~version))
               nil))
         ~@goals
         (== ~version ~next))))))


   