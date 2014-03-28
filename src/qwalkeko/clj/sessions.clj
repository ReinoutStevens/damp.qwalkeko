(ns qwalkeko.clj.sessions
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :as logic])
  (:require [qwalkeko.clj.graph :as graph])
  (:require [qwalkeko.clj.reification :as reification])
  (:require [damp.ekeko.workspace.projectmodel :as projectmodel])
  (:require [damp.ekeko.workspace.workspace :as workspace]))
 
;;Metaversions
(defn ensure-checkouto [version]
  "Logic goals that checks out the given version.
   Version must be grounded."
  (logic/project [version]
    (logic/== nil (graph/ensure-checkout version))))


(defn ensure-deleteo [version]
  "Logic goal that deletes the given version.
   Version must be grounded"
  (logic/project [version]
    (logic/== nil (graph/ensure-delete version))))



(defn open-session []
  #{})

(defn close-session [session]
  (doall
    (map graph/ensure-delete session)))


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
       (throw (new qwalkeko.SessionUnboundException)))
     (logic/project [~version]
       (logic/all
         (ensure-checkouto ~version)
         (logic/== nil ;;isnt she pretty?
             (do 
               (set! *current-session* (conj *current-session* ~version))
               nil))
         ~@goals
         (logic/== ~version ~next))))))


(defn set-current [version]
  (logic/all
    (logic/== true 
        (do
          (reset! damp.ekeko.ekekomodel/*queried-project-models* 
                (seq (.getProjectModel (damp.ekeko.EkekoModel/getInstance) (graph/eclipse-project version))))
          true))))


(defmacro vcurrent [[version] & goals]
  "Opens and sets the current version, and will evaluate all the goals in the current version.

  To ensure that the goals are always evaluated in the correct version (for example when backtracking) an additional
  goal is added to the end that resets the current version."
  `(fn [graph# ~version next#]
     (logic/project [~version]
                    (logic/all
                      (ensure-checkouto ~version)
                      (set-current ~version)
                      ~@goals
                      (logic/== ~version next#)
                      ;;we directly create a core.logic goal that just returns the substitutions
                      ;;as a sideeffect we restore the current version, used upon backtracking
                      ;;when you get weird results, start looking here :)
                      (fn [subs#]
                        (qwalkeko.clj.sessions/set-current ~version)
                        subs#)))))