(ns qwalkeko.clj.sessions
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :as l])
  (:use [qwalkeko.clj.reification :as reification])
  (:use [damp.ekeko.workspace.projectmodel :as projectmodel])
  (:use [damp.ekeko.workspace.workspace :as workspace]))
 

(def ^:dynamic *current-session*)

(defn ensure-checkouto [version]
  "Logic goals that checks out the given version.
   Version must be grounded."
  (l/project [version]
    (l/== nil (reification/ensure-checkout version))))

(defn ensure-deleteo [version]
  "Logic goal that deletes the given version.
   Version must be grounded"
  (l/project [version]
    (l/== nil (reification/ensure-delete version))))


(defn open-session []
  #{})

(defn close-session [session]
  (doall
    (map reification/ensure-delete session)))


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
     (l/project [~version]
       (l/all
         (ensure-checkouto ~version)
         (l/== nil ;;isnt she pretty?
             (do 
               (set! *current-session* (conj *current-session* ~version))
               nil))
         ~@goals
         (l/== ~version ~next))))))


(defn set-current [version]
  (l/all
    (l/== true 
        (do
          (swap! damp.ekeko.ekekomodel/*queried-project-models* 
                 (fn [previous] 
                   (seq (.getProjectModel (damp.ekeko.EkekoModel/getInstance) (.getEclipseProject version)))))
          true))))


(defn wait-for-builds-to-finisho []
  (l/all
    (l/== nil (workspace/workspace-wait-for-builds-to-finish))))

(defmacro vcurrent [[version] & goals]
  "Opens and sets the current version, and will evaluate all the goals in the current version.

  To ensure that the goals are always evaluated in the correct version (for example when backtracking) an additional
  goal is added to the end that resets the current version."
  `(fn [graph# ~version next#]
     (l/project [~version]
                    (all
                      (ensure-checkouto ~version)
                      (set-current ~version)
                      (wait-for-builds-to-finisho)
                      ~@goals
                      (l/== ~version next#)
                      ;;we directly create a core.logic goal that just returns the substitutions
                      ;;as a sideeffect we restore the current version, used upon backtracking
                      ;;when you get weird results, start looking here :)
                      (fn [subs#]
                        (qwalkeko.clj.sessions/set-current ~version)
                        subs#)))))