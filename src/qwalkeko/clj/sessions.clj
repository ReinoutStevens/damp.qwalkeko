(ns qwalkeko.clj.sessions
  (:use [clojure.core.logic :as logic])
  (:use [qwalkeko.clj.reification])
  (:use [qwalkeko.clj.logic]))
 

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