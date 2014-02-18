(ns qwalkeko.clj.logic
  (:refer-clojure :exclude [== type])
  (:require [clojure.core.logic :as logic])
  (:require [qwalkeko.clj.reification :as reification])
  (:require [qwalkeko.clj.ast :as ast])
  (:require [qwalkeko.clj.sessions :as sessions]) 
  (:require [damp.ekeko.workspace.reification :as workspace])
  (:require [damp.ekeko.jdt.ast :as jdt])
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





;;QWAL and Logic
(defn predecessoro [version pred]
  (logic/all
    (logic/== pred (reification/predecessors version))))

(defn successoro [version succ]
  (logic/all
    (logic/== succ (reification/successors version))))


(defn qendversiono [graph start end]
  (logic/all
    (endversiono start)
    (logic/== start end)))

(defn qrooto [graph start end]
  (logic/all
    (rooto start)
    (logic/== start end)))


;;Graph Helpers
(defn make-graph [project-model]
  "basic graph that considers all the versions"
  {:predecessors predecessoro
   :successors successoro
   :project project-model
   })


;;Graph Decorators that filter out certain versions
;;They all take a graph as argument and return a new graph
;;which can be decorated again
(defn make-graph-between [a-graph start end]
  {:successors
   (fn [version succ]
     (logic/all
       (logic/conda
         [(logic/== end version)
          (logic/== end '())]
         [((:successors a-graph) version succ)])))
   :predecessors
   (fn [version pred]
     (logic/all
       (logic/conda
         [(logic/== start version)
          (logic/== pred '())]
         [((:predecessors a-graph) version pred)])))
   :project (:project a-graph)
   :parent a-graph})


;;Rules over FileInfo

(defn fileinfo [?fileinfo version]
  (logic/all
    (logic/membero ?fileinfo (reification/file-infos version))))


(defn fileinfo|status [?fileinfo ?status version]
  (logic/all
    (fileinfo ?fileinfo version)
    (logic/project [?fileinfo]
                   (logic/featurec ?fileinfo {:status ?status}))))

(defn fileinfo|add [?fileinfo version]
  (logic/all
    (fileinfo|status ?fileinfo :add version)))

(defn fileinfo|edit [?fileinfo version]
  (logic/all
    (fileinfo|status ?fileinfo :edit version)))

(defn fileinfo|delete [?fileinfo version]
  (logic/all
    (fileinfo|status ?fileinfo :delete version)))


(defn fileinfo|file [?fileinfo ?file version]
  (logic/all
    (fileinfo ?fileinfo version)
    (logic/project [?fileinfo]
                   (logic/featurec ?fileinfo {:file ?file}))))



;;Helper macros
(defmacro qwalkeko* [ [ & vars] & goals]
  `(binding [damp.ekeko.ekekomodel/*queried-project-models*  (atom '())]
     (doall
       (logic/run* [~@vars] ~@goals))))


(defmacro qwalkeko [results [ & vars] & goals]
    `(binding [damp.ekeko.ekekomodel/*queried-project-models*  (atom '())]
       (doall
         (logic/run ~results [~@vars] ~@goals))))


(defmacro in-current-meta [[meta] & goals]
  `(qwal/qcurrent [~meta] ~@goals))

(defmacro in-current [[curr] & goals ]
  `(sessions/vcurrent [~curr] ~@goals))