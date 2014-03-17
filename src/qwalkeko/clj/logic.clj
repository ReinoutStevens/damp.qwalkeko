(ns qwalkeko.clj.logic
  (:refer-clojure :exclude [== type])
  (:require [clojure.core.logic :as logic])
  (:require [qwalkeko.clj.reification :as reification])
  (:require [qwalkeko.clj.graph :as graph])
  (:require [qwalkeko.clj.ast :as ast])
  (:require [qwalkeko.clj.sessions :as sessions]) 
  (:require [damp.ekeko.workspace.reification :as workspace])
  (:require [damp.ekeko.jdt.ast :as jdt])
  (:import [qwalkeko HistoryProjectModel])
  (:require [damp.qwal :as qwal]))




;;Rules over FileInfo

(defn fileinfo [?fileinfo version]
  (logic/all
    (logic/membero ?fileinfo (graph/file-infos version))))

(defn fileinfos [?fileinfos version]
  (logic/all
    (logic/== ?fileinfos (graph/file-infos version))))

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

;;general version information
(defn revisionnumber [?number version]
  (logic/all
    (logic/== ?number (graph/revision-number version))))



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