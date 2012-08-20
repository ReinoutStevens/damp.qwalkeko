(ns scrapperplugin.clj.reification
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:use [scrapperplugin.clj mli])
  (:import
    [scrapperplugin HistoryProjectModel]))


(defmacro with-metaproject [project-model [metaproject] & body]
  `(let [~metaproject (.getMetaProject ~project-model)]
     ~@body))

(defmacro with-eclipseproject [version [eclipse] & body]
  `(let [~eclipse (.getEclipseProject ~version)]
     ~@body))

;;HistoryProject
(defn versions [^HistoryProjectModel project-model]
  (with-metaproject project-model [metaproject]
    (seq (.getVersions metaproject))))

;;MetaProject stuff
(defn roots [project-model]
  (.getRoots project-model))


;;MetaVersion Stuff
(defn all-versions []
  (let [models (history-project-models)]
    (mapcat versions models)))

(defn all-roots []
  (mapcat
    (fn [model]
      (with-metaproject model [metaproject]
        (roots metaproject)))
    (history-project-models)))


(defn successors [version]
  (seq (.getSuccessors version)))

(defn predecessors [version]
  (seq (.getPredecessors version)))

(defn endversion? [version]
  (.isEndVersion version))

;;EclipseProject

(defn open [version]
  (with-eclipseproject version [eclipse]
    (.open eclipse nil)))

(defn close [version]
  (with-eclipseproject version [eclipse]
    (.close eclipse nil)))

(defn open-all [project-model]
  (map open (versions project-model)))

(defn close-all [project-model]
  (map close (versions project-model)))

