(ns qwalkeko.clj.reification
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:use [damp.ekeko.ekekomodel :as model])
  (:use [damp.ekeko.workspace.projectmodel :as pmodel])
  (:import
    [qwalkeko HistoryProjectModel]))


(defmacro with-metaproject [project-model [metaproject] & body]
  `(let [~metaproject (.getMetaProject (.getMetaProduct ~project-model))]
     ~@body))

(defmacro with-eclipseproject [version [eclipse] & body]
  `(let [~eclipse (.getEclipseProject ~version)]
     ~@body))


(defn history-project-models []
  (filter (fn [project-model]
            (instance? HistoryProjectModel project-model))
       (model/queried-project-models)))



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


(defn was-merged [version]
  (> (count (predecessors version)) 1))

(defn was-branched [version]
  (> (count (successors version)) 1))
                           

(defn endversion? [version]
  (.isEndVersion version))

(defn ensure-checkout [version]
  (.openAndCheckoutIfNeeded version))

(defn ensure-delete [version]
  (.closeAndDeleteIfNeeded version))


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

;;Changed Files
(defn file-infos [version]
  (seq (.getChangedFileInfos version)))


(defn file-info-changed? [changedfileinfo]
  (.wasChanged changedfileinfo))

(defn file-info-edited? [changedfileinfo]
  (.wasEdited changedfileinfo))


(defn changed-file-infos [version]
  (filter file-info-changed? (file-infos version)))

(defn file-changed? [path version]
  (let [infos (file-infos version)
        names (map #(.getFileName %) infos)
        ;;path contains the project name as well, which we have to ignore
        cutted-path (.toString (.removeFirstSegments path 1))]
    (some #(= cutted-path %) names)))


(defn get-file-name [changedfileinfo]
  (.getFileName changedfileinfo))

(defn changed-files [version]
  (map get-file-name 
       (filter file-info-changed? (file-infos version))))

(defn edited-files [version]
  (map get-file-name
       (filter file-info-edited? (file-infos version))))

