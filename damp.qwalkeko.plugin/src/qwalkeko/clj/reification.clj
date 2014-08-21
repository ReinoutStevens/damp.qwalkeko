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
;;Note that this returns Java MetaVersions
;;You probably want to look at graph for other predicates
(defn all-versions []
  (let [models (history-project-models)]
    (mapcat versions models)))

(defn all-roots []
  (mapcat
    (fn [model]
      (with-metaproject model [metaproject]
        (roots metaproject)))
    (history-project-models)))

(defn eclipse-project [version]
  (.getEclipseProject version))

(defn successors [version]
  (seq (.getSuccessors version)))

(defn predecessors [version]
  (seq (.getPredecessors version)))

(defn date [version]
  (.getTime version))

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

(defn revision-number [version]
  (.getRevisionNumber version))

(defn commit-message [version]
  (.getCommitMessage version))
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
(defn changed-file-info [status filename]
  {:status status
   :file filename})

(defn convert-changed-file-info-status [status]
  (condp = status
    qwalkeko.ChangedFileInfo$Status/ADD :add
    qwalkeko.ChangedFileInfo$Status/EDIT :edit
    qwalkeko.ChangedFileInfo$Status/DELETE :delete))
    
    

(defn convert-changed-file-info [changedfileinfo]
  (let [status (convert-changed-file-info-status (.getStatus changedfileinfo))]
    {:status status
     :file (.getFileName changedfileinfo)}))



(defn file-infos [version]
  (seq (map convert-changed-file-info (.getChangedFileInfos version))))

(defn file-info-edited? [info]
  (= :edit (:status info)))

(defn file-info-added? [info]
  (= :add (:status info)))

(defn file-info-deleted? [info]
  (= :delete (:status info)))

(defn file-info-path [info]
  (:file info))

(defn file-changed? [path version]
  (let [infos (file-infos version)
        names (map :file infos)
        ;;path contains the project name as well, which we have to ignore
        cutted-path (.toString (.removeFirstSegments path 1))]
    (some #(= cutted-path %) names)))

