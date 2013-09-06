(ns qwalkeko.clj.changedistiller
  (:import (ch.uzh.ifi.seal.changedistiller ChangeDistiller))
  (:require [clojure.core.logic :as logic])
  (:require [qwalkeko.clj.reification :as r]))




(def JAVA ch.uzh.ifi.seal.changedistiller.ChangeDistiller$Language/JAVA)

;;ChangeDistiller
(defn create-file-distiller []
  (ChangeDistiller/createFileDistiller JAVA))


(defn get-source-code-changes [distiller]
  (.getSourceCodeChanges distiller))

(defn distil-changes-of-files [left-file right-file]
  (let [distiller (create-file-distiller)]
    (.extractClassifiedSourceCodeChanges distiller left-file right-file)
    distiller))

(defn distil-changes-of-ifiles [left-ifile right-ifile]
  (distil-changes-of-files 
    (.toFile (.getLocation left-ifile))
    (.toFile (.getLocation right-ifile))))



(defn distil-changes-of-eclipse-files [left-project right-project location]
  (let [left-ifile (.getFile left-project location)
        right-ifile (.getFile right-project location)]
    (distil-changes-of-ifiles left-ifile right-ifile)))



(defn distil-changes-between-files-in-versions [left-version right-version location]
  (distil-changes-of-eclipse-files 
    (.getEclipseProject left-version)
    (.getEclipseProject right-version)
    location))



;;Integration with Qwal
(defn distill-changes [current compare-with path]
  (r/ensure-checkout compare-with)
  (r/ensure-checkout current)
  (distil-changes-between-files-in-versions current compare-with path))


(defn changeso [current compare-with path changes]
  "non relational"
  (logic/all
    (logic/== changes (distill-changes current compare-with path))))



;;SourceChange
(defn is-update? [a-change]
  (instance? ch.uzh.ifi.seal.changedistiller.model.entities.Update a-change))

(defn is-move? [a-change]
 (instance? ch.uzh.ifi.seal.changedistiller.model.entities.Move a-change))

(defn is-delete? [a-change]
  (instance? ch.uzh.ifi.seal.changedistiller.model.entities.Delete a-change))

(defn is-insert? [a-change]
  (instance? ch.uzh.ifi.seal.changedistiller.model.entities.Insert a-change))



;;SourceCodeEntity
(defn get-start [an-entity]
  (.getStartPostion an-entity))

(defn get-end [an-entity]
  (.getEndPosition an-entity))




(defn get-root-entity [a-change]
  "taken from right version"
  (.getRootEntity a-change))

(defn get-changed-entity [a-change]
  "taken from left version (with some exceptions)"
  (.getChangedEntity a-change))

(defn get-change-type [a-change]
  (.getChangeType a-change))

(defn get-parent-entity [a-change]
  (.getParentEntity a-change))


;;Update/Move
(defn get-new-entity [an-update]
  "the entity in the right/new ast"
  (.getNewEntity an-update))

;;Insert/Delete do not have class specific methods we care about



