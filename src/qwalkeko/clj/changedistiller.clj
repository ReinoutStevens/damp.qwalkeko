(ns qwalkeko.clj.changedistiller
  (:import (ch.uzh.ifi.seal.changedistiller ChangeDistiller)))



(def JAVA (first (seq (ChangeDistiller/getProvidedLanguages)))) ;;there should be a cleaner way to access Java Enum

(defn distil-changes-of-files [left-file right-file]
  (let [distiller (ChangeDistiller/createFileDistiller JAVA)]
    (.extractClassifiedSourceCodeChanges distiller left-file right-file)))

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