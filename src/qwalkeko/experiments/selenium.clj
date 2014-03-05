(ns qwalkeko.experiments.selenium
   (:require [clojure.java.jdbc :as sql])
   (:require [clojure.core.logic :as logic])
   (:require [qwalkeko.clj.logic :as l])
   (:require [qwalkeko.clj.reification :as r])
   (:require [damp.ekeko.jdt
              [ast :as jdt]])
   (:require [damp.qwal :as qwal]))



(def +db-path+  "/Users/resteven/Documents/PhD/mine.db")
(def +db-specs+ {:classname  "org.sqlite.JDBC",
                 :subprotocol   "sqlite",
                 :subname	    +db-path+})

(defn changed-files-query [project-name]
  ["select path from file where repo_key = ?", project-name])




(defn get-selenium-files [project]
 (map :path (sql/query +db-specs+
                       (changed-files-query project))))




;;
(defn is-selenium-file? [fileinfo]
  (let [path (str "/" (:file fileinfo))
        amount (count (sql/query +db-specs+
                                 ["select * from file where path = ? limit 1", path]))]
    (> amount 0)))

(defn fileinfo|selenium [fileinfo]
  (logic/project [fileinfo]
                 (logic/== true (is-selenium-file? fileinfo))))

(defn fileinfo|non-selenium [fileinfo]
  (logic/project [fileinfo]
                 (logic/== false (is-selenium-file? fileinfo))))


(defn process-file-infos [?infos ?selenium ?regular]
  (logic/fresh [?head ?tail ?newselenium ?newregular]
    (logic/conde [(logic/emptyo ?infos)
                  (logic/== ?selenium '())
                  (logic/== ?regular '())]
                 [(logic/conso ?head ?tail ?infos)
                  (logic/conda
                    [(fileinfo|selenium ?head)
                     (logic/conso ?head ?newselenium ?selenium)
                     (process-file-infos ?tail ?newselenium ?regular)]
                    [(logic/conso ?head ?newregular ?regular)
                     (process-file-infos ?tail ?selenium ?newregular)])])))
                  
(defn process-file-infos-clj [infos selenium regular]
  (if (empty? infos)
    [selenium regular]
    (if (is-selenium-file? (first infos))
      (recur (rest infos) (conj selenium (first infos)) regular)
      (recur (rest infos) selenium (conj regular (first infos))))))


(defn retrieve-file-infos [graph root]
  (l/qwalkeko* [?end ?selenium ?regular]
               (logic/fresh [?fileinfos]
                            (qwal/qwal graph root ?end []
                                       (qwal/q=>*)
                                       (l/in-current-meta [meta]
                                                          (l/fileinfos ?fileinfos meta)
                                                          (logic/project [?fileinfos]
                                                                         (logic/== [?selenium ?regular] (process-file-infos-clj ?fileinfos '() '()))))))))
                          

  
(defn java-file? [info]
  (.endsWith (:file info) ".java"))

(defn generate-file-ids [infos idx result]
  (if (empty? infos)
    result
    (let [info (first infos)]
      (if (or (contains? result (:file info))
              (not (java-file? info)))
        (recur (rest infos) idx result)
        (recur (rest infos)
               (inc idx)
               (conj result {(:file info) idx}))))))


(defn generate-version-ids [versions idx result]
  (if (empty? versions)
    result
    (recur (rest versions)
           (inc idx)
           (conj result {(first versions) idx}))))

(defn sort-and-filter-results [results]
  (let [filtered (filter 
                   (fn [[v s r]] 
                     (or (not (empty? (filter java-file? s)))
                         (not (empty? (filter java-file? r)))))
                   results)]
  (sort-by #(.getTime (first %)) filtered)))

(defn process-results [results]
  (defn process-file [file version type file-ids version-ids]
    (when (java-file? file)
      (println 
        (str
          "\""(get file-ids (:file file))"\"" " "
          "\""(get version-ids version)"\"" " "
          "\""(.getRevisionNumber version)"\"" " "
          "\""type"\"" " "
           "\""(.format (java.text.SimpleDateFormat. "yyyy/MM/dd HH:mm") (.getTime (.getTime version)))"\"" " "
           "\""(:file file)"\""))))       
  (defn process-version [[version selenium regular] file-ids version-ids]
    (let [edit-regular (filter #(r/file-info-edited? %) regular)
          delete-regular (filter #(r/file-info-deleted? %) regular)
          added-regular (filter #(r/file-info-added? %) regular)
          edit-selenium (filter #(r/file-info-edited? %) selenium)
          delete-selenium (filter #(r/file-info-deleted? %) selenium)
          added-selenium (filter #(r/file-info-added? %) selenium)]
      (do
        (doall (map #(process-file % version 'edit-regular file-ids version-ids) edit-regular))
        (doall (map #(process-file % version 'delete-regular file-ids version-ids) delete-regular))
        (doall (map #(process-file % version 'added-regular file-ids version-ids) added-regular))
        (doall (map #(process-file % version 'edit-selenium file-ids version-ids) edit-selenium))
        (doall (map #(process-file % version 'delete-selenium file-ids version-ids) delete-selenium))
        (doall (map #(process-file % version 'added-selenium file-ids version-ids) added-selenium))
        nil)))
  (let [sorted-results (sort-and-filter-results results)
        file-ids (generate-file-ids (concat (apply concat (map second sorted-results))
                                           (apply concat (map #(nth % 2) sorted-results))) 0 {})
        version-ids (generate-version-ids (map first sorted-results) 0 {})]
    (do
      (doall (map #(process-version % file-ids version-ids) sorted-results))
      nil)))


(defn output-results [results path]
  (do
    (with-open [wtr (clojure.java.io/writer (str path))]
      (binding [*out* wtr]
        (process-results results)))
    nil))


;;2nd try without core.logic

(defn changed-files-clojure [meta-project]
  (let [versions (.getVersions meta-project)]  
    (map (fn [v]
           (let [infos (r/file-infos v)
                 selenium (filter is-selenium-file? infos)
                 regular (filter #(not (is-selenium-file? %)) infos)]
             [v selenium regular]))
         versions)))

    


  (comment
    (def a-model (first (damp.ekeko.ekekomodel/all-project-models)))
    (def a-project (.getProject a-model))
    (def meta-project (first (.getMetaProjects (.getMetaProduct a-model))))
    (def a-graph (l/make-graph meta-project))
    (def a-root (first (.getRoots meta-project)))
    (def results (changed-files-clojure meta-project))
    (output-results results "/Users/resteven/Desktop/files.csv"))
    