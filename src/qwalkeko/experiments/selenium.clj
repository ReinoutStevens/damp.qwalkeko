(ns qwalkeko.experiments.selenium
   (:require [clojure.java.jdbc :as sql])
   (:require [clojure.core.logic :as logic])
   (:require [qwalkeko.clj.logic :as l])
   (:require [qwalkeko.clj.reification :as r])
   (:require [qwalkeko.clj.graph :as graph])
   (:require [qwalkeko.clj.ast :as ast])
   (:require [qwalkeko.clj.changenodes :as change])
   (:require [damp.ekeko.jdt
              [ast :as jdt]
              [convenience :as conv]])
   (:require [damp.qwal :as qwal]))



(def +db-path+  "/home/resteven/selenium/db/mine.db")
(def +db-specs+ {:classname  "org.sqlite.JDBC",
                 :subprotocol   "sqlite",
                 :subname	    +db-path+})

(defn changed-files-query [project-name]
  ["select path from selenium where repo_key = ?", project-name])


(defn get-selenium-files [project]
 (map :path (sql/query +db-specs+
                       (changed-files-query project))))


(defn is-selenium-file? [fileinfo]
  (let [path (:file fileinfo)
        amount (count (sql/query +db-specs+
                        ["select * from selenium where path = ? limit 1", path]))]
    (> amount 0)))

;;Populating database
(defn add-changed-file [project-name info commit loc]
  (when-not (is-selenium-file? info)
    (sql/insert! +db-specs+ "selenium"
      {:path (:file info) :repo_key project-name
       :commitno commit :loc loc})))
;;commit
;;projectname
;;numbers of line
;;


(defn populate-version [graph version]
  (let [results (set (l/qwalkeko* [?info ?cu]
                       (qwal/qwal graph version version
                         [?imp ?impname ?str ?package] ;;no actual imps are hurt during the query
                         (l/in-current [curr]
                           (logic/conde
                             [(l/fileinfo|add ?info curr)]
                             [(l/fileinfo|edit ?info curr)])
                           (l/fileinfo|compilationunit ?info ?cu curr)
                           (jdt/child :imports ?cu ?imp)
                           (jdt/has :name ?imp ?impname)
                           (jdt/name|qualified-string ?impname ?str)
                           (logic/project [?str]
                             (logic/== true (> (.indexOf ?str ".selenium") 0)))))))]
    (doall
      (map 
        (fn [[info cu]]
          (add-changed-file
            (graph/graph-project-name graph)
            info
            (graph/revision-number version)
            (count (filter #(= \newline %) (.toString cu)))))
        results))))

(defn populate-graph [graph]
  (doall
    (map 
      (fn [version]
        (do
          (populate-version graph version)
          (graph/ensure-delete version)))
      (:versions graph))))
    
    
;;

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
  (sort-by #(graph/date (first %)) filtered)))

(defn process-results [results]
  (defn process-file [file version type file-ids version-ids]
    (when (java-file? file)
      (println 
        (str
          "\""(get file-ids (:file file))"\"" " "
          "\""(get version-ids version)"\"" " "
          "\""(graph/revision-number version)"\"" " "
          "\""type"\"" " "
           "\""(.format (java.text.SimpleDateFormat. "yyyy/MM/dd HH:mm") (.getTime (graph/date version)))"\"" " "
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

(defn changed-files-clojure [a-graph]
  (let [versions (:versions a-graph)]
    (map (fn [v]
           (let [infos (graph/file-infos v)
                 selenium (filter is-selenium-file? infos)
                 regular (filter #(not (is-selenium-file? %)) infos)]
             [v selenium regular]))
         versions)))

    
(comment
  (def a-model (first (damp.ekeko.ekekomodel/all-project-models)))
  (def a-project (.getProject a-model))
  (def meta-project (first (.getMetaProjects (.getMetaProduct a-model))))
  (def a-graph (graph/convert-project-to-graph meta-project))
  (def a-root (first (:roots a-graph)))
  (def results (changed-files-clojure a-graph))
  (output-results results "/Users/resteven/Desktop/files.csv"))



;;change patterns

(defn version|modified-selenium [?version]
  (logic/fresh [?info]
               (l/fileinfo ?info ?version)
               (logic/onceo
                 (fileinfo|selenium ?info))))


;;even a filtered graph is way too slow for some projects...
(defn graph-to-selenium-graph [graph]
  (graph/filter-graph graph
    (fn [v] (let [infos (graph/file-infos v)]
              (some is-selenium-file? infos)))))


(defn change-pattern-query [x graph root file]
 (l/qwalkeko x [?info ?end]
     (qwal/qwal graph root ?end [?info ?changedinfo]
                (qwal/q=>*)
                (l/in-current-meta [curr]
                                   (l/fileinfo|file ?info file curr)
                                   (l/fileinfo|add ?info curr)
                                   (fileinfo|selenium ?info))
                (qwal/q=>+)
                (l/in-current-meta [curr]
                                   (l/fileinfo|file ?changedinfo file curr)
                                   (l/fileinfo|edit ?changedinfo curr)))))



(defn find-commit [graph rev-no]
  (first (filter #(= (graph/revision-number %) rev-no) (:versions graph))))

(comment
  (def filtered-graph (graph-to-selenium-graph a-graph))
  (def le-end (find-commit filtered-graph "238d082c6f36b42991c9fcdfe0790fcf15f7e440"))
  (def le-start (first (graph/predecessors le-end)))
  (def results 
   (first (l/qwalkeko 1 [?left ?right]
            (qwal/qwal filtered-graph le-start le-end [?linfo ?rinfo ?name]
              (l/in-current [curr]
                (l/fileinfo ?linfo curr)
                (fileinfo|selenium ?linfo)
                (l/fileinfo|maintypename ?linfo ?name curr)
                (l/fileinfo|compilationunit ?linfo ?left curr))
              qwal/q=>
              (l/in-current [curr]
                (l/fileinfo|edit ?rinfo curr)
                (l/fileinfo|maintypename ?rinfo ?name curr)
                (l/fileinfo|compilationunit ?rinfo ?right curr))))))
  (def left-ast (first results))
  (def right-ast (second results))
  (def changes  (qwalkeko.clj.changenodes/get-ast-changes left-ast right-ast)))
  


;;patterns
;;constant change
(defn ast|constant [ast]
  (logic/fresh [?type]
    (logic/membero ?type (seq '(:NumberLiteral :BooleanLiteral :CharacterLiteral :SimpleName :QualifiedName)))
    (jdt/ast ?type ast)))

(defn update|constant [change]
  (logic/fresh [?original ?to]
    (change/change|update change)
    (change/change|original change ?original)
    (change/update|newvalue change ?to)
    (ast|constant ?original)
    (ast|constant ?to)))
    

    
(comment
  (def filtered-graph (graph-to-selenium-graph a-graph))
  (def le-end (find-commit filtered-graph "8b4c3f9a75667a509c4c704a90de1e1dc3ec6f8f"))
  (def le-start (first (graph/predecessors le-end)))
  (def results 
   (first 
     (l/qwalkeko 1 [?left ?right]
                 (qwal/qwal filtered-graph le-end le-start [?ltype ?rinfo ?name ?lname]
                   (l/in-current [curr]
                     (l/fileinfo|edit ?rinfo curr)
                     (l/fileinfo|maintypename ?rinfo ?name curr)
                     ;(logic/== ?name "BaselineGeneBioEntityPageExistingGeneIT")
                     (logic/== ?name "HeatmapTablePage")
                     (l/fileinfo|compilationunit ?rinfo ?right curr))
                   qwal/q<=
                   (l/in-current [curr]
                     (jdt/ast :CompilationUnit ?left)
                     (ast/compilationunit-typedeclaration|main ?left ?ltype)
                     (jdt/has :name ?ltype ?lname)
                     (jdt/name-string|qualified ?lname ?name))))))
  (def left-ast (first results))
  (def right-ast (second results))
  (def changes  (change/get-ast-changes left-ast right-ast)))


(defn methodinvocation|assert [?x]
  (logic/all
    (jdt/ast :MethodInvocation ?x)
    (logic/conde
      [(conv/methodinvocation|named ?x "assertThat")]
      [(conv/methodinvocation|named ?x "assert")])))


(defn change|affects-assert [change ?assert]
  (logic/all
    (change/change|affects-node change ?assert)
    (methodinvocation|assert ?assert)))


;;counting changes of selenium files



(defn count-and-insert-changes [project-name left-ast right-ast info version predecessor]
  (let [commitno (graph/revision-number version)
        predno (graph/revision-number predecessor)
        path (:file info)]
    (when-not (> (count (sql/query +db-specs+ 
                          ["select * from no_changes where commitno = ? and path = ? and predecessor = ? LIMIT 1", 
                           commitno, path, predno])) 0)
      (let [changes (change/get-ast-changes left-ast right-ast)]
        (sql/insert! +db-specs+ "no_changes"
          {:path (:file info) :repo_key project-name
           :commitno commitno :changes (count changes)
           :predecessor predno})))))


(defn count-changes [graph]
  (defn classify-version [version]
    (let [preds (graph/predecessors version)
          results
          (when-not (empty? preds)
            (l/qwalkeko* [?left-cu ?right-cu ?info ?end]
              (qwal/qwal graph version ?end []
                (l/in-current-meta [curr]
                  (l/fileinfo|edit ?info curr)
                  (fileinfo|selenium ?info))
                (l/in-current [curr]
                  (l/fileinfo|compilationunit ?info ?right-cu curr))
                qwal/q<=
                (l/in-current [curr]
                  (ast/ast-compilationunit|corresponding ?right-cu ?left-cu)))))]
      (when-not (empty? preds)
        (do
          (doall (map graph/ensure-delete preds))
          (graph/ensure-delete version)
          (doall 
            (map (fn [[left-cu right-cu info end]]
                   (count-and-insert-changes
                     (graph/graph-project-name graph)
                     left-cu right-cu info version end))
                 results))))
      nil))
  (doall
    (map classify-version (:versions graph)))
  nil)



(defn classify-changes [graph]
  (defn write-out-changes [project-name version predecessor changes info changetype]
    (let [commitno (graph/revision-number version)
          predno (graph/revision-number predecessor)
          path (:file info)]
      (when-not (> (count (sql/query +db-specs+ 
                            ["select * from change_classification where commitno = ? and path = ? and predecessor = ? and changetype = ? LIMIT 1", 
                             commitno, path, predno, changetype])) 0)
        (sql/insert! +db-specs+ "change_classification"
          {:path (:file info) :repo_key project-name
           :commitno commitno :changes (count changes)
           :changetype changetype :predecessor predno}))))
 
  (defn classify-version [version]
    (let [preds (graph/predecessors version)
          results
          (when-not (empty? preds)
            (l/qwalkeko* [?assert-change ?info ?end]
              (qwal/qwal graph version ?end [?left-cu ?right-cu ?assert]
                (l/in-current-meta [curr]
                  (l/fileinfo|edit ?info curr)
                  (fileinfo|selenium ?info))
                (l/in-current [curr]
                  (l/fileinfo|compilationunit ?info ?right-cu curr))
                qwal/q<=
                (l/in-current [curr]
                  (ast/ast-compilationunit|corresponding ?right-cu ?left-cu)
                  (change/change ?assert-change ?left-cu ?right-cu)
                  (change|affects-assert ?assert-change ?assert)))))]
      (when-not (empty? preds)
        (do
          (doall (map graph/ensure-delete preds))
          (graph/ensure-delete version)
          (doall ;;isn't she loooovely (probably should look into changing this, if processing the results takes more LOC than the actual query)
            (map (fn [version-changes]
                   (map (fn [info-changes] ;;list of assertchanges to 1 specific file info
                          (let [changes (map first info-changes)
                                info (nth (first info-changes) 1)
                                pred (nth (first info-changes) 2)]
                            (write-out-changes (graph/graph-project-name graph) version pred info "assert"))) 
                          version-changes)
                   (vals (group-by (fn [[change info end]] info) version-changes))) ;;and now by file
              (vals (group-by (fn [[change info end]] end) results)))))))) ;;lets group them together by predecessor
  (doall
    (map classify-version (:versions graph))))
    