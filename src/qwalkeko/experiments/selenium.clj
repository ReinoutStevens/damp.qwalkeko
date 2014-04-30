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
  ;;we compute infos outside of the logic query as nesting memberos results in a horrible performance
  (let [infos (seq 
                (filter #(.endsWith (r/file-info-path %) ".java")
                  (filter r/file-info-added? (r/file-infos (:jmetaversion version)))))
        results (set (l/qwalkeko* [?info ?cu]
                       (qwal/qwal graph version version
                         [?imp ?impname ?str ?package] ;;no actual imps are hurt during the query
                         (l/in-current [curr]
                           (logic/membero ?info infos)
                           (logic/onceo (l/fileinfo|compilationunit ?info ?cu curr))
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

;;assert statements
(defn methodinvocation|assert [?x]
  (logic/fresh [?strname]
    (jdt/ast :MethodInvocation ?x)
    (conv/methodinvocation|named ?x ?strname)
    (logic/project [?strname]
      (logic/== true (.startsWith ?strname "assert")))))

(defn change|affects-assert [change ?assert]
  (logic/all
    (change/change|affects-node change ?assert)
    (methodinvocation|assert ?assert)))



;;By.<something>(value)

(defn methodinvocation|by [?x]
  (logic/fresh [?name]
    (jdt/ast :MethodInvocation ?x)
    (jdt/child :expression ?x ?name)
    (jdt/name|simple-string ?name "By")))

;;@FindBy(something)
(defn annotation|findBy [?x]
  (logic/fresh [?name]
    (jdt/ast :NormalAnnotation ?x)
    (jdt/has :typeName ?x ?name)
    (jdt/name|simple-string ?name "FindBy")))


(defn change|affects-findBy [change ?find-by]
  (logic/all
    (change/change|affects-node change ?find-by)
    (logic/conde
      [(methodinvocation|by ?find-by)]
      [(annotation|findBy ?find-by)])))
  


;;changes to PageObject
(defn classinstancecreation|pageobject [?x]
  (logic/fresh [?t ?n ?str]
    (jdt/ast :ClassInstanceCreation ?x)
    (jdt/has :type ?x ?t)
    (jdt/has :name ?t ?n)
    (jdt/name|simple-string ?n ?str)
    (logic/project [?str]
      (logic/== true (.endsWith ?str "Page")))))


(defn change|affects-pageobject [?change ?pageobject]
  (logic/all
    (change/change|affects-node ?change ?pageobject)
    (classinstancecreation|pageobject ?pageobject)))



;;changes to Driver
(defn assignment|driver [?ass]
  (logic/fresh [?name] 
    (jdt/ast :Assignment ?ass)
    (jdt/has :leftHandSide ?ass ?name)
    (jdt/ast :SimpleName ?name)
    (jdt/name|simple-string ?name "driver"))) ;;should use binding information (that we dont have) but it looks like driver is a common name

(defn change|affects-driver [?change ?assignment]
  (logic/all
    (change/change|affects-node ?change ?assignment)
    (assignment|driver ?assignment)))


;;constant change
(defn ast|constant [ast]
  (logic/fresh [?type]
    (logic/membero ?type (seq '(:NumberLiteral :BooleanLiteral :CharacterLiteral :SimpleName :QualifiedName)))
    (jdt/ast ?type ast)))

(defn update|constant [change ?constant]
  (logic/fresh [?original ?to]
    (change/change|update change)
    (change/change|original change ?original)
    (change/update|newvalue change ?to)
    (ast|constant ?original)
    (ast|constant ?to)))


;;adding try block
(defn insert|catch [change ?catch]
  (logic/all
    (change/change|insert change)
    (change/insert|newnode change ?catch)
    (jdt/ast :CatchClause ?catch)))


;;adding @Ignore @Test
(defn insert|annotation [change ?annotation]
  (logic/all
    (change/change|insert change)
    (change/insert|newnode change ?annotation)
    (jdt/ast :MarkerAnnotation ?annotation)))

(defn insert|annotation|test [change ?annotation]
  (logic/fresh [?name]
    (insert|annotation change ?annotation)
    (jdt/has :typeName ?annotation ?name)
    (jdt/name|simple-string ?name "Test")))


(defn insert|annotation|ignore [change ?annotation]
  (logic/fresh [?name]
    (insert|annotation change ?annotation)
    (jdt/has :typeName ?annotation ?name)
    (jdt/name|simple-string ?name "Ignore")))

;;Arguments to Commands
(defn ast|command [?ast]
  (logic/fresh [?name]
    (jdt/ast :MethodInvocation ?ast)
    (jdt/has :name ?ast ?name)
    (logic/conde
      [(jdt/name|simple-string ?name "getAttribute")]
      [(jdt/name|simple-string ?name "sendKeys")])))

(defn change|affects-command [?change ?command]
  (logic/all
    (change/change|affects-node ?change ?command)
    (ast|command ?command)))

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
            (let [infos (seq 
                          (filter is-selenium-file?
                          (filter #(.endWith (r/file-info-path %) ".java")
                            (filter r/file-info-edited? (r/file-infos (:jmetaversion version))))))]
            (l/qwalkeko* [?left-cu ?right-cu ?info ?end]
              (qwal/qwal graph version ?end []
                (l/in-current-meta [curr]
                  (logic/membero ?info infos))
                (l/in-current [curr]
                  (logic/onceo (l/fileinfo|compilationunit ?info ?right-cu curr)))
                qwal/q<=
                (l/in-current [curr]
                  (logic/onceo (ast/ast-compilationunit|corresponding ?right-cu ?left-cu)))))))]
      (when-not (empty? preds)
        (doall (map graph/ensure-delete preds))
        (graph/ensure-delete version)
        (doall 
          (map (fn [[left-cu right-cu info end]]
                 (count-and-insert-changes
                   (graph/graph-project-name graph)
                   left-cu right-cu info version end))
               results)))
      nil))
  (doall
    (map classify-version (:versions graph)))
  nil)


;;classification of changes

;;classification
(defn classify-assert [?change ?type]
  (logic/fresh [?assert]
    (logic/== ?type "assert")
    (change|affects-assert ?change ?assert)))


(defn classify-findby [?change ?type]
  (logic/fresh [?findBy]
    (change|affects-findBy ?change ?findBy)
    (logic/== ?type "findby")))

(defn classify-pageobject [?change ?type]
  (logic/fresh [?pageobject]
    (change|affects-pageobject ?change ?pageobject)
    (logic/== ?type "pageobject")))

(defn classify-constantupdate [?change ?type]
  (logic/fresh [?constant]
    (update|constant ?change ?constant)
    (logic/== ?type "constant")))

(defn classify-driver [?change ?type]
  (logic/fresh [?driver]
    (change|affects-driver ?change ?driver)
    (logic/== ?type "driver")))


(defn classify-command [?change ?type]
  (logic/fresh [?command]
    (change|affects-command ?change ?command)
    (logic/== ?type "command")))

(defn classify-annotation-test [?change ?type]
  (logic/fresh [?annotation]
    (insert|annotation|test ?change ?annotation)
    (logic/== ?type "@test")))


(defn classify-annotation-ignore [?change ?type]
  (logic/fresh [?annotation]
    (insert|annotation|ignore ?change ?annotation)
    (logic/== ?type "@ignore")))

(defn classify-catch [?change ?type]
  (logic/fresh [?catch]
    (insert|catch ?change ?catch)
    (logic/== ?type "catch")))

(defn change-classifier [?change ?type]
  (logic/conde
    [(classify-assert ?change ?type)]
    [(classify-findby ?change ?type)]
    [(classify-pageobject ?change ?type)]
    [(classify-constantupdate ?change ?type)]
    [(classify-command ?change ?type)]
    [(classify-annotation-test ?change ?type)]
    [(classify-annotation-ignore ?change ?type)]
    [(classify-catch ?change ?type)]))

(defn classify-changes [graph change-goal]
  "change-goal is a logic goal that takes a change as input and should succeed if it is a wanted change.
   changetype is an identifier for the kind of change, used to write it to the DB"
  (defn write-out-changes [project-name version predecessor changes info changetype]
    (let [commitno (graph/revision-number version)
          predno (graph/revision-number predecessor)
          path (:file info)]
      (when-not (> (count (sql/query +db-specs+ 
                            ["select * from change_classification where commitno = ? and path = ? and predecessor = ? and changetype = ? LIMIT 1", 
                             commitno, path, predno, changetype])) 0)
        (sql/insert! +db-specs+ "change_classification"
          {:path (:file info) :repo_key project-name
           :commitno commitno :changes changes
           :changetype changetype :predecessor predno}))))
  
  (defn classify-version [version]
    (let [preds (graph/predecessors version)
          results
          (when-not (empty? preds)
            (let [infos (seq 
                          (filter is-selenium-file?
                          (filter #(.endWith (r/file-info-path %) ".java")
                            (filter r/file-info-edited? (r/file-infos (:jmetaversion version))))))]
              (l/qwalkeko* [?change ?info ?end ?type]
                (qwal/qwal graph version ?end [?left-cu ?right-cu]
                  (l/in-current-meta [curr]
                   (logic/membero ?info infos))
                  (l/in-current [curr]
                    (logic/onceo (l/fileinfo|compilationunit ?info ?right-cu curr)))
                  qwal/q<=
                  (l/in-current [curr]
                    (logic/onceo (ast/ast-compilationunit|corresponding ?right-cu ?left-cu))
                    (change/change ?change ?left-cu ?right-cu)
                    (change-goal ?change ?type))))))]
      (when-not (empty? preds)
        (let [processed  (reduce (fn [m [change info end type]] ;;gives a {:predA {:fileA {:type no-changes} :fileB {:type no-changes}} :predB ...}
                                   (update-in m [end info type] (fnil inc 0))) {} results)]
          (doall (map graph/ensure-delete preds))
          (graph/ensure-delete version)
          (doall
            (map 
              (fn [pred]
                (doall
                  (map (fn [file]
                         (doall
                           (map (fn [changetype]
                                  (write-out-changes 
                                    (graph/graph-project-name graph) version pred (get-in processed [pred file changetype]) file changetype))
                             (keys (get-in processed [pred file])))))
                    (keys (get processed pred)))))
              (keys processed)))))))
  (doall
    (map classify-version (:versions graph))))

;;
(defn history-model-to-graph [history-model]
  (let [meta-project (first (.getMetaProjects (.getMetaProduct history-model)))
        a-graph (graph/convert-project-to-graph meta-project)]
    a-graph))


;;do le magic
(defn do-selenium-experiment-on-graphs [graphs]
  (doall
    (map populate-graph graphs)) ;;this 1 first so if something goes wrong in the next steps we at least have this
  (println "counting")
  (doall
    (map count-changes graphs))
  (println "classifying")
  (doall
    (map classify-changes graphs)))
  
  