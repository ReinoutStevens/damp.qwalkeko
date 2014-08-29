(ns qwalkeko.experiments.selenium
   (:require [clojure.java.jdbc :as sql])
   (:require [clojure.core.logic :as logic])
   (:require [qwalkeko.clj.logic :as l])
   (:require [qwalkeko.clj.reification :as r])
   (:require [qwalkeko.clj.graph :as graph])
   (:require [qwalkeko.clj.ast :as ast])
   (:require [qwalkeko.clj.changes :as change])
   (:require [damp.ekeko.jdt
              [ast :as jdt]
              [convenience :as conv]])
   (:require [damp.qwal :as qwal]))



(def +db-path+  "/Users/resteven/Documents/PhD/papers/2014-icpc-seleniumusage/mine.db")
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


(defn compilationunit|selenium [?cu]
  (logic/fresh [?imp ?impname ?str]
    (jdt/ast :CompilationUnit ?cu)
    (jdt/child :imports ?cu ?imp)
    (jdt/has :name ?imp ?impname)
    (jdt/name|qualified-string ?impname ?str)
    (logic/project [?str]
      (logic/== true (> (.indexOf ?str ".selenium") 0)))))
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
(defn find-commit [graph rev-no]
  (first (filter #(= (graph/revision-number %) rev-no) (:versions graph))))

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
    (logic/project [ast]
      (logic/conda
        [(jdt/ast :NumberLiteral ast)]
        [(jdt/ast :BooleanLiteral ast)]
        [(jdt/ast :CharacterLiteral ast)]
        [(jdt/ast :SimpleName ast)]
        [(jdt/ast :QualifiedName ast)]
        [(jdt/ast :StringLiteral ast)]
        [(logic/== false (instance? org.eclipse.jdt.core.dom.ASTNode ast))]))))

(defn update|constant [change ?constant]
  (logic/fresh [?original ?to]
    (change/change|update change)
    (change/change|original change ?original)
    (change/update|newvalue change ?to)
    (ast|constant ?original)
    (ast|constant ?to)))


;;adding try block
;;we explicetly use an insert here of a try block as we try to capture
;;exceptions that are added after people notice timeout exceptions
;;we dont want to look for changes to nodes that affect a try block, as this will also
;;capture changes made to the body of a try block, which is unrelated to adding the try
;;we undershoot here
(defn insert|try [change ?try]
  (logic/all
    (change/change|insert change)
    (change/insert|newnode change ?try)
    (jdt/ast :TryStatement ?try)))

;;a) waarom restricteren tot inserts? 
;;algemener zou zijn je change/change|affects-node te gebruiken

;;b) optioneel: 
;;kunnen we hier nog checken of de naam van gevangen exception
;;wel degelijk TimeOutException of StaleElementReferenceException is?
;;(zonder bindings)
;;Deze exceptions zijn namelijk te wijten aan het feit dat je met een
;;verschillende proces aan het communiceren bent (de browser).
;;Andere exceptions zoals ElementNotFoundException 
;;geven aan dat de test iets anders verwacht dan het systeem zelf. 
;;(deze worden normaal niet opgevangen zodat de test faalt).


(defn trystatement|timeoutrelated [?try]
  (logic/fresh [?catch ?exception ?type ?name]
    (jdt/child :catchClauses ?try ?catch)
    (jdt/has :exception ?catch ?exception)
    (jdt/has :type ?exception ?type)
    (jdt/has :name ?type ?name)
    (logic/conda
      [(jdt/name|simple-string ?name "TimeOutException")]
      [(jdt/name|simple-string ?name "StaleElementReferenceException")]))) ;;perhaps more exceptions should be added here

  

;; @Ignore @Test @BeforeClass @AfterClass
(defn change|annotation [change ?annotation]
  (logic/all
    (change/change|affects-node change ?annotation)
    (jdt/ast :MarkerAnnotation ?annotation)))

(defn change|annotation|test [change ?annotation]
  (logic/fresh [?name]
    (change|annotation change ?annotation)
    (jdt/has :typeName ?annotation ?name)
    (jdt/name|simple-string ?name "Test")))


(defn change|annotation|ignore [change ?annotation]
  (logic/fresh [?name]
    (change|annotation change ?annotation)
    (jdt/has :typeName ?annotation ?name)
    (jdt/name|simple-string ?name "Ignore")))

(defn change|annotation|beforeclass [change ?annotation]
  (logic/fresh [?name]
    (change|annotation change ?annotation)
    (jdt/has :typeName ?annotation ?name)
    (jdt/name|simple-string ?name "BeforeClass")))

(defn change|annotation|afterclass [change ?annotation]
  (logic/fresh [?name]
    (change|annotation change ?annotation)
    (jdt/has :typeName ?annotation ?name)
    (jdt/name|simple-string ?name "AfterClass")))


;;Inspectors and Commands
(defn ast|inspector [?ast]
  (logic/fresh [?name]
    (jdt/ast :MethodInvocation ?ast)
    (jdt/has :name ?ast ?name)
    (logic/conda
      [(jdt/name|simple-string ?name "getAttribute")]
      [(jdt/name|simple-string ?name "getCssValue")]
      [(jdt/name|simple-string ?name "getSize")]
      [(jdt/name|simple-string ?name "getTagName")]
      [(jdt/name|simple-string ?name "getText")]
      [(jdt/name|simple-string ?name "isDisplayed")]
      [(jdt/name|simple-string ?name "isEnabled")]
      [(jdt/name|simple-string ?name "isSelected")])))

(defn change|affects-inspector [?change ?inspector]
  (logic/all
    (change/change|affects-node ?change ?inspector)
    (ast|inspector ?inspector)))


(defn ast|command [?ast]
  (logic/fresh [?name]
    (jdt/ast :MethodInvocation ?ast)
    (jdt/has :name ?ast ?name)
    (logic/conda
      [(jdt/name|simple-string ?name "clear")]
      [(jdt/name|simple-string ?name "click")]
      [(jdt/name|simple-string ?name "sendKeys")]
      [(jdt/name|simple-string ?name "submit")])))

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
                          (filter #(.endsWith (r/file-info-path %) ".java")
                            (filter r/file-info-edited? (r/file-infos (:jmetaversion version))))))]
            (l/qwalkeko* [?left-cu ?right-cu ?info ?end]
              (qwal/qwal graph version ?end []
                (l/in-current-meta [curr]
                  (logic/membero ?info infos))
                (l/in-current [curr]
                  (logic/onceo (l/fileinfo|compilationunit ?info ?right-cu curr)))
                qwal/q<=
                (l/in-current [curr]
                  (logic/onceo (ast/compilationunit|corresponding ?right-cu ?left-cu)))))))]
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
    (change|affects-assert ?change ?assert)
    (logic/== ?type :assertion)))

(defn classify-findby [?change ?type]
  (logic/fresh [?findBy]
    (change|affects-findBy ?change ?findBy)
    (logic/== ?type :location)))

(defn classify-pageobject [?change ?type]
  (logic/fresh [?pageobject]
    (change|affects-pageobject ?change ?pageobject)
    (logic/== ?type :pageobject)))

(defn classify-constantupdate [?change ?type]
  (logic/fresh [?constant]
    (update|constant ?change ?constant)
    (logic/== ?type :constant)))

(defn classify-driver [?change ?type]
  (logic/fresh [?driver]
    (change|affects-driver ?change ?driver)
    (logic/== ?type :driver)))

(defn classify-command [?change ?type]
  (logic/fresh [?command]
    (change|affects-command ?change ?command)
    (logic/== ?type :command)))

(defn classify-demarcator [?change ?type]
  (logic/fresh [?annotation]
    (logic/conda
      [(change|annotation|afterclass ?change ?annotation)]
      [(change|annotation|beforeclass ?change ?annotation)]
      [(change|annotation|ignore ?change ?annotation)]
      [(change|annotation|test ?change ?annotation)])
    (logic/== ?type :demarcator)))


(defn classify-exception [?change ?type]
  (logic/fresh [?try]
    (insert|try ?change ?try)
    (logic/== ?type :exception)))

(defn change-classifier [?change ?type]
  ;;onceo is added here as 'change|affects-node' looks in both the original tree and the new tree
  ;;as a result it could affect an assert statement multiple times
  ;;(we could also just filter out duplicate results later)
  (logic/all
    (logic/conde
      [(logic/onceo (classify-assert ?change ?type))]
      [(logic/onceo (classify-findby ?change ?type))]
      [(logic/onceo (classify-constantupdate ?change ?type))]
      [(logic/onceo (classify-command ?change ?type))]
      [(logic/onceo (classify-demarcator ?change ?type))]
      [(logic/onceo (classify-exception ?change ?type))])))


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
                          (filter #(.endsWith (r/file-info-path %) ".java")
                            (filter r/file-info-edited? (r/file-infos (:jmetaversion version))))))]
              (l/qwalkeko* [?change ?info ?end ?type]
                (qwal/qwal graph version ?end [?left-cu ?right-cu]
                  (l/in-current-meta [curr]
                   (logic/membero ?info infos))
                  (l/in-current [curr]
                    (logic/onceo (l/fileinfo|compilationunit ?info ?right-cu curr)))
                  qwal/q<=
                  (l/in-current [curr]
                    (logic/onceo (ast/compilationunit|corresponding ?right-cu ?left-cu))
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


  
;;counting changes of selenium files
(defn count-and-insert-changes [project-name left-ast right-ast info version predecessor changes]
  (let [commitno (graph/revision-number version)
        predno (graph/revision-number predecessor)
        path (:file info)]
    (when-not (> (count (sql/query +db-specs+ 
                          ["select * from no_changes where commitno = ? and path = ? and predecessor = ? LIMIT 1", 
                           commitno, path, predno])) 0)
        (sql/insert! +db-specs+ "no_changes"
          {:path (:file info) :repo_key project-name
           :commitno commitno :changes (count changes)
           :predecessor predno})
        changes)))

(defn classify-change [change]
  (logic/run* [?change ?type]
    (logic/== change ?change)
    (change-classifier ?change ?type)))


(defn version-selenium-is-computed? [graph version]
  (> (count (sql/query +db-specs+
                                ["select * from selenium_computed where repo_key = ? and commitno = ? limit 1", 
                                 (graph/graph-project-name graph),
                                 (graph/revision-number version)])) 0))

(defn selenium-experiment-on-graph [graph compute-selenium compute-no-changes] ;;longest method in the history of methods
  (defn ensure-selenium-files-computed [version]
    (let [infos (seq 
                  (filter #(.endsWith (r/file-info-path %) ".java")
                    (filter r/file-info-edited? (r/file-infos (:jmetaversion version)))))
          rev-no (graph/revision-number version)]
      (if (and compute-selenium (not (version-selenium-is-computed? graph version)))
        (let [results
              (l/qwalkeko* [?info ?cu]
                (qwal/qwal graph version version
                  [?imp ?impname ?str ?package] ;;no actual imps are hurt during the query
                  (l/in-current [curr]
                    (logic/membero ?info infos)
                    (logic/onceo (l/fileinfo|compilationunit ?info ?cu curr))
                    (compilationunit|selenium ?cu))))]
          (doall
            (map 
              (fn [[info cu]]
                (add-changed-file
                  (graph/graph-project-name graph)
                  info
                  (graph/revision-number version)
                  (count (filter #(= \newline %) (.toString cu)))))
              results))
          (sql/insert! +db-specs+ "selenium_computed" {:repo_key (graph/graph-project-name graph) :commitno (graph/revision-number version)})))))
          
  (defn write-out-changes [project-name version predecessor changes info changetype operation]
    (let [commitno (graph/revision-number version)
          predno (graph/revision-number predecessor)
          path (:file info)]
      (when-not (> (count (sql/query +db-specs+ 
                            ["select * from change_classification where commitno = ? and path = ? and predecessor = ? and changetype = ? and operation = ? LIMIT 1", 
                             commitno, path, predno, changetype, operation])) 0)
        (sql/insert! +db-specs+ "change_classification"
          {:path (:file info) :repo_key project-name
           :commitno commitno :changes changes
           :changetype changetype :predecessor predno
           :operation operation}))))
  
  
  (defn classify-version [version]
    (let [preds (graph/predecessors version)
          results
          (when-not (empty? preds)
            (doall
              (map ensure-selenium-files-computed (cons version preds)))
            (let [infos (seq 
                          (filter is-selenium-file?
                            (filter #(.endsWith (r/file-info-path %) ".java")
                              (filter r/file-info-edited? (r/file-infos (:jmetaversion version))))))]
              (l/qwalkeko* [?left-cu ?right-cu ?info ?end]
                (qwal/qwal graph version ?end []
                  (l/in-current-meta [curr]
                    (logic/membero ?info infos))
                  (l/in-current [curr]
                    (logic/onceo (l/fileinfo|compilationunit ?info ?right-cu curr)))
                  qwal/q<=
                  (l/in-current [curr]
                    (logic/onceo (ast/compilationunit|corresponding ?right-cu ?left-cu)))))))]
      (when-not (empty? preds)
        (doall (map graph/ensure-delete preds))
        (graph/ensure-delete version)
        (doall 
          (map (fn [[left-cu right-cu info end]]
                 (let [changes
                       (change/get-ast-changes left-cu right-cu)
                       classified
                       (mapcat classify-change changes)
                       grouped-together
                       (reduce (fn [m [change type]]
	                                (update-in m [type (:operation change)] (fnil inc 0)))
	                        {} classified)]
                   (when compute-no-changes
                     (count-and-insert-changes
                           (graph/graph-project-name graph)
                           left-cu right-cu info version end changes))
                   (doall
                     (map
                       (fn [type]
                         (doall
                           (map
                             (fn [operation]
                               (let [v (get-in grouped-together [type operation])]
                                 (write-out-changes (graph/graph-project-name graph) version end v info type operation)))
                             (keys (get grouped-together type)))))
                       (keys grouped-together)))))
            results))
        nil)))
  (doall
    (map classify-version (:versions graph))))

;;write results to .csv


(defn write-out-number-of-changes [path]
  (defn total-changes [repo-key]
    (let [changes (apply + (map :changes (sql/query +db-specs+ ["select changes from no_changes where repo_key = ?", repo-key])))] ;;could also do select sum(changes)
      changes))
  (defn write-out-change-classification [repo-key]
        (let [classification  (sql/query +db-specs+ ["select changetype, changes from change_classification where repo_key = ?", repo-key])
              result (reduce (fn [m e] ;;returns map with key change classification and value the amount
                               (update-in m [(:changetype e)]
                                 (fnil (fn [x] (+ (:changes e) x)) 0)))
                       {} classification)
              total (total-changes repo-key)]
          (doall 
            (for [[k v] result] (println (str "\"" repo-key  "\"" " " "\"" k "\"" " " "\"" (double (/ v total)) "\""))))))
  (let [repos (map :repo_key (sql/query +db-specs+ "select distinct repo_key from no_changes"))]
     (with-open [wtr (clojure.java.io/writer (str path))]
       (binding [*out* wtr]
         (doall (map write-out-change-classification repos))))
     
    nil))
    
        
(comment
  (write-out-number-of-changes "/Users/resteven/Documents/PhD/papers/2014-icpc-seleniumusage/data/changes.csv")

(map
  (fn [version]
    (let [results (find-selenium-files version)]
      (write-results-to-db results)
      (ensure-delete version)
      (ensure-delete-predecessors version)))
  (:versions graph)))
