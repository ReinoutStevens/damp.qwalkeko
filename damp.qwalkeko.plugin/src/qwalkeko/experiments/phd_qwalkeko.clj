(ns qwalkeko.experiments.phd-qwalkeko
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

(def a-model (first (damp.ekeko.ekekomodel/all-project-models)))
(def a-graph (graph/convert-model-to-graph a-model))
(def a-root (first (:roots a-graph)))

(def +db-path+  "/Users/resteven/Documents/PhD/papers/2014-icpc-seleniumusage/mine.db")
(def +db-specs+ {:classname  "org.sqlite.JDBC",
                 :subprotocol   "sqlite",
                 :subname	    +db-path+})

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

(defn string-contains [?str part]
  (logic/all
    (logic/project [?str]
      (logic/== true (> (.indexOf ?str part) 0)))))

(defn compilationunit|selenium [?cu]
  (logic/fresh [?imp ?impname ?str]
    (jdt/ast :CompilationUnit ?cu)
    (jdt/child :imports ?cu ?imp)
    (jdt/has :name ?imp ?impname)
    (jdt/name|qualified-string ?impname ?str)
    (string-contains ?str ".selenium")))

(defn count-lines [ast]
  (count (clojure.string/split-lines (.toString ast))))

(defn identify-selenium-files []
  (let [results
        (l/qwalkeko* [?info ?cu ?end]
          (qwal/qwal a-graph version ?end []
            (qwal/q=>*)
            (l/in-git-info [curr]
              (l/fileinfo|add ?info curr))
            (l/in-source-code [curr]
              (l/fileinfo|compilationunit ?info ?cu curr)
              (compilationunit|selenium ?cu))))]
    (doall (map graph/ensure-delete (:versions a-graph)))
    (doall (map (fn [[info cu end]]
                  (add-changed-file (graph/graph-project-name a-graph) info end (count-lines cu)))
                  results))))

(defn fileinfo|selenium [fileinfo]
  (logic/project [fileinfo]
    (logic/== true (is-selenium-file? fileinfo))))


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
    (jdt/name|simple-string ?name "driver"))) 

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
        [(jdt/ast :StringLiteral ast)]))))

(defn update|constant [change ?constant]
  (logic/fresh [?original ?to]
    (change/change|update change)
    (change/change|original change ?original)
    (change/update|newvalue change ?to)
    (ast|constant ?original)
    (ast|constant ?to)))


;; @Ignore @Test @BeforeClass @AfterClass
(defn change|annotation [change ?annotation]
  (logic/all
    (change/change|affects-node change ?annotation)
    (jdt/ast :MarkerAnnotation ?annotation)))

(defn annotation-name|equals [?annotation ?val]
  (logic/fresh [?name]
    (change|annotation change ?annotation)
    (jdt/has :typeName ?annotation ?name)
    (jdt/name|simple-string ?name ?val)))

(defn change|annotation|test [change ?annotation]
  (logic/all
    (change|annotation change ?annotation)
    (annotation-name|equals ?annotation "Test")))


(defn change|annotation|ignore [change ?annotation]
  (logic/fresh [?name]
    (change|annotation change ?annotation)
    (annotation-name|equals ?annotation "Ignore")))

(defn change|annotation|beforeclass [change ?annotation]
  (logic/fresh [?name]
    (change|annotation change ?annotation)
    (annotation-name|equals ?annotation"BeforeClass")))

(defn change|annotation|afterclass [change ?annotation]
  (logic/fresh [?name]
    (change|annotation change ?annotation)
    (annotation-name|equals ?annotation "AfterClass")))


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

(defn change-classifier [?change ?type]
  (logic/all
    (logic/conde
      [(logic/onceo (classify-assert ?change ?type))]
      [(logic/onceo (classify-findby ?change ?type))]
      [(logic/onceo (classify-constantupdate ?change ?type))]
      [(logic/onceo (classify-command ?change ?type))]
      [(logic/onceo (classify-demarcator ?change ?type))])))

(defn write-out-changes [project-name version predecessor changes info changetype]
    (let [commitno (graph/revision-number version)
          predno (graph/revision-number predecessor)
          path (:file info)]
        (sql/insert! +db-specs+ "change_classification"
          {:path (:file info) :repo_key project-name
           :commitno commitno :changes changes
           :changetype changetype :predecessor predno})))

(defn add-classified-changes [repo commit file classified]
  (let [results (reduce
                  (fn [results [change types]]
                    (let [ctype (change-type change)]
                      (assoc results ctype
                        (reduce
                          (fn [result category]
                            (update-in result [category] #(if (nil? %) 1 (inc %))))
                          (get results ctype)
                          types))))
                  {}
                  classified)
         id (commit-id commit)
         repo-key (repo-name repo)]
    (doall
      (map
        (fn [ctype]
          (let [classifieds (get results ctype)]
            (doall
              (map
                (fn [category]
                  (let [amount (get classifieds category)]
                    (sql/insert! +db-specs+ "classified_changes"
                       {:repo_key repo-key
                        :commitno id :file file :amount amount
                        :change_type ctype :type category})))
                (keys classifieds)))))
        (keys results)))))

(defn classify-changes [graph]
  (let [results
        (logic/run* [?end ?pred ?change ?classification ?file]
          (logic/fresh []
            (qwal graph root ?end []
              (q=>+)
              (l/in-git-info [curr]
                (l/fileinfo|edit ?info curr)
                (fileinfo|selenium ?info))
              (l/in-source-code [curr]
                (l/fileinfo|compilationunit ?info ?right-cu curr))
              qwal/q<= 
              (l/in-source-code [curr]
                (logic/== ?pred curr)
                (ast/compilationunit|corresponding ?right-cu ?left-cu)
                (change/change ?change ?left-cu ?right-cu)
                (change-classifier ?change ?classification)))))]
    (doall
      (map graph/ensure-delete (:versions graph)))
    (let [partioned-version (group-by first results)]
      (map 
        (fn [version]
          (let [result (get partioned-version version)
                grouped (group-by #(nth % 3) result)]
            (map 
              (fn [type]
                (let [val (get grouped type)
                      grouped-change-type (group-by #(:operation (nth val 2)) val)]
                  (map
                    (fn [change-type]
                      (let [[version pred change class file] (get grouped-change-type change-type)
                            no-changes (count val)]
                        (sql/insert! +db-specs+ "classified_changes"
                          {:repo_key (:name graph)
                           :commitno (graph/revision-number version)
                           :file (:file file) :amount no-changes
                           :change_type change-type :type type})))
                    (keys grouped-change-type))))
              (keys grouped))))
        (keys partioned-version)))))
