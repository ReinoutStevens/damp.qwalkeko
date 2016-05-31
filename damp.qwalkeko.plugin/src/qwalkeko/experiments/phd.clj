(ns qwalkeko.experiments.phd
  (:require [damp.ekeko.logic :as el])
   (:require [clojure.core.logic :as logic])
   (:require [qwalkeko.clj.logic :as l])
   (:require [qwalkeko.clj.graph :as graph])
   (:require [qwalkeko.clj.functionalnodes :as changes])
   (:require [qwalkeko.clj.changenavigation :as nav])
   (:require [qwalkeko.clj.ast :as ast])
   (:require [damp.ekeko.jdt.astnode :as astnode])
   (:require [damp.ekeko.jdt
              [ast :as jdt]])
   (:require [damp.qwal :as qwal]))



(def left
  (damp.ekeko/ekeko 1 [?left]
            (l/in-source-code [c]
              (jdt/ast :TypeDeclaration ?type)
              (jdt/has :name ?type ?name)
              (jdt/name|simple-string ?name "Test")
              (jdt/ast-typedeclaration|encompassing ?type ?left))))

(def right
  (damp.ekeko/ekeko 1 [?right]
            (l/in-source-code [c]
              (jdt/ast :TypeDeclaration ?type)
              (jdt/has :name ?type ?name)
              (jdt/name|simple-string ?name "Test2")
              (jdt/ast-typedeclaration|encompassing ?type ?right))))




;;selenium shizzle
(def a-model (first (damp.ekeko.ekekomodel/all-project-models)))
(def a-graph (graph/convert-model-to-graph a-model))
(def a-root (first (:roots a-graph)))


;;selenium with clojure
(defn file-to-cu [project efile]
  (let [path (.toOSString (.getRawLocation efile))
        contents (slurp path)
        parser (org.eclipse.jdt.core.dom.ASTParser/newParser org.eclipse.jdt.core.dom.AST/JLS8)]
    (.setSource parser (.toCharArray contents))
    (.setKind parser org.eclipse.jdt.core.dom.ASTParser/K_COMPILATION_UNIT)
    (.setUnitName parser (.toString (.getLocation efile)))
    ;(.setProject parser project)
    (.createAST parser nil)))
    
(defn get-compilation-unit [eclipse-name file-name]
  (let [workspace (org.eclipse.core.resources.ResourcesPlugin/getWorkspace)
        root (.getRoot workspace)
        project (.getProject root eclipse-name)
        file (.getFile project file-name)]
    (file-to-cu project file)))


(defn process-file [eclipse file]
  (let [cu (get-compilation-unit eclipse (:file file))
        imports (ast/has-clj-unwrapped :imports cu)]
    (some
      (fn [import]
        (let [qualname (ast/has-clj-unwrapped :name import)
              name (ast/has-clj-unwrapped :name qualname)
              id (ast/has-clj-unwrapped :identifier name)]
          (> (.indexOf id ".selenium") 0)))
      imports)))
        

(defn process-version [graph version]
  (let [modified-files (graph/file-infos version)
        added-files (filter #(= (:status %) :add) modified-files)
        java-files (filter #(.endsWith (:file %) ".java") added-files)
        eclipse-name (str (graph/graph-project-name graph) "-" (graph/revision-number version))]
    (when-not (empty? java-files)
      (graph/ensure-checkout version))
    (let [result (doall (remove nil? (map
                                       (fn [file]
                                         (process-file eclipse-name file))
                                       java-files)))]
      (when-not (empty? java-files)
        (graph/ensure-delete version))
      result)))




;;lets limit ourselves to the first 50 revisions just so we dont lose all day
;(def a-graph (assoc a-graph :versions (take 50 (sort-by graph/date (:versions a-graph)))))

;; identify selenium files
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

(defn identify-selenium-files []
  (defn query [version]
    (l/qwalkeko* [?info ?cu ?end]
      (qwal/qwal a-graph version ?end
        []
        (l/in-git-info [curr]
          (l/fileinfo|add ?info curr))
        (l/in-source-code [curr]
          (l/fileinfo|compilationunit ?info ?cu curr)
          (compilationunit|selenium ?cu)))))
  (map 
    #(let [res (query %)]
       (graph/ensure-delete %)
       res)
    (:versions a-graph)))

;;we could store the results in a DB so we can reuse them later
;;we will now just keep the results in memory as we are working with a small dataset
(def results (filter #(not (empty? %)) (identify-selenium-files)))

;;go take a cup of $beverage, this should take 5minutes
;;projects should get open and closed in the Eclipse running QwalKeko

;;this predicate normally queries the aforementioned DB
(defn is-selenium-file? [fileinfo]
  (let [path (:file fileinfo)
        amount (count (filter (fn [[info cu version]]
                                (= (:path info) (:path fileinfo)))
                        results))]
    (> amount 0)))


(defn fileinfo|selenium [fileinfo]
  (logic/project [fileinfo]
    (logic/== true (is-selenium-file? fileinfo))))
    
;;compute changes
(defn selenium-changes []
  (defn query [version]
    (l/qwalkeko* [?left-cu ?right-cu ?info ?end ?change]
      (qwal/qwal a-graph version ?end []
        (l/in-git-info [curr]
          (l/fileinfo|edit ?info curr)
          (fileinfo|selenium ?info))
        (l/in-source-code [curr]
          (l/fileinfo|compilationunit ?info ?right-cu curr))
        qwal/q<= ;;moving back here
        (l/in-source-code [curr]
          (ast/compilationunit|corresponding ?right-cu ?left-cu)
          (change/change ?change ?left-cu ?right-cu)))))
  (map #(let [res (query %)]
          (graph/ensure-delete %)
          (doall (map graph/ensure-delete (graph/predecessors %)))
          res) 
    (:versions a-graph)))


;;go for coffee and a chat with a colleague.
(def changes (filter #(not (empty? %)) (selenium-changes)))

;;now that we have changes we can classify them
;;you can find more examples in experiments.selenium.clj


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
  
(def findbys
  (logic/run* [?change]
    (logic/fresh [?findBy]
      (logic/membero ?change changes)
      (change|affects-findBy ?change ?findBy))))