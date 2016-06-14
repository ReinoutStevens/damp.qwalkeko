(ns qwalkeko.experiments.phd
  (:require [damp.ekeko.logic :as el])
  (:require [clojure.java.jdbc :as sql])
  (:require [clojure.core.logic :as logic])
  (:require [qwalkeko.clj.logic :as l])
  (:require [qwalkeko.clj.graph :as graph])
  (:require [qwalkeko.clj.functionalnodes :as changes])
  (:require [qwalkeko.clj.changenavigation :as nav])
  (:require [qwalkeko.clj.ast :as ast])
  (:require [damp.ekeko.jdt.astnode :as astnode])
  (:require [damp.ekeko.jdt
             [ast :as jdt]])
  (:require [damp.qwal :as qwal])
  (:import [qwalkeko.experiments PhDMetaVersion])
  (:import [org.eclipse.core.resources ResourcesPlugin])
  (:import [qwalkeko GitCommands])
  (:import [org.eclipse.jgit.lib ObjectId])
  (:import [org.eclipse.jgit.api Git])
  (:import [org.eclipse.jgit.revwalk RevWalk])
  (:import [org.eclipse.jgit.diff DiffFormatter])
  (:import [org.eclipse.jgit.diff DiffEntry])
  (:import [org.eclipse.jgit.diff DiffEntry$ChangeType])
  (:import [org.eclipse.jgit.storage.file FileRepositoryBuilder])
  (:import [org.eclipse.jdt.core.dom ASTNode]))

(def +db-path+ "/home/resteven/selenium.db")
(def +db-specs+ {:classname "org.sqlite.JDBC",
                 :subprotocol "sqlite"
                 :subname +db-path+})

;;selenium with clojure
;;temporal part
(defn checkout-commit [repo rev-commit]
  (let [version (PhDMetaVersion. rev-commit (.getName (.getParentFile (.getDirectory repo))))]
    (.openAndCheckoutIfNeeded version)))

(defn delete-commit [repo rev-commit]
  (let [version (PhDMetaVersion. rev-commit (.getName (.getParentFile (.getDirectory repo))))]
    (.closeAndDeleteIfNeeded version)))

(defn read-git-repo [location]
  (let [builder (new FileRepositoryBuilder)]
    (.setGitDir builder (clojure.java.io/file location))
    (.build builder)))

(defn get-walker [repo]
  (let [git (new Git repo)
        walker (-> git .log .all .call)]
    walker))

(defn file-to-cu [project efile]
  (let [path (.toOSString (.getRawLocation efile))
        contents (slurp path)
        parser (org.eclipse.jdt.core.dom.ASTParser/newParser org.eclipse.jdt.core.dom.AST/JLS8)]
    (.setSource parser (.toCharArray contents))
    (.setKind parser org.eclipse.jdt.core.dom.ASTParser/K_COMPILATION_UNIT)
    (.setUnitName parser (.toString (.getLocation efile)))
    (.createAST parser nil)))
    
(defn get-compilation-unit [eclipse-name file-name]
  (let [workspace (org.eclipse.core.resources.ResourcesPlugin/getWorkspace)
        root (.getRoot workspace)
        project (.getProject root eclipse-name)
        file (.getFile project file-name)]
    (file-to-cu project file)))

(defn count-lines [ast]
  (count (clojure.string/split-lines (.toString ast))))


(defn process-file [eclipse file]
  (letfn [(qualified-name-to-str [res name]
            (if (.isSimpleName name)
              (conj res name)
              (recur (conj res (ast/has-clj-unwrapped :name name)) (ast/has-clj-unwrapped :qualifier name))))
          (contains-selenium-import? [cu]
            (let [imports (ast/has-clj-unwrapped :imports cu)]
              (some
                (fn [import]
                  (let [qualname (ast/has-clj-unwrapped :name import)
                        names (qualified-name-to-str '() qualname)
                        ids (map #(ast/has-clj-unwrapped :identifier %) names)]
                    (some
                      #{"selenium"}
                      ids)))
                imports)))]
  (let [cu (get-compilation-unit eclipse file)]
    (when (contains-selenium-import? cu)
      {:loc (count-lines cu)
       :file file}))))
        
(defn get-parents [repo walker commit]
 (let [parsed (.parseCommit walker (.getId commit))]
   (map #(.parseCommit walker (.getId %)) (seq (.getParents commit)))))

(defn get-modified-files [repo walker commit]
  (let [parsed (.parseCommit walker (.getId commit))
        parents (get-parents repo walker commit)
        df (new DiffFormatter (org.eclipse.jgit.util.io.DisabledOutputStream/INSTANCE))]
    (.setRepository df repo)
    (.setDiffComparator df org.eclipse.jgit.diff.RawTextComparator/DEFAULT)
    (.setDetectRenames df false)
    (mapcat #(.scan df (.getTree %) (.getTree parsed)) parents)))

(defn get-modified-files-parent [repo walker commit parent]
  (let [parsed (.parseCommit walker (.getId commit))
        pparent (.parseCommit walker (.getId parent))
        df (new DiffFormatter (org.eclipse.jgit.util.io.DisabledOutputStream/INSTANCE))]
    (.setRepository df repo)
    (.setDiffComparator df org.eclipse.jgit.diff.RawTextComparator/DEFAULT)
    (.setDetectRenames df false)
    (.scan df (.getTree pparent) (.getTree parsed))))

(defn diffentry-added? [de]
  (let [changetype (.getChangeType de)]
    (= changetype DiffEntry$ChangeType/ADD)))

(defn diffentry-modified? [de]
  (let [changetype (.getChangeType de)]
    (= changetype DiffEntry$ChangeType/MODIFY)))

(defn get-path [de]
  (.getNewPath de))

(defn repo-name [repo]
  (-> repo .getDirectory .getParentFile .getName))

(defn commit-id [commit]
  (ObjectId/toString (.getId commit)))


(defn add-changed-file [repo commit path loc]
  (let [repo-key (repo-name repo)
        id (commit-id commit)]
    (sql/insert! +db-specs+ "selenium"
      {:path path :repo_key repo-key
       :commitno id :loc loc})))

(defn is-selenium-file? [path]
  (let [amount
        (count (sql/query +db-specs+
                 ["select * from selenium where path = ? limit 1", path]))]
    (> amount 0)))

(defn process-commit [repo walker commit]
  (let [modified-files (get-modified-files repo walker commit)
        added-files (map get-path (filter diffentry-added? modified-files))
        java-files (filter #(.endsWith % ".java") added-files)
        eclipse-name (str (repo-name repo) "-" (commit-id commit))]
    (when-not (empty? java-files)
      (checkout-commit repo commit))
    (let [results (doall (remove nil?  (map
                                   (fn [file]
                                     (process-file eclipse-name file))
                                   java-files)))]
      (when-not (empty? java-files)
        (delete-commit repo commit))
      (doall
        (map (fn [result] (add-changed-file repo commit (:file result) (:loc result))) results))
      results)))

(defn process-repo [location]
  (let [repo (read-git-repo location)
        walker (get-walker repo)]
    (time
      (doall (map #(process-commit repo walker %) (seq walker))))))


;;clojure change classification
(defn node-get-parent-nodes [node]
  (take-while #(not (nil? %)) 
    (iterate #(.getParent %) node)))

(defn insert-get-affected-nodes [insert]
  (let [original (.getOriginal insert)]
    (node-get-parent-nodes original)))

(defn move-get-affected-nodes [move]
  (let [source (.getOriginal move)
        target (.getRightNode move)]
    (concat 
      (node-get-parent-nodes source)
      (node-get-parent-nodes target))))

(defn delete-get-affected-nodes [delete]
  (let [removed (.getOriginal delete)]
    (node-get-parent-nodes removed)))
    
(defn update-get-affected-nodes [update]
  (let [node (.getOriginal update)]
    (node-get-parent-nodes node)))

(defn change-get-affected-nodes [change]
  (cond
    (.isInsert change) (insert-get-affected-nodes change)
    (.isMove change) (move-get-affected-nodes change)
    (.isDelete change) (delete-get-affected-nodes change)
    (.isUpdate change) (update-get-affected-nodes change)))

(defn ast-constant? [ast]
  (#{org.eclipse.jdt.core.dom.ASTNode/NUMBER_LITERAL
     org.eclipse.jdt.core.dom.ASTNode/BOOLEAN_LITERAL
     org.eclipse.jdt.core.dom.ASTNode/CHARACTER_LITERAL
     org.eclipse.jdt.core.dom.ASTNode/SIMPLE_NAME
     org.eclipse.jdt.core.dom.ASTNode/QUALIFIED_NAME
     org.eclipse.jdt.core.dom.ASTNode/STRING_LITERAL}
    (.getNodeType ast)))
  

(defn ast-assert? [node]
  (when-not (nil? node)
    (= (.getNodeType node) org.eclipse.jdt.core.dom.ASTNode/ASSERT_STATEMENT)))

(defn ast-markerannotation? [node]
  (when-not (nil? node)
    (= (.getNodeType node) org.eclipse.jdt.core.dom.ASTNode/MARKER_ANNOTATION)))

(defn ast-normalannotation? [node]
  (when-not (nil? node)
    (= (.getNodeType node) org.eclipse.jdt.core.dom.ASTNode/NORMAL_ANNOTATION)))

(defn ast-methodinvocation? [node]
  (when-not (nil? node)
    (= (.getNodeType node) org.eclipse.jdt.core.dom.ASTNode/METHOD_INVOCATION)))

(defn ast-simplename? [node]
  (when-not (nil? node)
    (= (.getNodeType node) org.eclipse.jdt.core.dom.ASTNode/SIMPLE_NAME)))

(defn method-findby? [node]
  (when (ast-methodinvocation? node)
    (let [exp (ast/has-clj-unwrapped :expression node)]
      (when (ast-simplename? exp)
        (= (.getIdentifier exp) "FindBy")))))

(defn annotation-findby? [node]
  (when (ast-normalannotation? node)
    (let [name (.getIdentifier (ast/has-clj-unwrapped :typeName node))]
      (= name "FindBy"))))


(defn get-annotation-name [node]
  (ast/has-clj-unwrapped :typeName node))

(defn annotation-test? [annotation]
  (= "Test" (get-annotation-name)))

(defn annotation-ignore? [annotation]
  (= "Ignore" (get-annotation-name)))

(defn annotation-beforeclass? [annotation]
  (= "BeforeClass" (get-annotation-name)))

(defn annotation-afterclass? [annotation]
  (= "AfterClass" (get-annotation-name)))

(defn ast-inspector? [ast]
  (when (ast-methodinvocation? ast)
    (let [name (.getIdentifier (ast/has-clj-unwrapped :name ast))]
      (#{"getAttribute"
         "getCssValue"
         "getSize"
         "getTagName"
         "getText"
         "isDisplayed"
         "isEnabled"
         "isSelected"}
        name))))

(defn ast-command? [ast]
  (when (ast-methodinvocation? ast)
    (let [name (.getIdentifier (ast/has-clj-unwrapped :name ast))]
      (#{"clear"
         "click"
         "sendKeys"
         "submit"}
        name))))

(defn ast-assignment? [ast]
  (when-not (nil? ast)
    (= (.getNodeType ast) org.eclipse.jdt.core.dom.ASTNode/ASSIGNMENT)))

(defn ast-driver? [ast]
  (when (ast-assignment? ast)
    (let [lefthand (ast/has-clj-unwrapped :leftHandSide ast)]
      (when (ast-simplename? lefthand)
        (= (.getIdentifier lefthand) "driver")))))
  
(defn ast-classinstancecreation? [x]
  (when-not (nil? x)
    (= (.getNodeType x) org.eclipse.jdt.core.dom.ASTNode/CLASS_INSTANCE_CREATION)))

(defn classinstancecreation-pageobject? [ast]
  (when (ast-classinstancecreation? ast)
    (let [type (ast/has-clj-unwrapped :type ast)]
      (when-not (nil? type)
        (let [name (ast/has-clj-unwrapped :name type)]
          (when-not (nil? name)
            (.endsWith (.getIdentifier name) "Page")))))))

(defn classify-assert [change]
  (let [affected (change-get-affected-nodes change)]
    (some ast-assert? affected)))

(defn classify-findby [change]
  (let [affected (change-get-affected-nodes change)]
    (some (fn [node]
            (or
              (annotation-findby? node)
              (method-findby? node)))
      affected)))

(defn classify-pageobject [change]
  (let [affected (change-get-affected-nodes change)]
    (some classinstancecreation-pageobject? affected)))

(defn classify-constantupdate [change]
  (when (.isUpdate change)
    (let [affected (change-get-affected-nodes change)]
      (some ast-constant? affected))))

(defn classify-driver [change]
  (let [affected (change-get-affected-nodes change)]
    (some ast-driver? affected)))

(defn classify-command [change]
  (let [affected (change-get-affected-nodes change)]
    (some ast-command? affected)))

(defn classify-demarcator [change]
  (when (ast-markerannotation? change)
    (or
      (annotation-afterclass? change)
      (annotation-beforeclass? change)
      (annotation-ignore? change)
      (annotation-test? change))))

(defn classify-change [change]
  (remove nil?
    (list
      (if (classify-assert change) :assert)
      (if (classify-findby change) :findby)
      (if (classify-pageobject change) :pageobject)
      (if (classify-constantupdate change) :constant)
      (if (classify-driver change) :driver)
      (if (classify-command change) :command))))

(defn change-type [change]
  (cond
    (.isUpdate change) :update
    (.isInsert change) :insert
    (.isMove change) :move
    (.isDelete change) :delete))


(defn add-total-changes [repo commit file changes]
  (let [id (commit-id commit)
        repo-key (repo-name repo)]
    (sql/insert! +db-specs+ "total_changes"
      {:repo_key repo-key :commitno id :file file 
       :changes (count changes)})))

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

(defn add-change-result [repo commit file changes classified]
  ;;total changes
  (add-total-changes repo commit file changes)
  (add-classified-changes repo commit file classified))

(defn classify-all-changes [repo]
  (letfn 
    [(process-commit-changes [repo walker commit]
       (letfn 
         [(project-name [commit]
            (str (repo-name repo) "-" (commit-id commit)))
          (process-file [commit parent file]
            (let [left (get-compilation-unit (project-name parent) file)
                  right (get-compilation-unit (project-name commit) file)
                  changes 
                  (try 
                    (changes/get-java-changes left right)
                    (catch Exception e '()))
                  classified (map classify-change changes)
                  partitioned (remove #(empty? (second %)) (partition 2 (interleave changes classified)))]
              [changes partitioned]))
          (process-commit-parent [commit prev]
            (let [modified-files (filter diffentry-modified? (get-modified-files-parent repo walker commit prev))
                  seleniums (filter is-selenium-file? (map get-path modified-files))]
              (when (not (empty? seleniums))
                (checkout-commit repo commit)
                (checkout-commit repo prev))
              (let [results (map
                              (fn [file]
                                (conj (process-file commit prev file) file)) seleniums)]
              (doall
                (map
                  (fn [[changes interleaved file]]
                    (add-change-result repo commit file changes interleaved))
                  results)))))]
         (let [preds (get-parents repo walker commit)]
           (doall
             (map #(process-commit-parent commit %) preds))
           (doall
             (map #(delete-commit repo %) preds))
           (delete-commit repo commit))))]
    (let [walker (get-walker repo)]
      (doall
        (map
          (fn [commit]
            (process-commit-changes repo walker commit))
          (seq walker))))))

(defn classify-changes-location [location]
  (let [repo (read-git-repo location)]
    (time
      (classify-all-changes repo))))
