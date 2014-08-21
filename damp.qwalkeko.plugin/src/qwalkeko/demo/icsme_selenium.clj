(ns qwalkeko.demo.icsme-selenium
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

;;;;Video available on QwalKeko's wiki
;; https://github.com/ReinoutStevens/damp.qwalkeko/wiki/Video-Demonstration

;;;;Ensure we have properly setup a project
;; we will use the motech project for this example
;; it can be found here: https://github.com/gxa/atlas
;; its clone url is: https://github.com/gxa/atlas.git


;; clone the repository locally, and add it to eclipse by going to the qwalkeko menu and selecting the .git folder
;; make sure to first add the history nature before adding the ekeko nature, as the history nature changes
;; what kind of model is being created



;; first of all we need to get a graph object we can use
(def a-model (first (damp.ekeko.ekekomodel/all-project-models)))
(def a-graph (graph/convert-model-to-graph a-model))
(def a-root (first (:roots a-graph)))

;;lets limit ourselves to the first 50 revisions just so we dont lose all day
(def a-graph (assoc a-graph :versions (take 50 (sort-by graph/date (:versions a-graph)))))

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