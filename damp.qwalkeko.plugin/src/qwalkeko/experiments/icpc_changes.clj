(ns qwalkeko.experiments.icpc_changes
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt.astnode :as astnode])
  (:require [damp.ekeko.logic :as el])
  (:require [qwalkeko.clj.logic :as l])
  (:require [qwalkeko.clj.ast :as ast])
  (:require [damp.qwal :as qwal])
  (:require [qwalkeko.clj.graph :as graph])
  (:require [qwalkeko.clj.functionalnodes :as changes])
  (:require [qwalkeko.clj.changenavigation :as nav])
  (:require [damp.ekeko.jdt
             [ast :as jdt]]))



(def left
  (first
    (logic/run 1 [?left]
      (logic/fresh [?typedecl ?tname]
        (jdt/ast :CompilationUnit ?left)
        (ast/compilationunit-typedeclaration|main ?left ?typedecl)
        (jdt/has :name ?typedecl ?tname)
        (jdt/name|simple-string ?tname "Test")))))

(def right
  (first
    (logic/run 1 [?left]
      (logic/fresh [?typedecl ?tname]
        (jdt/ast :CompilationUnit ?left)
        (ast/compilationunit-typedeclaration|main ?left ?typedecl)
        (jdt/has :name ?typedecl ?tname)
        (jdt/name|simple-string ?tname "Test2")))))

(def methods
  (first
    (logic/run 1 [?mA ?mB]
      (logic/fresh [?parametersA ?parametersB ?name]
        (ast/method-cu-method-cu|same-name ?mA left ?mB right)
        (jdt/has :name ?mA ?name)
        (jdt/name|simple-string ?name "setJava")
        (jdt/has :parameters ?mA ?parametersA)
        (jdt/has :parameters ?mB ?parametersB)))))

(def left-meth (first methods))
(def right-meth (second methods))

(def left-para
  (astnode/value-unwrapped
    (first 
      (logic/run 1 [?left-para]
        (jdt/has :parameters left-meth ?left-para)))))

(def right-para
  (astnode/value-unwrapped
    (first 
      (logic/run 1 [?left-para]
        (jdt/has :parameters right-meth ?left-para)))))


(def a-model (first (damp.ekeko.ekekomodel/all-project-models)))
(def a-graph (graph/convert-model-to-graph a-model))
(def a-root (first (:roots a-graph)))


(def refactored-version ;;extracted method from paper
  (first
    (filter #(= "d97f4f390c59827af8a3bfe380c1bf7d4a4a84a9" 
               (graph/revision-number %)) 
      (:versions a-graph))))

(def pre-refactored-version
  (first
    (graph/predecessors refactored-version)))

(def projects
  (list
   ; ["07b710cc96c363b69d4e7225c96ffc0296354597" "34dc5127ac1a581305f7b89cc9801f1624b2e039" "Jar"]
    ["821004a5407db28a7482cff8f928d92b7615ea36" "d97f4f390c59827af8a3bfe380c1bf7d4a4a84a9" "WeblogicDeploymentTool"]))

(defn field|introduced [left right ?field]
  (logic/fresh [?name ?other ?other-name ?fragment ?other-fragment]
    (ast/child+-type right :FieldDeclaration ?field)
    (jdt/child :fragments ?field ?fragment)
    (jdt/has :name ?fragment ?name)
    (el/fails
      (logic/all
        (ast/child+-type left :FieldDeclaration ?other)
        (jdt/child :fragments ?other ?other-fragment)
        (jdt/has :name ?other-fragment ?other-name)
        (jdt/name|simple-name|simple|same ?name ?other-name)))))


(defn ast|literal [?literal]
  (logic/conde
    [(jdt/ast :StringLiteral ?literal)]
    [(jdt/ast :NumberLiteral ?literal)]
    [(jdt/ast :BooleanLiteral ?literal)]
    [(jdt/ast :NullLiteral ?literal)]
    [(jdt/ast :TypeLiteral ?literal)]))

(defn literal-value [?literal ?value]
  (logic/conde
    [(logic/fresh [?wrapped]
       (jdt/ast :StringLiteral ?literal)
       (jdt/has :escapedValue ?literal ?wrapped)
       (jdt/value-raw ?wrapped ?value))]
    [(logic/fresh [?wrapped]
       (jdt/ast :NumberLiteral ?literal)
       (jdt/has :token ?literal ?wrapped)
       (jdt/value-raw ?wrapped ?value))]
    [(logic/fresh [?wrapped]
       (jdt/ast :BooleanLiteral ?literal)
       (jdt/has :booleanValue ?literal ?wrapped)
       (jdt/value-raw ?wrapped ?value))]
      [(jdt/ast :NullLiteral ?literal)
       (logic/== ?value nil)]
      [(jdt/ast :TypeLiteral ?literal)
       (jdt/has :type ?literal ?value)]))

(defn magic-constant [graph]
  (logic/run 1 [?field ?end]
    (nav/step-changes graph ?end 
      [?not-present ?left-method ?literal ?str
       ?right-method ?fragment ?init ?field-name ?field-access
       ?new-literal]
      (nav/in-current-change-state [curr ast]
        (logic/== ast ?not-present)
        (ast/child+-iter ast ?literal)
        (literal-value ?literal ?str)
        (jdt/ast-parent+ ?literal ?left-method)
        (jdt/ast :MethodDeclaration ?left-method))
      nav/change==>*
      (nav/in-current-change-state [curr ast]
        (field|introduced ?not-present ast ?field)
        (ast/method-cu-method-cu|corresponding ?left-method ?not-present ?right-method ast)
        (jdt/child :fragments ?field ?fragment)
        (jdt/has :initializer ?fragment ?init)
        (literal-value ?init ?str)
        (jdt/has :name ?fragment ?field-name)
        (jdt/child+ ?right-method ?field-access)
        (jdt/ast :SimpleName ?field-access)
        (jdt/name|simple-name|simple|same ?field-name ?field-access)
       ; (el/fails
       ;   (logic/all
       ;     (jdt/child+ ?right-method ?new-literal)
       ;     (literal-value ?new-literal ?str)))))))
        ))))

(defn field-introduced [graph]
  (logic/run 1 [?field ?end]
    (nav/step-changes graph ?end [?not-present]
      (nav/in-current-change-state [curr ast]
        (logic/== ast ?not-present))
      nav/change==>*
      (nav/in-current-change-state [curr ast]
        (field|introduced ?not-present ast ?field)))))

(defn field-introduced-limited [graph]
  (logic/run* [?field ?end]
    (nav/step-changes graph ?end [?not-present]
      (nav/in-current-change-state [curr ast]
        (logic/== ast ?not-present))
      (nav/change-limit-> 3)
      (nav/in-current-change-state [curr ast]
        (field|introduced ?not-present ast ?field)))))

(defn field-introduced-solutions [graph]
  (let [solutions (qwalkeko.clj.graph-algo/all-subset-changes graph)
        changes (map #(qwalkeko.clj.graph-algo/solution-ordered graph %) solutions)]
    (first
    (remove
      empty?
      (map
        (fn [c]
          (doall
            (logic/run 1 [?field ?end]
              (nav/step-changes graph ?end [?not-present]
                (nav/in-current-change-state [curr ast]
                  (logic/== ast ?not-present))
                (nav/change-sol-> c)
                (nav/in-current-change-state [curr ast]
                  (field|introduced ?not-present ast ?field))))))
        changes)))))


  
(defn graph-project [[pre-ref ref fname]]
  (let [pre-refactored  
        (first
          (filter #(= pre-ref (graph/revision-number %)) (:versions a-graph)))
        refactored
        (first
          (filter #(= ref (graph/revision-number %)) (:versions a-graph)))
        left
        (first
         (l/qwalkeko 1 [?left]
           (qwal/qwal a-graph pre-refactored pre-refactored []
             (l/in-source-code [curr]
               (logic/fresh [?typedecl ?tname]
                 (jdt/ast :CompilationUnit ?left)
                 (ast/compilationunit-typedeclaration|main ?left ?typedecl)
                 (jdt/has :name ?typedecl ?tname)
                 (jdt/name|simple-string ?tname fname))))))
        right
        (first
          (l/qwalkeko 1 [?left]
            (qwal/qwal a-graph refactored refactored []
              (l/in-source-code [curr]
                (logic/fresh [?typedecl ?tname]
                  (jdt/ast :CompilationUnit ?left)
                  (ast/compilationunit-typedeclaration|main ?left ?typedecl)
                  (jdt/has :name ?typedecl ?tname)
                  (jdt/name|simple-string ?tname fname))))))
        nav-graph
        (nav/ast-ast-navigatable-graph left right)]
    nav-graph))

(defn compute-project [f pro]
  (let [nav-graph (graph-project pro)]
    (f pro)))

(map #(compute-project field-introduced %) projects)
    
   
;;field declaration inserted
(logic/run 1 [?end]
  (logic/fresh [?field]
    (nav/step-changes nav-graph ?end [?not-present]
      (nav/in-current-change-state [curr ast]
        (logic/== ast ?not-present))
      nav/change==>*
      (nav/in-current-change-state [curr ast]
        (field|introduced ?not-present ast ?field)))))

;;Magic Constant replaced
(logic/run 1 [?end]
  (logic/fresh [?field]
    (nav/step-changes nav-graph ?end 
      [?not-present ?left-method ?literal ?str
       ?right-method ?fragment ?init ?field-name ?field-access
       ?new-literal]
      (nav/in-current-change-state [curr ast]
        (logic/== ast ?not-present)
        (ast/child+-iter ast ?literal)
        (literal-value ?literal ?str)
        (jdt/ast-parent+ ?literal ?left-method)
        (jdt/ast :MethodDeclaration ?left-method))
      nav/change==>*
      (nav/in-current-change-state [curr ast]
        (field|introduced ?not-present ast ?field)
        (ast/method-cu-method-cu|corresponding ?left-method ?not-present ?right-method ast)
        (jdt/child :fragments ?field ?fragment)
        (jdt/has :initializer ?fragment ?init)
        (literal-value ?init ?str)
        (jdt/has :name ?fragment ?field-name)
        (jdt/child+ ?right-method ?field-access)
        (jdt/ast :SimpleName ?field-access)
        (jdt/name|simple-name|simple|same ?field-name ?field-access)
       ; (el/fails
       ;   (logic/all
       ;     (jdt/child+ ?right-method ?new-literal)
       ;     (literal-value ?new-literal ?str)))))))
        ))))
        
