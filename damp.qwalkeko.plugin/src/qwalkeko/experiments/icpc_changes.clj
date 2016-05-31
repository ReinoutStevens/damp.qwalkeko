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


(def ant-projects
  (list
   ["07b710cc96c363b69d4e7225c96ffc0296354597" "34dc5127ac1a581305f7b89cc9801f1624b2e039" "Jar"]
   ["821004a5407db28a7482cff8f928d92b7615ea36" "d97f4f390c59827af8a3bfe380c1bf7d4a4a84a9" "WeblogicDeploymentTool"]
   ["6231c77d24ce79d308b50e46097851fa8dc63e93" "a794b2b204995a03c2e3c117ff2c4749710e6840" "FixCRLF"]))

(def jmeter-projects
  (list
    ;["83c789314d92cc998b022447286c01debba9da97" "31ecdbb04d2bc04d4a64ec3274e1ff3fc32e8454" "JMeterUtils"]
    ["43dfc6ac77fb1ce03b948eb854bae04699605dc3" "b57a7b3a8656073c9052d44883b7cc6915daa917" "AuthPanel"]
    ;["43dfc6ac77fb1ce03b948eb854bae04699605dc3" "09714800c98388cc247078e5aa6727e0f7092a36" "CookiePanel"]
    ["50b60b298ae941baf40709fa3d32f4eae1117936" "3a53a0a6ed2a1aba7c15bcdc8fae997c73644b60" "HTTPSampler"]
    ["a5a669904f40908fa6bb13361a76f08c133f923e" "82759176fc814202431f4794472f91b9d455b8ee" "HTTPSampler"]))

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
      nav/change!=>*
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
        
(defn minimize-solution [graph end-state f]
  (defn validate-changes [solution]
    (let [ordered (qwalkeko.clj.graph-algo/solution-ordered graph solution)
          real-changes (map #(nth (:changes graph) %) ordered)
          new-graph (reduce
                      nav/change-apply
                      graph
                      real-changes)]
      (f new-graph)))
  (defn select-changes [applied-ids solution]
    (if (empty? applied-ids)
      solution
      (let [remove-id (first applied-ids)
            dependencies (conj (nav/graph-change-dependents-recursive graph remove-id) remove-id)
            new-solution (remove (fn [x] (some #{x} dependencies)) solution)
            new-applied (remove (fn [x] (some #{x} dependencies)) applied-ids)]
        (if (validate-changes new-solution) ;;we can remove that change and its dependencies
          (recur new-applied new-solution)
          (recur (rest applied-ids) solution)))))
  (let [applied-ids (filter #(nth (:applied end-state) %) (range (count (:changes end-state))))]
    (select-changes applied-ids applied-ids)))


(defn magic-constant-checker [graph]
  (let [left (:left graph)
        curr (first (:asts graph))]
  (not 
    (empty?
      (logic/run 1 [?field]
          (logic/fresh 
            [?left-method ?literal ?str
             ?right-method ?fragment ?init ?field-name ?field-access
             ?new-literal]
            (ast/child+-iter left ?literal)
            (literal-value ?literal ?str)
            (jdt/ast-parent+ ?literal ?left-method)
            (jdt/ast :MethodDeclaration ?left-method)
            (field|introduced left curr ?field)
            ;;right part
            (ast/method-cu-method-cu|corresponding ?left-method left ?right-method curr)
            (jdt/child :fragments ?field ?fragment)
            (jdt/has :initializer ?fragment ?init)
            (literal-value ?init ?str)
            (jdt/has :name ?fragment ?field-name)
            (jdt/child+ ?right-method ?field-access)
            (jdt/ast :SimpleName ?field-access)
            (jdt/name|simple-name|simple|same ?field-name ?field-access)))))))


(defn field-introduced-checker [graph]
  (let [left (:left graph)
        curr (first (:asts graph))]
    (not
      (empty?
        (logic/run 1 [?field]
          (field|introduced left curr ?field))))))



;;remove method
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

     
;(get-compilation-unit "0678-org.eclipse.jdt.ui-BEFORE" "ui/org/eclipse/jdt/internal/ui/javaeditor/JavaEditor.java")
;(get-compilation-unit "0678-org.eclipse.jdt.ui-AFTER" "ui/org/eclipse/jdt/internal/ui/javaeditor/JavaEditor.java")

;(get-compilation-unit "2910-org.eclipse.jdt.ui-BEFORE" "ui/org/eclipse/jdt/internal/ui/navigator/JavaNavigatorContentProvider.java"))
;(get-compilation-unit "2910-org.eclipse.jdt.ui-AFTER" "ui/org/eclipse/jdt/internal/ui/navigator/JavaNavigatorContentProvider.java"))

;(get-compilation-unit "2722-org.eclipse.jdt.ui-BEFORE" "core extension/org/eclipse/jdt/internal/corext/codemanipulation/StubUtility2.java"))
;(get-compilation-unit "2722-org.eclipse.jdt.ui-AFTER" "core extension/org/eclipse/jdt/internal/corext/codemanipulation/StubUtility2.java"))

;(get-compilation-unit "0277-org.eclipse.jdt.ui.tests-BEFORE" "ui/org/eclipse/jdt/ui/tests/quickfix/MarkerResolutionTest.java"))
;(get-compilation-unit "0277-org.eclipse.jdt.ui.tests-AFTER" "ui/org/eclipse/jdt/ui/tests/quickfix/MarkerResolutionTest.java"))

;(get-compilation-unit "2810-org.eclipse.jdt.ui-BEFORE" "core refactoring/org/eclipse/jdt/internal/corext/refactoring/code/SourceAnalyzer.java"))
;(get-compilation-unit "2810-org.eclipse.jdt.ui-AFTER" "core refactoring/org/eclipse/jdt/internal/corext/refactoring/code/SourceAnalyzer.java"))

;(get-compilation-unit "2810-org.eclipse.jdt.ui-BEFORE" "core refactoring/org/eclipse/jdt/internal/corext/refactoring/code/SourceProvider.java"))
;(get-compilation-unit "2810-org.eclipse.jdt.ui-AFTER" "core refactoring/org/eclipse/jdt/internal/corext/refactoring/code/SourceProvider.java"))

;(get-compilation-unit "2810-org.eclipse.jdt.ui-BEFORE" "core refactoring/org/eclipse/jdt/internal/corext/refactoring/code/InlineMethodRefactoring.java"))
;(get-compilation-unit "2810-org.eclipse.jdt.ui-AFTER" "core refactoring/org/eclipse/jdt/internal/corext/refactoring/code/InlineMethodRefactoring.java"))



(defn remove-java-method [graph]
  (logic/run 1 [?end]
    (nav/step-changes graph ?end [?method ?name ?corresponding ?cname]
      (nav/in-current-change-state [curr ast]
        (jdt/child+ ast ?method)
        (jdt/ast :MethodDeclaration ?method)
        (jdt/has :name ?method ?name))
      nav/change==>*
      (nav/in-current-change-state [curr ast]
        (el/fails
          (logic/all
            (jdt/child+ ast ?corresponding)
            (jdt/ast :MethodDeclaration ?corresponding)
            (jdt/has :name ?corresponding ?cname)
            (jdt/name|simple-name|simple|same ?name ?cname)))))))
          
(defn ast-method|unused [ast ?method]
  (logic/fresh [?name]
    (jdt/has :name ?method ?name)
    (el/fails
      (logic/all [?invoc ?invocname]
        (jdt/child+ ast ?invoc)
        (jdt/ast :MethodInvocation ?invoc)
        (jdt/has :name ?invoc ?invocname)
        (jdt/name|simple-name|simple|same ?name ?invocname)))))

(defn remove-java-method-checker [graph]
 (let [left (:left graph)
       curr (first (:asts graph))]
   (not
     (empty?
       (logic/run 1 [?method]
         (logic/fresh [?name ?corresponding ?cname]
           (jdt/child+ left ?method)
           (jdt/ast :MethodDeclaration ?method)
           (jdt/has :name ?method ?name)
           ;;right part
           (el/fails
             (logic/all
               (jdt/child+ curr ?corresponding)
               (jdt/ast :MethodDeclaration ?corresponding)
               (jdt/has :name ?corresponding ?cname)
               (jdt/name|simple-name|simple|same ?name ?cname)))))))))


;;field rename
(defn field-name|accesses [?field ?ast]
  (logic/fresh [?fragment ?fname ?oname]
    (jdt/ast :FieldDeclaration ?field)
    (jdt/child :fragments ?field ?fragment)
    (jdt/has :name ?fragment ?fname)
    (logic/conde
      [(jdt/ast :FieldAccess ?ast)
       (jdt/has :name ?ast ?oname)
       (jdt/name|simple-name|simple|same ?fname ?oname)]
      [(jdt/ast :SimpleName ?ast)
       (jdt/name|simple-name|simple|same ?fname ?ast)
       (jdt/ast-parent ?ast ?oname)
       (logic/conde
         [(jdt/ast :Expression ?oname)]
         [(jdt/ast :Statement ?oname)])]
       )))
      
(defn type-type|same-name [?typeA ?typeB]
  (logic/fresh [?nameA ?nameB ?rawA ?rawB]
    (logic/conde
      [(jdt/ast :SimpleType ?typeA)
       (jdt/ast :SimpleType ?typeB)
       (jdt/has :name ?typeA ?nameA)
       (jdt/has :name ?typeB ?nameB)
       (jdt/name|simple-name|simple|same ?nameA ?nameB)]
      [(jdt/ast :PrimitiveType ?typeA)
       (jdt/ast :PrimitiveType ?typeB)
       (jdt/has :primitiveTypeCode ?typeA ?nameA)
       (jdt/has :primitiveTypeCode ?typeB ?nameB)
       (jdt/value-raw ?nameA ?rawA)
       (jdt/value-raw ?nameB ?rawB)
       (logic/== ?rawA ?rawB)])))

(defn ast-ast-field|introduced [right left ?field]
  (logic/fresh [?name ?fragment]
    (jdt/child+ right ?field)
    (jdt/ast :FieldDeclaration ?field)
    (jdt/child :fragments ?field ?fragment)
    (jdt/has :name ?fragment ?name)
    (el/fails
      (logic/fresh [?ofield ?ofragment ?oname]
        (jdt/child+ left ?ofield)
        (jdt/ast :FieldDeclaration ?ofield)
        (jdt/child :fragments ?ofield ?ofragment)
        (jdt/has :name ?ofragment ?oname)
        (jdt/name|simple-name|simple|same ?name ?oname)))))
        

(defn field-rename [graph]
  (logic/run 1 [?end]
    (nav/step-changes graph ?end [?original ?field ?renamed]
        (nav/in-current-change-state [curr ast]
          (jdt/child+ ast ?field)
          (jdt/ast :FieldDeclaration ?field)
          (logic/== ast ?original))
        nav/change->
        (nav/in-current-change-state [curr ast]
          (jdt/child+ ast ?renamed)
          (jdt/ast :FieldDeclaration ?renamed)
          (ast-ast-field|introduced ast ?original ?renamed)
          (el/fails
            (logic/fresh [?caccess]
              (jdt/child+ curr ?caccess)
              (field-name|accesses ?field ?caccess)))))))


(defn field-rename-checker [graph]
  (let [left (:left graph)
        curr (first (:asts graph))]
    (not
      (empty?
      (logic/run 1 [?field]
        (logic/fresh [?access ?ftype ?renamed ?rtype]
          (jdt/child+ left ?field)
          (jdt/ast :FieldDeclaration ?field)
          (jdt/child+ left ?access)
          (field-name|accesses ?field ?access)
          (jdt/has :type ?field ?ftype)
          ;;a type with the same 
          (jdt/child+ curr ?renamed)
          (jdt/ast :FieldDeclaration ?renamed)
          (jdt/has :type ?field ?rtype)
          ;(type-type|same-name ?ftype ?rtype)
          ;;ensure that that type was introduced
          (ast-ast-field|introduced curr left ?renamed)
          ;;ensure that the fields are no longer calling
          (el/fails
            (logic/fresh [?caccess]
              (jdt/child+ curr ?caccess)
              (field-name|accesses ?field ?caccess)))
          ))))))


(defn project-left-right [[pre-ref ref fname]]
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
                  (jdt/name|simple-string ?tname fname))))))]
     [left right]))

(defn count-lines [ast]
  (count (clojure.string/split-lines (.toString ast))))

(defn perform-benchmark [left right checker]
  (let [nav-graph (time (nav/ast-ast-navigatable-graph left right))
        sorted (qwalkeko.clj.graph-algo/topo-sort-graph nav-graph)
        solution (time (reduce nav/change-apply nav-graph (map #(nth (:changes nav-graph) %) sorted)))]
    (time (minimize-solution nav-graph solution checker))))
         
;;statistics
(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))


(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (mean [bottom-val top-val])))))


(defn max-distance [sol]
  (let [pairs (partition 2 1 sol)]
    (map (fn [[x y]] (- y x)) pairs)))


(comment
(defn max-distance [sol sorted]
  (let [sol-topo (map (fn [el] (.indexOf sorted el)) sol)
        sol-part (partition 2 (interleave sol sol-topo))
        sol-sorted (sort-by second sol-part)
        pairs (partition 2 1 sol-sorted)]
    (map (fn [[[x xtop] [y ytop]]] (- ytop xtop)) pairs))))

(defn do-more-graph-stuff [nav-graph]
  (let [paths (qwalkeko.clj.graph-algo/paths nav-graph)
        longest (apply max paths)
        medpaths (median paths)
        roots (count (:roots nav-graph))]
    [longest medpaths roots]))

;;runtime changes
;;runtime solution
;;runtime minimize
;;solution
;;top-sort
;;LOC left
;;LOC right
;;distances median
;;longest-path median components


;;first jmeter
"Elapsed time: 2892.790273 msecs"
"Elapsed time: 3934.996153 msecs"
"Elapsed time: 99449.420719 msecs"
(4 24 50 185 199)
(8 185 30 26 244 5 215 2 3 25 28 6 29 243 238 150 223 235 241 214 242 4 149 1 27 7 213 240 233 218 0 37 199 42 18 10 38 16 203 45 14 9 11 202 19 35 23 12 17 34 201 227 198 44 151 43 196 195 41 36 22 13 33 40 32 
  31 21 15 197 204 46 39 225 24 224 20 49 63 80 71 67 209 52 73 207 79 81 78 53 68 57 76 82 47 230 66 228 51 64 61 231 184 56 208 75 50 48 54 77 217 55 69 60 58 72 59 229 210 74 62 70 65 84 177 94 114 105 96 169 
  87 171 124 98 126 173 133 140 138 179 115 97 127 180 107 142 236 152 166 170 103 134 93 122 131 118 167 178 143 109 237 90 168 181 108 172 117 91 129 226 174 139 113 99 116 104 137 141 92 176 144 95 119 88 85 
  102 175 110 161 120 163 123 132 112 145 125 111 89 162 156 136 159 234 106 206 135 205 154 86 128 130 164 121 190 212 160 211 200 158 155 189 191 216 157 165 153 147 220 219 187 193 221 192 222 188 186 183 83 
  146 182 239 101 100 148 194 232)
388
365
(20 26 135 14) 23 
[10 4 31]
;;second jmeter
"Elapsed time: 15621.763339 msecs"
"Elapsed time: 3937.268406 msecs"
"Elapsed time: 1966148.651145 msecs"
(2 14 17 113 118)
(49 42 71 18 30 124 126 120 133 140 123 112 57 19 97 35 23 107 66 2 125 111 93 44 100 41 143 109 22 108 139 136 113 31 21 99 137 92 119 106 85 135 24 69 1 27 20 128 130 110 62 70 121 0 84 8 80 114 10 73 87 98 38 
  79 26 78 14 83 5 9 115 11 76 82 47 12 3 34 25 51 28 6 29 61 89 118 90 13 33 56 91 116 50 48 4 46 88 72 7 94 37 105 96 81 16 132 53 45 138 68 127 17 148 43 122 36 129 32 15 54 95 60 86 59 63 52 145 146 103 131 40
  104 141 144 39 55 102 101 58 67 147 142 117 65 77 134 64 74 75)
926
923
(12 3 96 5) 8.5
[7 2 54]

;;third jmeter
"Elapsed time: 7401.411345 msecs"
"Elapsed time: 620.382381 msecs"
"Elapsed time: 277394.40933 msecs"
(0 4 5 8 11 14)
(14 23 6 24 0 8 10 9 2 3 21 4 1 18 5 11 12 22 13 20 15 17 19 16 7)
846
850
(4 1 3 3 3) 3
[5 2 5]

;;first ant
"Elapsed time: 1407.722907 msecs"
"Elapsed time: 489.131387 msecs"
"Elapsed time: 17516.217716 msecs"
(0 5 12 23)
(63 73 14 35 23 17 28 22 15 1 72 20 0 8 18 10 30 16 5 9 11 2 3 34 25 41 40 31 4 24 27 7 42 26 12 29 43 33 32 49 37 38 45 51 44 36 21 46 39 53 47 56 50 48 54 55 67 52 68 64 61 60 59 62 65 70 58 69 66 57 6 71 13 19)
371
364
(5 7 11) 7
[10 3 13]

;;second ant
"Elapsed time: 2412.007945 msecs"
"Elapsed time: 1305.694869 msecs"
"Elapsed time: 30188.328125 msecs"
(2 10 15 44 45 51 62 82)
(8 10 5 9 11 202 2 3 6 54 4 1 7 0 49 37 42 18 30 38 16 26 45 14 19 35 47 23 12 17 34 25 28 44 29 43 41 36 22 13 33 40 32 31 21 48 15 46 39 24 27 20 63 52 53 57 66 51 64 61 56 60 58 59 62 65 71 73 87 81 78 83 68 82 
  77 85 55 69 72 86 70 84 80 79 76 100 102 101 94 105 96 97 103 50 104 95 67 106 124 123 125 89 90 91 75 92 98 112 107 93 109 108 136 113 110 114 169 120 138 115 166 170 111 122 118 167 168 117 139 99 116 137 119 
  165 121 190 147 126 133 132 127 180 142 152 148 146 134 151 131 143 191 181 129 182 192 135 153 128 130 74 177 161 163 140 179 145 158 189 150 187 178 141 144 149 88 154 188 164 183 185 194 160 171 155 198 184 162 
  196 195 156 172 159 176 197 157 175 186 174 193 201 200 173 199)
314
382
(8 5 29 1 6 11 20) 8
[14 8 14]

;;third
"Elapsed time: 19900.027307 msecs"
"Elapsed time: 44088.607318 msecs"
"Elapsed time: 1990800.523466 msecs"
(0 17 41 109 182 245 283 570 700 1000)
(1243 0 49 8 37 42 18 10 30 38 16 26 45 14 5 9 11 19 35 47 23 2 12 3 17 34 25 28 6 44 29 43 41 36 22 13 33 40 32 31 21 48 15 4 46 39 24 1 27 20 7 84 177 190 94 212 63 183 199 80 71 161 67 147 209 114 52 185 105 96 73 
  194 207 160 169 87 124 98 126 173 211 79 120 81 163 133 140 203 123 132 78 53 138 83 179 112 145 115 200 68 202 57 97 215 76 82 127 158 180 230 107 142 66 152 148 125 228 146 166 201 51 170 103 220 227 155 198 189 
  64 134 111 93 151 61 122 131 100 89 219 184 162 196 150 118 195 187 143 191 216 109 90 168 56 181 223 156 108 172 117 91 129 226 193 214 182 208 139 136 113 99 159 75 116 50 104 137 141 221 92 54 192 144 95 222 119
  197 106 77 204 157 149 217 88 225 85 165 206 55 135 102 101 69 175 60 205 58 72 224 154 86 59 213 153 229 210 128 130 110 188 186 164 74 62 218 70 65 121 246 362 379 372 318 296 253 271 270 386 336 381 288 320 268 
  321 315 326 333 265 346 283 254 324 266 378 245 344 289 244 255 337 339 335 302 383 350 371 359 373 236 342 382 330 332 279 286 312 340 306 325 252 261 285 248 295 280 323 334 374 301 250 258 348 369 231 243 351 238
  384 313 366 310 368 303 316 376 328 247 367 375 292 237 347 338 257 380 365 308 358 341 364 294 317 304 263 262 235 314 241 256 305 284 363 331 309 281 251 242 353 234 327 356 264 361 290 307 349 329 293 319 267 260 
  232 300 345 239 297 269 360 354 385 352 322 355 343 259 377 311 240 298 233 370 282 287 299 249 291 357 390 549 432 438 406 425 628 612 545 469 403 395 576 488 499 486 511 434 509 409 423 422 593 421 605 441 392 480 
  542 569 449 455 513 404 524 446 481 414 427 457 546 503 698 462 431 452 609 597 492 566 537 526 570 444 439 484 435 440 536 520 606 482 535 436 594 447 489 611 697 494 941 476 507 587 465 411 608 456 408 573 424 539 
  413 598 629 541 477 604 426 461 393 567 522 565 428 599 528 551 474 391 525 498 501 496 500 942 417 419 523 553 412 515 445 467 418 420 607 442 485 514 578 483 460 396 506 407 626 495 388 458 429 479 540 516 437 583 
  585 394 517 627 464 502 512 471 505 504 450 518 940 405 470 478 415 508 615 387 527 448 568 490 547 416 493 510 459 552 463 454 466 571 610 592 491 613 473 472 468 529 430 497 475 443 451 521 433 410 586 389 530 519
  487 584 453 820 915 764 686 591 754 677 714 722 579 725 651 602 864 655 748 633 543 660 1047 809 738 928 843 679 653 595 574 666 582 762 614 636 643 700 807 577 667 766 753 914 664 647 639 650 634 572 617 680 658 906 
  913 661 839 699 730 824 624 733 632 704 787 821 701 533 644 659 625 532 757 727 703 642 544 663 720 760 588 622 828 590 822 768 842 724 808 623 538 548 997 616 684 674 739 575 654 844 845 905 671 668 996 640 742 1072
  761 826 596 618 531 649 601 665 810 729 600 673 672 656 635 685 619 657 708 581 589 825 827 811 740 669 630 1046 749 645 995 848 759 817 728 758 863 1071 723 818 819 603 534 823 755 726 646 550 805 681 721 652 715 676
  763 998 713 788 631 662 835 770 806 830 756 621 620 834 641 799 580 648 702 638 798 765 929 716 637 858 936 922 1036 1004 965 744 861 791 919 877 939 711 780 750 1022 1015 968 717 937 837 874 736 896 767 747 981 840 
  795 900 1048 924 1037 746 1050 705 1014 793 732 860 712 925 918 831 706 857 983 946 868 938 751 771 718 735 675 991 870 786 719 745 984 772 948 783 1023 1049 907 876 741 813 775 778 853 943 1034 737 866 850 951 838 784
  933 873 1053 963 790 999 962 846 794 707 829 792 872 947 833 993 867 979 944 923 908 1000 880 1033 782 785 836 869 985 731 855 932 875 952 1012 781 916 709 967 994 917 969 832 945 856 678 935 871 710 992 1008 776 921 
  743 682 865 862 683 966 961 752 773 1011 988 854 777 1001 980 859 814 851 934 894 670 931 982 769 774 812 949 734 779 789 950 920 893 892 960 1021 841 1035 1092 852 903 1027 1160 1045 1013 1031 959 1153 1025 953 1063 
  1054 926 1090 910 1055 927 881 1069 849 1068 1079 1094 973 1020 879 1064 1060 1005 978 904 816 957 895 1026 1003 883 1032 886 889 971 1089 1018 909 1024 1061 1043 1085 1095 974 987 1044 1028 1091 1042 954 1073 1002 1109
  796 955 1019 1038 847 975 815 890 930 1062 956 1017 976 1156 1058 1087 902 897 1039 878 1157 797 884 1006 1065 901 899 964 888 1010 887 958 977 989 882 898 891 1009 1070 1086 970 972 1139 1029 986 1078 885 1041 1158 1099
  1051 1210 1161 1181 1074 1098 1084 1076 1059 1142 1100 1083 1140 1112 1162 1088 1143 1105 1209 1067 990 1077 1007 1056 1141 1082 1111 1137 1208 1016 1104 1103 1080 1081 1159 1096 1030 1066 1145 1144 1075 1110 1106 1052 
  1102 1146 1211 1040 1138 1101 1097 1108 1136 1130 1123 1119 1132 1134 1117 1113 1163 1174 1114 1120 1116 1057 1179 1126 1148 1154 1133 1135 1177 1155 1124 1127 1118 1175 1129 1093 1149 1131 1107 1176 174 1122 1125 1115 
  1128 1180 1121 1178 1147 1194 1203 1151 272 1199 1227 1186 1195 1152 1165 1173 1190 1206 1166 1168 276 1202 1192 1188 1150 1228 1172 1200 1182 1215 1167 1201 1183 1170 1184 1191 1193 1189 1226 1164 274 1169 1207 1204 1198
  1216 1205 273 1185 1171 275 400 559 1221 556 1213 1225 1220 689 1229 1224 691 398 1197 1196 1217 1219 1187 688 397 555 1212 1234 1222 399 1218 1236 1237 802 803 1214 1232 1223 804 1233 557 1235 801 1231 1230 558 1238 690
  1241 1240 800 1239 178 277 278 562 563 401 561 402 171 554 564 695 693 560 167 176 694 687 911 696 692 1242 912)
272
643
(17 24 68 73 63 38 287 130 300) 68
[20 6 2]

;;0678 JavaEditor
"Elapsed time: 185213.896626 msecs"
"Elapsed time: 658.860551 msecs"
"Elapsed time: 562308.540305 msecs"
(0 1)
(8 10 5 9 2 3 6 4 7 0 1)
2801
2791
(1) 0.5
[2 1 10]


;;2910 JavaNavigatorContentProvider.java
"Elapsed time: 358.643554 msecs"
"Elapsed time: 10.876943 msecs"
"Elapsed time: 2443.210165 msecs"
(0)
(0 1 2 3 4)
158
140
'(0) 0
[1 1 5]
;;2722 StubUtility2
"Elapsed time: 24183.684943 msecs"
"Elapsed time: 17662.659791 msecs"
"Elapsed time: 1494529.775175 msecs"
(7 29 32 49 54 63 71 78 79 99 105 108 110 118 133 137 140 143 161 164 169 172 174 184 197 205 206 210 213 214 232 238 246 247 249 258 262 263 264)
(49 246 8 212 71 209 52 185 270 10 73 169 171 173 211 79 120 288 16 268 140 203 123 78 45 283 266 245 112 289 244 9 11 200 202 277 215 180 230 66 2 279 3 148 276 146 34 166 201 51 170 227 285 248 6 258 44 231 43 122 243 
  184 162 238 118 167 247 143 168 257 33 278 181 223 172 262 226 214 284 139 136 113 281 50 242 48 137 54 144 267 77 204 4 46 225 165 206 135 205 72 224 7 213 229 110 186 273 282 164 70 287 121 275 0 37 80 147 114 18 207 
  30 38 81 163 26 53 5 145 115 19 76 82 35 142 12 28 111 29 36 13 117 40 208 174 32 31 21 75 116 15 264 176 119 269 24 1 27 175 94 63 183 42 253 271 96 98 138 254 179 57 97 107 17 228 252 25 103 280 64 93 250 61 100 41 
  178 109 90 22 56 108 91 182 99 251 234 290 141 92 95 39 85 101 60 58 20 59 210 233 62 65 84 177 161 105 160 87 14 83 68 127 158 23 152 155 134 151 89 156 129 104 232 106 157 217 88 55 102 86 153 128 130 74 218 199 67 194 
  124 126 133 132 236 125 198 189 131 150 191 237 235 193 159 192 222 197 239 149 154 188 249 190 255 220 219 196 195 187 216 263 256 221 274 69 240 259 260 241 261 47 286 265 272)
784
661
(22 3 17 5 9 8 7 1 20 6 3 2 8 15 4 3 3 18 3 5 3 2 10 13 8 1 4 3 1 18 6 8 1 2 9 4 1 1) 4.5
[8 2 114]

;;0277 MarkerResolutionTest.java
"Elapsed time: 607.634595 msecs"
"Elapsed time: 136.510723 msecs"
"Elapsed time: 52247.164044 msecs"
(5 8 9)
(8 5 9 4 0 1 3 2 6 7)
175
175
(3 1) 2
[4 2.5 5]
;;2810 SourceAnalyzer
"Elapsed time: 3714.652605 msecs"
"Elapsed time: 884.960498 msecs"
"Elapsed time: 8837439.172318 msecs"
(3 7 20 25 30 41 44 46 48 50 53 55 57)
(8 52 30 53 5 57 2 3 25 51 44 61 41 56 50 48 54 4 46 55 1 58 59 62 0 18 10 16 14 9 11 19 12 17 6 13 15 7 22 21 60 20 26 23 28 24 37 38 29 33 32 31 27 35 47 34 43 36 40 39 45 42 49)
413
424
(4 13 5 5 11 3 2 2 2 3 2 2) 3
[7 2 25]

;;2810 SourceProvider.java
"Elapsed time: 5951.742833 msecs"
"Elapsed time: 553.838008 msecs"
"Elapsed time: 3126949.857179 msecs"
(2 3 6 10 11 12 13 14 15 17 18 19 20 23 24)
(18 16 19 23 2 3 17 25 21 15 20 7 0 8 10 5 9 6 22 12 11 26 14 1 24 13 4)
507
508
(1 3 4 1 1 1 1 1 2 1 1 1 3 1) 1
[6 2 13]

;'(183 114 112 115 113 161)
;;
"Elapsed time: 9126.03624 msecs"
"Elapsed time: 4374.492121 msecs"
"Elapsed time: 5743885.510602 msecs"
(31 60 137 186 187 213)
(42 67 52 18 207 30 126 38 120 140 203 53 138 5 11 68 202 215 127 35 47 2 12 3 17 166 51 155 28 6 61 187 41 143 216 13 33 129
  40 214 136 32 31 50 137 119 204 217 88 55 69 1 60 7 213 210 128 186 62 121 0 94 80 10 73 87 79 16 26 78 14 9 57 19 180 23 
  142 25 111 93 122 167 22 56 181 21 15 141 92 95 77 24 27 58 72 20 59 130 110 218 70 84 63 37 199 71 96 169 171 124 98 211 
  123 83 97 158 125 34 170 220 44 219 36 156 99 159 48 157 85 86 49 177 173 45 179 76 66 152 201 178 174 139 206 154 153 188 
  74 65 190 132 200 103 151 131 100 191 90 117 208 75 104 192 176 135 102 101 175 212 105 133 107 198 189 196 109 168 108 172
  116 197 106 46 147 209 194 145 146 195 193 144 205 43 64 91 148 134 89 118 114 112 115 150 113 149 164 161 160 163 162 165
  183 185 184 182 4 39 54 29 82 81 8)
494
526
(29 77 49 1 26) 29
[14 3 61]
