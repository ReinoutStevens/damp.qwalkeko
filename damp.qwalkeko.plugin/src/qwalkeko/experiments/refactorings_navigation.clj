(ns qwalkeko.experiments.refactorings_navigation
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


(def a-model (first (filter #(instance? qwalkeko.HistoryProjectModel %) (damp.ekeko.ekekomodel/all-project-models))))
(def a-graph (graph/convert-model-to-graph a-model))
(def a-root (first (:roots a-graph)))

(def refactored-version ;;partially extracted method body
  (first
    (filter #(= "21a1b3cfb48abc9e87a97e1bdab451abe49b44eb"
               (graph/revision-number %))
         (:versions a-graph))))

(def refactored-version ;;extracted method from paper
  (first
    (filter #(= "28d39b09a766fbb0dd3ca9b65ac06edf89075e8e" 
               (graph/revision-number %)) 
      (:versions a-graph))))



(def original-version
  (first (graph/predecessors refactored-version)))

(def asts
  (l/qwalkeko 1 [?left ?right]
    (qwal/qwal a-graph refactored-version original-version 
      [?typedecl ?tname ?package ?pname]
      (l/in-source-code [c]
        (jdt/ast :CompilationUnit ?right)
        (jdt/has :package ?right ?package)
        (jdt/has :name ?package ?pname)
        (jdt/name|qualified-string ?pname "org.apache.tools.ant")
        (ast/compilationunit-typedeclaration|main ?right ?typedecl)
        (jdt/has :name ?typedecl ?tname)
        (jdt/name|simple-string ?tname "DirectoryScanner"))
      qwal/q<=
      (l/in-source-code [c]
        (ast/compilationunit-compilationunit|corresponding ?right ?left)))))

(def asts
  (logic/run 1 [?left ?right]
    (logic/fresh [?ltype ?rtype ?lname ?rname ?lpack ?rpack]
      (jdt/ast :TypeDeclaration ?ltype)
      (jdt/has :name ?ltype ?lname)
      (jdt/name|simple-string ?lname "Test")
      (jdt/ast-root ?ltype ?left)
      (jdt/has :package ?left ?lpack)
      (jdt/value-raw ?lpack nil)
      (jdt/ast :TypeDeclaration ?rtype)
      (jdt/has :name ?rtype ?rname)
      (jdt/name|simple-string ?rname "Test2")
      (jdt/ast-root ?rtype ?right)
      (jdt/has :package ?right ?rpack)
      (jdt/value-raw ?rpack nil))))
      

(def left (first (first asts)))
(def right (second (first asts)))

(def nav-graph (nav/ast-ast-navigatable-graph left right))

(def clones
    (logic/run 1 [?cloneA ?cloneB]
      (logic/fresh  [?nameA ?nameB]
        (jdt/child+ left ?cloneA)
        (jdt/ast :MethodDeclaration ?cloneA)
        (jdt/has :name ?cloneA ?nameA)
        (jdt/name|simple-string ?nameA "setIncludes")
         (jdt/child+ left ?cloneB)
        (jdt/ast :MethodDeclaration ?cloneB)
        (jdt/has :name ?cloneB ?nameB)
        (jdt/name|simple-string ?nameB "setExcludes"))))

(def cloneA (first (first clones)))
(def cloneB (second (first clones)))

(defn small-query [graph cloneA cloneB]
  (logic/run 5 [?notPresent ?introMethod ?introName ?end]
    (nav/step-changes graph ?end []
      nav/change->*
      (nav/in-current-change-state [curr ast]
        (logic/== ?notPresent ast))
      nav/change->
      (nav/with-last-change [current ast change] ;var introducere of niet?
        (jdt/child+ ast ?introMethod)
        (jdt/ast :MethodDeclaration ?introMethod)
        (jdt/has :name ?introMethod ?introName)
        (el/fails
          (logic/fresh [?otherMethod ?otherName]
            (jdt/child+ ?notPresent ?otherMethod)
            (jdt/ast :MethodDeclaration ?otherMethod)
            (jdt/has :name ?otherMethod ?otherName)
            (jdt/name|simple-name|simple|same ?otherName ?introName)))))))


(defn change-query [navgraph cloneA cloneB]
  (logic/run 1 [?notPresent ?introMethod ?introName ?methodChange ?moveChange ?end ?cmethod ?original ?copy]
    (nav/step-changes navgraph ?end []
      nav/change->*
      (nav/in-current-change-state [curr ast]
        (logic/== ?notPresent ast))
      nav/change->
      (nav/with-last-change [current ast change] ;var introducere of niet?
        (jdt/child+ ast ?introMethod)
        (jdt/ast :MethodDeclaration ?introMethod)
        (jdt/has :name ?introMethod ?introName)
        (el/fails
          (logic/fresh [?otherMethod ?otherName]
            (jdt/child+ ?notPresent ?otherMethod)
            (jdt/ast :MethodDeclaration ?otherMethod)
            (jdt/has :name ?otherMethod ?otherName)
            (jdt/name|simple-name|simple|same ?otherName ?introName))))
      nav/change->*
      (nav/with-last-change [current ast ?move]
        (changes/change|move ?move)
        (changes/change-original ?move ?original)
        (jdt/child+ ?original cloneA)
        (changes/change-copy ?move ?copy)
        (nav/graph-node-node-ast-corresponding current ?introMethod ?cmethod ast)
        (jdt/has :body ?cmethod ?copy))))) 

(logic/run 1 [?change]
  (nav/step-changes change-graph ?end-ast []
    nav/change->*
    (nav/in-current-change-state [curr ast]
      (logic/== ?notPresent ast))
    (nav/with-last-change [curr ast change]
      (logic/== change ?change)
      (jdt/child+ ast ?introMethod)
      (jdt/ast :MethodDeclaration ?introMethod)
      (jdt/has :name ?introMethod ?introName)
      (el/fails
        (jdt/child+ ?notPresent ?method)
        (jdt/ast :MethodDeclaration ?method)
        (jdt/has :name ?method ?methodName)
        (jdt/name|simple-name|simple|same ?methodName ?introName)))))



;;Testing the change graph
(defn latest-revision [vgraph]
  (let [sorted (sort-by graph/date (:versions vgraph))]
    (second (reverse  sorted))))

(defn retrieve-cu-pairs [vgraph version]
  (let [preds (graph/predecessors version)
        edits (filter #(= (:status %) :edit) (graph/file-infos version))]
    (mapcat
      (fn [file]
        (l/qwalkeko* [?source ?target]
          (logic/fresh [?pred]
            (qwal/qwal vgraph version ?pred []
              (l/in-source-code [curr]
                (l/fileinfo|java file version)
                (l/fileinfo|compilationunit file ?target curr))
              qwal/q<=
              (l/in-source-code [curr]
                (logic/onceo (ast/compilationunit-compilationunit|corresponding ?target ?source)))))))
      edits)))

(def les-cus (retrieve-cu-pairs a-graph refactored-version))

(defn do-magic [vgraph]
  (let [latest (latest-revision vgraph)
        cus (retrieve-cu-pairs vgraph latest)
        graphs (remove #(= (count (:changes %)) 0) (map #(apply nav/ast-ast-navigatable-graph %) cus))]
    (do
      (map
        (fn [nav]
          (let [lines (count (filter #(= % \newline ) (.toString (:left nav))))
                name (ast/has-clj-unwrapped :identifier (ast/has-clj-unwrapped :name (first (ast/has-clj-unwrapped :types (:left nav)))))
                sep " & "]
            (println (.getName (:project vgraph)) sep name sep lines sep (count (:changes nav)) sep (qwalkeko.clj.graph-algo/longest-path nav))))
        graphs))))


(def failed-graph (atom nil))
(defn test-change-graph [left right]
  (defn loop-changes [graph]
    (let [next-changes (nav/graph-next-changes graph)]
      (map
        (fn [change]
          (let [applied 
                (try
                  (nav/change-apply graph change)
                  (catch NullPointerException e (do (reset! failed-graph graph) (throw e))))]
           (loop-changes applied)))
        (map #(changes/graph-change-idx graph %) next-changes))))
  (let [nav-graph (nav/ast-ast-navigatable-graph left right)]
    (loop-changes nav-graph)))



;;Change Pattern
(defn method|introduced [?method pre post]
  (logic/fresh [?mname ?present ?pname ?mname]
    (ast/child+-type post :MethodDeclaration ?method)
    (jdt/has :name ?method ?mname)
    (el/fails
      (logic/all
        (ast/child+-type pre :MethodDeclaration ?present)
        (jdt/has :name ?present ?pname)
        (jdt/name|simple-name|simple|same ?pname ?mname)))))

(defn methodinvoc-method|invokes [?invoc ?method] ;;could add fancy signature checking
  (logic/fresh [?iname ?mname ?ival ?mval ?iargs ?mpara]
    (jdt/ast :MethodInvocation ?invoc)
    (jdt/ast :MethodDeclaration ?method)
    (jdt/has :name ?invoc ?iname)
    (jdt/has :name ?method ?mname)
    (jdt/name|simple-name|simple|same ?iname ?mname)
    (jdt/has :arguments ?invoc ?iargs)
    (jdt/value-raw ?iargs ?ival)
    (jdt/has :parameters ?method ?mpara)
    (jdt/value-raw ?mpara ?mval)
    (logic/project [?mval ?ival]
      (logic/== (count (seq ?mval)) (count (seq ?ival))))))

(defn method-string|named [?method ?name]
  (logic/fresh [?mname]
    (jdt/ast :MethodDeclaration ?method)
    (jdt/has :name ?method ?mname)
    (jdt/name|simple-string ?mname ?name)))

(defn refactoring-query [nav-graph nameA nameB extracted]
  (logic/run 1 [?end]
    (nav/step-changes nav-graph ?end [?intro ?introCurr ?aInvoc ?bInvoc ?aCurr ?bCurr ?cloneA ?cloneB]
      (nav/in-current-change-state [curr ast]
        (ast/child+-iter ast ?cloneA)
        (ast/child+-iter ast ?cloneB)
        (method-string|named ?cloneA nameA)
        (method-string|named ?cloneB nameB))
      nav/change->*
      (nav/with-last-change [curr ast change]
        (jdt/ast :MethodDeclaration ?extracted)
        (ast/ast-ast|same ?extracted extracted)
        (ast/child+-type ast :MethodInvocation ?aInvoc)
        (ast/child+-type ast :MethodInvocation ?bInvoc)
        (methodinvoc-method|invokes ?aInvoc ?extracted)
        (methodinvoc-method|invokes ?bInvoc ?extracted)
        (nav/graph-node-node|tracked curr ?cloneA ?aCurr)
        (nav/graph-node-node|tracked curr ?cloneB ?bCurr)
        (jdt/child+ ?aCurr ?aInvoc)
        (jdt/child+ ?bCurr ?bInvoc)))))


;;Redundant changes
(defn redundant-changes [nav-graph]
  (logic/run 1 [?nodeA ?nodeB]
    (nav/step-changes nav-graph ?nodeB [?astA]
      nav/change->*
      (nav/in-current-change-state [curr ast]
        (logic/== ast ?astA)
        (logic/== ?nodeA curr))
      nav/change->+
      (nav/in-current-change-state [curr ast]
        (ast/ast-ast|same ast ?astA)))))



;;Introduced method
(defn ast-method|not-present [ast method]
  (logic/fresh [?name ?m ?n]
    (jdt/has :name method ?name)
    (el/fails
      (logic/all
        (ast/child+-type ast :MethodDeclaration ?m)
        (jdt/has :name ?m ?n)
        (jdt/name|simple-name|simple|same ?name ?n)))))

(defn introduced-method [nav-graph]
  (logic/run 1 [?method ?node]
    (nav/step-changes nav-graph ?node [?not-present ?name]
      (nav/in-current-change-state [curr ast]
        (logic/== ?not-present ast))
      nav/change->+
      (nav/in-current-change-state [curr ast]
        (ast/child+-type ast :MethodDeclaration ?method)
        (jdt/has :name ?method ?name)
        (ast-method|not-present ?not-present ?method)))))

;;Rename method refactoring
(defn rename-method [nav-graph]
  (logic/run 1 [?method ?invoke]
    (nav/step-changes nav-graph ?end-node []
      (nav/in-current-change-state [curr ast]
        (ast/child+-type ast :MethodDeclaration ?method)
        (jdt/has :name ?method ?name)
        (ast/child+-type ast :MethodInvocation ?invoke)
        (method-invocation|invokes ?method ?invoke))
      nav/change->+
      (nav/in-current-change-state [curr ast]
        (logic/fresh [?current-method ?current-name]
          (nav/graph-node-node|tracked curr ?method ?current-method)
          (jdt/has :name ?current ?current-name)
          (el/fails
            (jdt/name|simple-name|simple|same ?name ?current-name))))
      nav/change->+
      (nav/in-current-change-state [curr ast]
        (logic/fresh [?current-method ?current-invoke]
          (nav/graph-node-node|tracked curr ?invoke ?current-invoke)
          (nav/graph-node-node|tracked curr ?original-method ?current-method)
          (method-invocation|invokes ?current-method ?current-invoke))))))
     

;;Renamed field
(defn renamed-field [changes]
  (logic/run 1 [?sequence]
    (logic/conde
      [(logic/fresh [?update ?orig-val ?parent ?new-val]
         (logic/membero ?update changes)
         (changes/change|update ?update)
         (logic/== ?sequence (list ?update))
         (changes/change-original ?update ?orig-val)
         (jdt/ast :SimpleName ?orig-val)
         (jdt/ast-parent ?orig-val ?parent)
         (jdt/ast :VariableDeclarationFragment ?parent)
         (changes/change-copy ?update ?new-val)
         (jdt/ast :SimpleName ?new-val)
         (el/fails
           (jdt/name|simple-name|simple|same ?orig-val ?new-val)))]
      [(logic/fresh [?insert ?delete ?inserted ?deleted ?i-name ?d-name]
         (logic/membero ?insert changes)
         (logic/membero ?delete changes)
         (logic/== ?sequence (list ?insert ?delete))
         (changes/change|insert ?insert)
         (changes/change|delete ?delete)
         (changes/change-copy ?insert ?inserted)
         (jdt/ast :VariableDeclarationFragment ?inserted)
         (changes/change-original ?delete ?deleted)
         (jdt/ast :VariableDeclarationFragment ?deleted)
         (jdt/has :name ?inserted ?i-name)
         (jdt/has :name ?deleted ?d-name)
         (el/fails
           (jdt/name|simple-name|simple|same ?i-name ?d-name)))])))


(defn renamed-field [nav-graph]
  (logic/run 1 [?end-node]
    (nav/step-changes nav-graph ?end-node []
      (nav/in-current-change-state [curr ast]
        (logic/== ?orig-ast ast)
        (ast/child+-type ast :VariableDeclarationFragment ?field)
        (jdt/has :name ?orig-name))
      nav/change->+
      (nav/in-current-change-state [curr ast]
        (ast/child+ ast :VariableDeclarationFragment ?other)
        (jdt/has :name ?other-name)
        (fails
          (jdt/name|simple-name|simple|same ?orig-name ?other-name))
        (fails
          (ast/child+-type ast :VariableDeclarationFragment ?field)
          (jdt/has :name ?field-name)
          (jdt/name|simple-name|simple|same ?other-name ?field-name))))))


;;Feature Extraction

      

