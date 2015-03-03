(ns qwalkeko.experiments.refactorings-japan
  (:require [clojure.core.logic :as logic])
  (:require [qwalkeko.clj.logic :as l])
  (:require [qwalkeko.clj.reification :as r])
  (:require [qwalkeko.clj.graph :as graph])
  (:require [qwalkeko.clj.ast :as ast])
  (:require [qwalkeko.clj.functionalnodes :as change])
  (:require [damp.ekeko.jdt
             [ast :as jdt]
             [convenience :as conv]])
  (:require [damp.ekeko.logic :as el])
  (:require [damp.qwal :as qwal]))


;;https://www.dropbox.com/home/CloneRefactoring


(def a-model (first (damp.ekeko.ekekomodel/all-project-models)))
(def a-graph (graph/convert-model-to-graph a-model))
(def a-root (first (:roots a-graph)))

(comment
  (filter #(let [infos (graph/file-infos %)]
                (> (count (filter (fn [f]
                                    (re-find #"DirectoryScanner" (:file f)))
                            infos))
                  0))
       (:versions a-graph))

  (filter
       #(let [r (l/qwalkeko* [?v]
                  (qwal/qwal a-graph % ?v [?type ?method ?name ?mname]
                    (l/in-source-code [c]
                      (jdt/ast :TypeDeclaration ?type)
                      (jdt/has :name ?type ?name)
                      (jdt/name|simple-string ?name "DirectoryScanner")
                      (jdt/ast :MethodDeclaration ?method)
                      (jdt/has :name ?method ?mname)
                      (jdt/name|simple-string ?mname "normalizePattern")
                      (jdt/ast-typedeclaration|encompassing ?method ?type))))]
          (graph/ensure-delete %)
          (> (count r) 0))
       les-versions))

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

(def left-ast (first (first asts)))
(def right-ast (second (first asts)))

(def changes (qwalkeko.clj.changenodes/get-ast-changes left-ast right-ast))
(def normalized-changes (qwalkeko.clj.changenodes/normalize-changes changes))

(defn change-affects-method-declaration [change ?method]
  (logic/all
    (change/change-affects-node change ?method)
    (jdt/ast :MethodDeclaration ?method)))

;;non-relational
(defn changes-affect-same-node [changes ?new-changes node]
  (logic/conda
    [(logic/emptyo changes)
     (logic/== ?new-changes '())]
    [(logic/fresh [?head ?tail ?result]
       (logic/conso ?head ?tail changes)
       (logic/conda
         [(change/change-affects-node ?head node)
          (logic/conso ?head ?result ?new-changes)
          (changes-affect-same-node ?tail ?result node)]
         [(changes-affect-same-node ?tail ?new-changes node)]))]))


(defn change-method-inserted [change ?method]
  (logic/all
    (change/change|insert change)
    (change/insert-newnode change ?method)
    (jdt/ast :MethodDeclaration ?method)))


(defn change|body-updated [change]
  (logic/fresh [?method]
    (change/change|update change)
    (change/change-original change ?method)
    (change/update-property change :body)))

(defn ast-ast|similar [?left ?right]
  (logic/all
    (ast/ast-ast|usim-similar ?left ?right)))

(defn changes-extract-method-deleting-inserting [changes ?extracted ?deleting ?inserting]
  (logic/fresh 
    [?methodname ?methodinvoc ?methodcallname ?insert-change ?insert-into ?deleted-node ?extracted-body]
    (e/contains changes ?insert-change)
    (change-method-inserted ?insert-change ?extracted)
    (jdt/has :name ?extracted ?methodname)
    (el/contains changes ?inserting)
    (change/change|insert ?inserting)
    (change/change-affects-original-node ?inserting ?insert-into)
    (jdt/ast :MethodDeclaration ?insert-into)
    (change/change-contains-new-node ?inserting ?methodinvoc)
    (jdt/ast :MethodInvocation ?methodinvoc)
    (jdt/has :name ?methodinvoc ?methodcallname)
    (jdt/name|simple-name|simple|same ?methodcallname ?methodname)
    (el/contains changes ?deleting)
    (change/change|delete ?deleting)
    (change/change-affects-original-node ?deleting ?insert-into)
    ;;verify similarity
    (jdt/has :body ?extracted ?extracted-body)
    (change/change-original ?deleting ?deleted-node)
    (ast-ast|similar  ?deleted-node ?extracted-body)))


(defn changes-extract-method-updating [changes ?extracted ?updating]
  (logic/fresh
    [?methodname ?methodinvoc ?methodcallname ?updated-method ?insert-change ?updated-node ?extracted-body]
    (el/contains changes ?insert-change)
    (change-method-inserted ?insert-change ?extracted)
    (jdt/has :name ?extracted ?methodname)
    (el/contains changes ?updating)
    (change/change|update ?updating)
    (change/change-affects-original-node ?updating ?updated-method)
    (jdt/ast :MethodDeclaration ?updated-method)
    (change/change-contains-new-node ?updating ?methodinvoc)
    (jdt/ast :MethodInvocation ?methodinvoc)
    (jdt/has :name ?methodinvoc ?methodcallname)
    (jdt/name|simple-name|simple|same ?methodcallname ?methodname)
    (jdt/has :body ?extracted ?extracted-body)
    (change/change-original ?updating ?updated-node)
    (ast-ast|similar ?updated-node ?extracted-body)))


(defn changes-extract-method-moving-inserting [changes ?extracted ?moving ?inserting]
  (logic/fresh 
    [?methodname ?methodinvoc ?methodcallname ?insert-change ?insert-into ?extracted-body ?move-right]
    (el/contains changes ?insert-change)
    (change-method-inserted ?insert-change ?extracted)
    (jdt/has :name ?extracted ?methodname)
    (el/contains changes ?inserting)
    (change/change|insert ?inserting)
    (change/change-affects-original-node ?inserting ?insert-into)
    (jdt/ast :MethodDeclaration ?insert-into)
    (change/change-contains-new-node ?inserting ?methodinvoc)
    (jdt/ast :MethodInvocation ?methodinvoc)
    (jdt/has :name ?methodinvoc ?methodcallname)
    (jdt/name|simple-name|simple|same ?methodcallname ?methodname)
    (el/contains changes ?moving)
    (change/change|move ?moving)
    (change/change-affects-original-node ?moving ?insert-into)
    (jdt/has :body ?extracted ?extracted-body)
    (change/move-rightnode ?moving ?move-right)
    (jdt/ast-parent ?move-right ?extracted-body)))


(defn meh [changes ?renamed ?inserted]
(logic/fresh
  []
  (change/change-ast|inserted changes ?insertchange  ?inserted)
  (jdt/ast :MethodDeclaration ?inserted)
  (change/ast|renamed changes ?renamechange ?renamed)
  (
    
  (el/contains changes ?insert-name-change)
  (change/change|insert ?insert-name-change)
  (change/insert-newnode ?insert-name-change ?insert-name)
  (jdt/ast :SimpleName ?insert-name)
  (jdt/ast-parent ?insert-name ?method)
  (jdt/ast :MethodDeclaration ?method)
  (jdt/has :name ?method ?insert-name)
  (el/contains changes ?insert-change)
    
;;detecting extracted methods from clones

(defn changes-extract-method-from-clones [changes ?extracted ?deletingA ?insertingA ?movingB ?insertingB]
  (logic/all
    (changes-extract-method-deleting-inserting changes ?extracted ?deletingA ?insertingA)
    (changes-extract-method-moving-inserting changes ?extracted ?movingB ?insertingB)
    (logic/!= ?insertingA ?insertingB)))


(defn changes-extract-method-from-clones [changes ?extracted ?deletingA ?insertingA ?deletingB ?insertingB]
  (logic/all
    (changes-extract-method-deleting-inserting changes ?extracted ?deletingA ?insertingA)
    (changes-extract-method-deleting-inserting changes ?extracted ?deletingB ?insertingB)
    (logic/!= ?deletingA ?deletingB)
    (logic/!= ?insertingA ?insertingB)))


;;comparing differences between both clones that were refactored
(defn distill-changes-from-clones [graph changes start]
  (l/qwalkeko* [?changesA ?changesB]
    (logic/fresh [?extracted ?deletingA ?insertingA ?deletingB ?insertingB ?leftroot ?rightroot 
                  ?end ?methodA ?methodB ?rightMethodA ?rightMethodB]
      (changes-extract-method-from-clones changes ?extracted ?deletingA ?insertingA ?deletingB ?insertingB)
      (qwal/qwal graph start ?end []
        (l/in-source-code [curr]
          (jdt/ast-root ?deletingA ?leftroot)
          (jdt/ast-parent+ ?deletingA ?methodA)
          (jdt/ast :MethodDeclaration ?methodA)
          (jdt/ast-parent+ ?deletingB ?methodB)
          (jdt/ast :MethodDeclaration ?methodB)
          (logic/!= ?methodB ?methodA))
        qwal/q=>
        (l/in-source-code [curr]
          (ast/compilationunit|corresponding ?leftroot ?rightroot)
          (ast/methoddeclaration|corresponding ?methodA ?rightMethodA)
          (ast/methoddeclaration|corresponding ?methodB ?rightMethodB)
          (change/changes ?changesA ?methodA ?rightMethodA)
          (change/changes ?changesB ?methodB ?rightMethodB))))))



;;extract method
;;given: a clone instance (or partially cloned) from which cloned part is extracted into extracted method
(defn compute-changes [graph start classname methodname]
  (l/qwalkeko* [?end ?changes]
    (qwal/qwal graph start ?end [?left-class ?left-method ?right-class ?right-method]
      (l/in-source-code [curr]
        (class-method|named|named ?left-class ?left-method classname methodname))
      qwal/q=>
      (l/in-source-code [curr]
        (class-method|named|named ?right-class ?right-method classname methodname)
        (ast/method-method|same-signature ?left-method ?right-method)
        (change/changes ?changes ?left-method ?right-method)))))



;;Crappy Queries for the Paper
(defn class|named [?class ?cstrname]
  (logic/fresh [?cname]
    (jdt/ast :TypeDeclaration ?class)
    (jdt/has :name ?class ?cname)
    (jdt/name|simple-string ?cname ?cstrname)))

(defn method|named [?method ?mstrname]
  (logic/fresh [?mname]
    (jdt/ast :MethodDeclaration ?method)
    (jdt/has :name ?method ?mname)
    (jdt/name|simple-string ?mname ?mstrname)))


(defn class-method|named|named [?class ?method ?cstrname ?mstrname]
  (logic/all
    (class|named ?class ?cstrname)
    (ast/child+-iter ?class ?method)
    (jdt/ast :MethodDeclaration ?method)
    (method|named ?method ?mstrname)))


(defn detect-refactoring [graph root]
  (l/qwalkeko* [?left ?right ?lincludes ?lexcludes ?rincludes ?rexcludes ?rnormalize]
    (logic/fresh [?next]
      (qwal/qwal graph root ?next [?lclass ?rclass]
        (l/in-source-code [curr]
          (class-method|named|named ?lclass ?lincludes "DirectoryScanner" "setIncludes")
          (class-method|named|named ?lclass ?lexcludes "DirectoryScanner" "setExcludes")
          (jdt/ast-root ?lclass ?left))
        qwal/q=>
        (l/in-source-code [curr]
          (jdt/ast :CompilationUnit ?right)
          (class-method|named|named ?rclass ?rincludes "DirectoryScanner" "setIncludes")
          (class-method|named|named ?rclass ?rexcludes "DirectoryScanner" "setExcludes")
          (class-method|named|named ?rclass ?rnormalize "DirectoryScanner" "setExcludes")
          (jdt/ast-root ?lclass ?right))))))


(defn change-pattern [changes lincludes lexcludes rincludes rexcludes]
  (logic/run* [?insertMethod ?insertIncCall ?insertExcCall ?deleteInc ?deleteExc ?moveBody]
    (logic/fresh [?insertMDecl ?cNormalize ?normalizeBody ?insertMDeclName
                  ?moveBody ?deleteIncludesClone ]
                  
      (el/contains changes ?insertMethod)
      (change/change|insert ?insertMDecl)
      (change/change-copy ?insertMDecl ?cNormalize)
      (jdt/ast :MethodDeclaration ?cNormalize)
      (jdt/has :body ?cNormalize ?normalizeBody)
      (jdt/has :name ?insertMDecl ?insertMDeclName)
      (jdt/name|simple-string ?rincludeCallName ?insertMDeclName)
      
      (el/contains changes ?moveBody)
      (change/change|move ?moveBody)
      (change/change-copy ?moveBody ?normalizeBody)
      
      (el/contains changes ?deleteIncludesClone)
      (change/change|delete ?deleteIncludesClone)
      (change/change|original ?deleteIncludesClone ?deleteIncludes)
      (change/change|delete ?deleteExcludesClone)
      (change/change-original ?deleteIncludesClone ?deleteExcludes)
      (logic/!= ?deleteIncludes ?deleteExcludes)
      (jdt/ast-parent+ ?deleteIncludes lincludes)
      (jdt/ast-parent+ ?deleteExcludes lexcludes)
      (ast/ast-ast|usim-similar ?deleteIncludes ?deleteExcludes)
      (jdt/has :body rnormalize ?rnormalizeBody) 
      (ast/ast-ast|usim-similar ?deleteIncludes ?rnormalizeBody)
      
      (el/contains changes ?rincludesInsert)
      (change/insert-newnode ?rincludesInsert ?rincludeCall)
      (jdt/ast :MethodInvocation ?rincludeCall)
      (jdt/has :name ?rincludeCall ?rincludeCallName)
      (jdt/name|simple-string ?rincludeCallName ?methodName)
      (jdt/has :name ?insertMDecl ?insertMDeclName)
      (jdt/name|simple-string ?rincludeCallName ?insertMDeclName)
      (el/contains changes ?rexcludesInsert)
      (jdt/ast :MethodInvocation ?rexcludeCall)
      (jdt/has :name ?rexcludeCall ?rexcludeCallName)
      (jdt/name|simple-string ?rexcludeCallName ?insertMDeclName)))))))




(run* [?changes ?methodA ?methodB ?extracted]
  (logic/fresh [?classA ?classB]
    (class-method|named|named ?classA ?methodA "DirectoryScanner" "setExcludes")
    (class-method|named|named ?classA ?methodA "DirectoryScanner" "setIncludes")
    
      