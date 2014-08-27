(ns qwalkeko.experiments.refactorings-japan
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
        (ast/compilationunit|corresponding ?right ?left)))))

(def left-ast (first (first asts)))
(def right-ast (second (first asts)))

(def changes (change/get-ast-changes left-ast right-ast))

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
  (logic/fresh [?levenshtein]
    (ast/ast-ast|levenshtein-normalized ?left ?right ?levenshtein)
    (logic/project [?levenshtein]
      (logic/== true (> 0.6 ?levenshtein))))) ;;this value is chosen atm chosen at random

(defn changes-extract-method-deleting-inserting [changes ?extracted ?deleting ?inserting ]
  (logic/fresh 
    [?methodname ?methodinvoc ?methodcallname ?insert-change ?insert-into ?deleted-node ?extracted-body]
    (damp.ekeko.logic/contains changes ?insert-change)
    (change-method-inserted ?insert-change ?extracted)
    (jdt/has :name ?extracted ?methodname)
    (damp.ekeko.logic/contains changes ?inserting)
    (change/change|insert ?inserting)
    (change/change-affects-original-node ?inserting ?insert-into)
    (jdt/ast :MethodDeclaration ?insert-into)
    (change/change-contains-new-node ?inserting ?methodinvoc)
    (jdt/ast :MethodInvocation ?methodinvoc)
    (jdt/has :name ?methodinvoc ?methodcallname)
    (jdt/name|simple-name|simple|same ?methodcallname ?methodname)
    (damp.ekeko.logic/contains changes ?deleting)
    (change/change|delete ?deleting)
    (change/change-affects-original-node ?deleting ?insert-into)
    ;;verify similarity
    (jdt/has :body ?extracted ?extracted-body)
    (change/change-original ?deleting ?deleted-node)
    (ast-ast|similar  ?deleted-node ?extracted-body)))


(defn changes-extract-method-updating [changes ?extracted ?updating]
  (logic/fresh
    [?methodname ?methodinvoc ?methodcallname ?updated-method ?insert-change ?updated-node ?extracted-body]
    (damp.ekeko.logic/contains changes ?insert-change)
    (change-method-inserted ?insert-change ?extracted)
    (jdt/has :name ?extracted ?methodname)
    (damp.ekeko.logic/contains changes ?updating)
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