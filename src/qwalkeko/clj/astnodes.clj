(ns qwalkeko.clj.astnodes
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt.reification :as jdt])
  (:require [damp.qwal :as qwal])
  (:use [qwalkeko.clj.unification])
  (:use [qwalkeko.clj.reification])
  (:use [qwalkeko.clj.sessions])
  (:use [qwalkeko.clj.logic]))



(defn collect-nodes [ast & classes]
  "Collects all the nodes in the ast tree that are an instance of the given types"
  (let [collector (new qwalkeko.ASTCollector)]
    (doall
      (map
        (fn [x]
          (.addClass collector x))
        classes))
    (.accept ast collector)
    (seq (.getCollected collector))))

(defn in-anonymous-class [ast]
  "Returns whether the ast is inside an anonymous class"
  (not (nil?
         (loop-parents org.eclipse.jdt.core.dom.AnonymousClassDeclaration ast))))
    

;; Guessing whether a node changed or not
;; Currently we just see whether the defining file changed
(defn defining-path [ast]
  "Returns the path to the file in which the ast node is defined"
  (let [comp-unit (loop-parents org.eclipse.jdt.core.dom.CompilationUnit ast)
        java-element (.getJavaElement comp-unit)
        path (.getPath java-element)]
    path))


(defn defining-project [ast]
  "Returns the java project in which the ast node is defined"
  (let [comp-unit (loop-parents org.eclipse.jdt.core.dom.CompilationUnit ast)
        java-element (.getJavaElement comp-unit)
        java-project (.getJavaProject java-element)]
    java-project))


(defn could-have-changed? [node]
  "Returns whether the node may have changed."
  (let [defining-p (defining-path node)
        current (current-version)]
    (file-changed? defining-p current)))  


;; Protocols



(defprotocol ISameEntity
  "Finds the same entity in the current version"
  (same [entity lvar] ))

(defprotocol IChanged
  "Was this entity changed in the current version"
  (changed? [entity] ))



(defn same-package [packdecl var]
  (logic/all
    (jdt/ast :PackageDeclaration var)
    (logic/== packdecl var)))


(defn same-type [typedecl var]
  (let [package (get-package typedecl)]
    (logic/fresh [?vpackage]
      (same package ?vpackage)
      (logic/project [?vpackage]
               (logic/membero var
                        (collect-nodes (get-compilation-unit ?vpackage)
                                       org.eclipse.jdt.core.dom.TypeDeclaration))
               (logic/== var typedecl)))))


(defn same-method [methdecl var]
  (let [typedecl (get-type-class methdecl)
        package (get-package methdecl)]
    (if-not (in-anonymous-class methdecl)
      (logic/fresh [?vtypedecl]
                   (same typedecl ?vtypedecl)
                   (logic/project [?vtypedecl]
                                  (logic/membero var
                                                 (collect-nodes ?vtypedecl
                                                                org.eclipse.jdt.core.dom.MethodDeclaration)))
                   (logic/== var methdecl))
      logic/fail)))
  

(extend-protocol ISameEntity
  org.eclipse.jdt.core.dom.PackageDeclaration
  (same [entity lvar]
                 (same-package entity lvar))
  org.eclipse.jdt.core.dom.TypeDeclaration
  (same [entity lvar]
                 (same-type entity lvar))
  org.eclipse.jdt.core.dom.MethodDeclaration
  (same [entity lvar]
                 (same-method entity lvar))
  clojure.core.logic.LVar
  (same [entity lvar]
        (logic/project [entity]
                       (same entity lvar))))


;;Derivates of the previous two protocols
(defn is-removed [entity]
  (damp.ekeko.logic/fails
    (logic/fresh [other-entity]
           (same entity other-entity))))