(ns qwalkeko.clj.astnodes
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt.ast :as jdt])
  (:require [damp.qwal :as qwal])
  (:use [qwalkeko.clj.unification])
  (:use [qwalkeko.clj.reification])
  (:use [qwalkeko.clj.sessions])
  (:use [qwalkeko.clj.logic]))



;;helpers
(defn loop-parents [class instance]
  (loop [curr instance]
    (if-not (nil? curr)
      (if (instance? class curr)
        curr
        (recur (.getParent curr)))
      nil)))


(defn get-compilation-unit [ast]
  (loop-parents org.eclipse.jdt.core.dom.CompilationUnit ast))

(defn get-package [ast]
  (.getPackage
    (get-compilation-unit ast)))




(defn get-type-class [ast]
  (loop-parents org.eclipse.jdt.core.dom.TypeDeclaration ast))



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


(defn same-var [vardecl var]
  (let [ivarbinding (.resolveBinding vardecl)
        declaring-method (.getDeclaringMethod ivarbinding)
        comp-unit (get-compilation-unit vardecl)
        declaring-method-node (.findDeclaringNode comp-unit declaring-method)]
    (logic/fresh [?methoddecl]
                 (same declaring-method-node ?methoddecl)
                 (logic/project [?methoddecl]
                                  (logic/membero var
                                                 (collect-nodes ?methoddecl
                                                                org.eclipse.jdt.core.dom.SingleVariableDeclaration)))
                 (logic/== var vardecl))))  

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
  org.eclipse.jdt.core.dom.SingleVariableDeclaration
  (same [entity lvar]
    (same-var entity lvar))
  clojure.core.logic.LVar
  (same [entity lvar]
    (logic/project [entity]
                   (same entity lvar))))


;;Derivates of the previous two protocols
(defn is-removed [entity]
  "Succeeds when the entity is no longer present in the current version."
  (damp.ekeko.logic/fails
    (logic/fresh [other-entity]
           (same entity other-entity))))



(defn is-introduced [entity]
  "Succeeds when the entity was not present in any of the predecessors."
  ;;a bit harder than is-removed
  (fn [graph current next]
    (let [goals (seq (list qwal/q<= (vcurrent [curr]
                                         (logic/fresh [other-entity]
                                                      (damp.ekeko.logic/fails
                                                        (same entity other-entity))))))]
      (logic/fresh [end]
        (qwal/solve-goals graph current end goals)
        (logic/== current next)))))