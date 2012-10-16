(ns scrapperplugin.clj.protocols
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt.reification :as jdt])
  (:require [damp.qwal :as qwal])
  (:use [scrapperplugin.clj.unification])
  (:use [scrapperplugin.clj.logic]))



(defn collect-nodes [ast & classes]
  (let [collector (new scrapperplugin.ASTCollector)]
    (doall
      (map
        (fn [x]
          (.addClass collector x))
        classes))
    (.accept ast collector)
    (seq (.getCollected collector))))
    
    


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
    (logic/fresh [?vtypedecl]
           (same typedecl ?vtypedecl)
           (logic/project [?vtypedecl]
                          (logic/membero var
                                         (collect-nodes ?vtypedecl
                                                        org.eclipse.jdt.core.dom.MethodDeclaration)))
           (logic/== var methdecl))))
  

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

(defn introduced? [entity]
  (fn [graph current next]
    (logic/fresh [?preventity ?kind]
                 (qwal/solve-all-goals graph current next
                                  (scurrent [curr]
                                            (jdt/ast ?kind entity))
                                  qwal/q<=
                                  (scurrent [curr]
                                            (damp.ekeko.logic/fails
                                              (same entity ?preventity))))
                 (logic/== current next))))