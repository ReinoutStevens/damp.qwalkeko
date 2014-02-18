(ns qwalkeko.clj.ast
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt
             [ast :as jdt]]))

;;reification of Java functions
(defn compilation-unit? [astnode]
  (= (.getNodeType astnode) org.eclipse.jdt.core.dom.ASTNode/COMPILATION_UNIT))

(defn get-ast-path [ast]
  (let [root (.getRoot ast)]
    (when (compilation-unit? root)
      (let [javaelement (.getJavaElement root)] ;;should be a org.eclipse.jdt.internal.core.CompilationUnit
        (.removeFirstSegments (.getPath javaelement) 1)))))


(defn string-to-path [string]
  (new org.eclipse.core.runtime.Path string))

(defn path-to-string [path]
  (.toString path))

;;logicify them
(defn path-string [?path ?string]
  (logic/all
    (logic/conde
      [(logic/nonlvaro ?path)
       (logic/project [?path]
                      (logic/== ?string (path-to-string ?path)))]
      [(logic/nonlvaro ?string)
       (logic/project [?string]
                      (logic/== ?path (string-to-path ?string)))])))

(defn ast-path [?ast ?path]
  (logic/all
    (jdt/ast :ASTNode ?ast)
    (logic/project [?ast]
                   (logic/== ?path (get-ast-path ?ast)))))



(defn ast-compilationunit|corresponding [ast ?compunit]
  "finds the corresponding compilationunit in this version of the ast"
  (logic/fresh [?root ?path]
               (logic/project [ast]
                              (ast-path ast ?path)
                              (jdt/ast :CompilationUnit ?compunit)
                              (ast-path ?compunit ?path))))