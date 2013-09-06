(ns qwalkeko.changedistiller.treedistilling
  (:import (ch.uzh.ifi.seal.changedistiller 
           JavaChangeDistillerModule
           ast.ASTHelperFactory
           treedifferencing.TreeDifferencer))
  (:import (com.google.inject.Guice)))



(defn create-guice-injector [] 
  (let [modules (list (new JavaChangeDistillerModule))]
    (com.google.inject.Guice/createInjector modules)))


(defn ast-helper-factory-instance [injector]
  (.getInstance injector ASTHelperFactory))


(defn create-java-ast-helper [injector a-file]
  (let [factory (ast-helper-factory-instance injector)]
    (.create factory a-file)))



(defn create-structure-tree [ast-helper]
  (.createStructureTree ast-helper))


(defn create-declaration-tree 
  ([ast-helper]
    (create-declaration-tree
      ast-helper
      (create-structure-tree ast-helper)))
  ([ast-helper structure-node]
    (.createDeclarationTree ast-helper (first (.getChildren structure-node)))))




(defn distil-tree-operations [left-file right-file]
  (let [injector (create-guice-injector)
        left-helper (create-java-ast-helper injector left-file)
        right-helper (create-java-ast-helper injector right-file)
        left-tree  (create-declaration-tree left-helper)
        right-tree (create-declaration-tree right-helper)
        tree-diff (new TreeDifferencer)]
    (.calculateEditScript tree-diff left-tree right-tree)
    (.getEditScript tree-diff))) 