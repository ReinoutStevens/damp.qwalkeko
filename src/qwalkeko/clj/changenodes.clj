(ns qwalkeko.clj.changenodes
  (:import [changenodes.Differencer])
  (:import [changenodes.operations
            Delete Insert Move Update])
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt
             [reification :as jdt]]))




;;Changes
(derive ::delete ::operation)
(derive ::move ::operation)
(derive ::update ::operation)
(derive ::insert ::operation)

(defn make-delete [astnode]
  {:operation :delete :original astnode})

(defn make-move [astnode new-parent property index]
  {:operation :move 
   :original astnode 
   :property property 
   :index index 
   :new-parent new-parent})

(defn make-update [left-parent right-parent property]
  {:operation :update 
   :original left-parent 
   :right-parent right-parent
   :property property})

(defn make-insert [left-parent right-parent right-node property index]
  {:operation :insert 
   :original left-parent 
   :right-parent right-parent 
   :property property 
   :index index
   :right-node right-node})

(defn convert-index [idx]
  (if (< idx 0)
    nil
    idx))

(defmulti convert-operation class)

(defmethod convert-operation Delete [operation]
  (make-delete (.getOriginal operation)))

(defmethod convert-operation Insert [operation]
  (make-insert (.getOriginal operation)
               (.getRightParent operation)
               (.getRightNode operation)
               (.getProperty operation)
               (convert-index (.getIndex operation))))


(defmethod convert-operation Move [operation]
  (make-move (.getOriginal operation)
             (.getNewParent operation)
             (.getProperty operation)
             (convert-index (.getIndex operation))))

(defmethod convert-operation Update [operation]
  (make-update (.getOriginal operation)
               (.getRightParent operation)
               (.getProperty operation)))

;;Clojure Functions

(defn make-differencer [left-ast right-ast]
  (new changenodes.Differencer left-ast right-ast))

(defn difference [differencer]
  (.difference differencer)
  differencer)

(defn get-operations [differencer]
  (seq (.getOperations differencer)))

(def get-ast-changes
  (memoize 
    (fn
      [left-ast right-ast]
      (let [differencer (make-differencer left-ast right-ast)]
        (seq (map convert-operation (get-operations (difference differencer))))))))


(defn apply-change [change ast]
  (let [updated-change (.setAffectedNode change ast)]
    (.apply updated-change)))


(defn compilation-unit? [astnode]
  (= (.getNodeType astnode) org.eclipse.jdt.core.dom.ASTNode/COMPILATION_UNIT))

(defn get-ast-path [ast]
  (let [root (.getRoot ast)]
    (when (compilation-unit? root)
      (let [javaelement (.getJavaElement root)] ;;should be a org.eclipse.jdt.internal.core.CompilationUnit
        (.getPath javaelement)))))

        

;;Reification

(defn change [?change ?left-ast ?lroot ?rroot]
  (logic/fresh [?changes]
             (jdt/ast :CompilationUnit ?lroot)
             (jdt/ast-root ?left-ast ?lroot)
             (jdt/ast :CompilationUnit ?rroot)
             (logic/project [?lroot ?rroot]
                            (logic/== ?changes (get-ast-changes ?lroot ?rroot)))
             (logic/membero ?change ?changes)
             (logic/project [?change]
                            (logic/featurec ?change {:original ?left-ast}))))


(defn ast-path [?ast ?path]
  (logic/all
    (jdt/ast :ASTNode ?ast)
    (logic/project [?ast]
                   (logic/== ?path (get-ast-path ?ast)))))



               
               


;;Combining changes


;;Idea
;;Anti-unify changes by looping through the properties and seeing which ones differ/remain the same
;;Select the ones that would not affect any other nodes in the current program

;;Moeten context nakijken om te zien hoe de change relateert
;;Hierbij moeten we zowel naar boven als naar beneden wandelen

;;Zolang we naar boven kunnen gaan en dezelfde type node tegenkomen kunnen we nodes blijven behouden
;;Vanaf we != type tegenkomen kunnen we vervangen door logische var
;;Goed mogelijk dat we die node moeten vervangen door een logische var en verder naar boven blijven gaan
;;Maar pakt da da initieel nie nodig is

;;Hetzelfde in het afdalen van de node

;;Matcher gebruiken hiervoor en matchende nodes laten staan, de rest vervangen door logische vars?
;;Ik moet minder lijm snuiven


;;Voordelen van deze aanpak: context van changes wordt afgeleid van de AST waar hij plaats vindt.
;;Bijv: ge verandert een method van naam en past dan ook al de callers van die method aan.
;;De naamaanpassing zelf wordt niet veralgemeend, maar al de callers vd method wordt bijv wel aangepast
;;Het is gewoon te hopen dat context van 1 file voldoende is, en we niet verder moeten kijken
;;Zou wss toch niet schalen



;clojure representatie van een astnode
(defn make-ast [type properties]
  {:type type :properties properties})

