(ns qwalkeko.clj.changenodes
  (:import [changenodes.Differencer])
  (:import [changenodes.operations
            Delete Insert Move Update])
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt.astnode :as astnode])
  (:require [damp.ekeko.jdt
             [ast :as jdt]]))




;;Changes
(derive ::delete ::operation)
(derive ::move ::operation)
(derive ::update ::operation)
(derive ::insert ::operation)

(defn make-delete [astnode]
  {:operation :delete :original astnode})

(defn make-move [astnode new-parent right-node property index]
  {:operation :move 
   :original astnode 
   :property property 
   :index index 
   :new-parent new-parent
   :right-node right-node})

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

(defn convert-property [property]
  (astnode/ekeko-keyword-for-property-descriptor property))

(defmulti convert-operation class)

(defmethod convert-operation Delete [operation]
  (make-delete (.getOriginal operation)))

(defmethod convert-operation Insert [operation]
  (make-insert (.getOriginal operation)
               (.getRightParent operation)
               (.getRightNode operation)
               (convert-property (.getProperty operation))
               (convert-index (.getIndex operation))))


(defmethod convert-operation Move [operation]
  (make-move (.getOriginal operation)
             (.getNewParent operation)
             (.getRightNode operation)
             (convert-property (.getProperty operation))
             (convert-index (.getIndex operation))))

(defmethod convert-operation Update [operation]
  (make-update (.getOriginal operation)
               (.getRightParent operation)
               (convert-property (.getProperty operation))))

;;Clojure Functions

(defn make-differencer [left-ast right-ast]
  (new changenodes.Differencer left-ast right-ast))

(defn difference [differencer]
  (.difference differencer)
  differencer)

(defn get-operations [differencer]
  (seq (.getOperations differencer)))

(def get-ast-changes
  (fn
    [left-ast right-ast]
    (let [differencer (make-differencer left-ast right-ast)]
      (seq (map convert-operation (get-operations (difference differencer)))))))


(defn apply-change [change ast]
  (let [updated-change (.setAffectedNode change ast)]
    (.apply updated-change)))


;;
(defn update-get-value [update]
  (let [{:keys [right-parent property]} update]
    ((property (damp.ekeko.jdt.astnode/reifiers right-parent)) right-parent)))
    

;;note that these are not 100% correct: an insert is never used to update smthg
;;still we allow the conversion to make reasoning over changes easier
(defn move->delete-insert [move]
  (let [moved-node (:original move)
        idx (:index move)
        property (:property move)
        new-parent (:new-parent move)
        towards (:right-node move)
        delete (make-delete moved-node)
        insert (make-insert new-parent (.getParent towards) towards property idx)]
    [delete insert]))


(defn update->delete-insert [update]
  (let [updated-node (:original update)
        property (:property update)
        right-parent (:right-parent update)
        new-value (update-get-value update)
        delete (make-delete updated-node)
        insert (make-insert (.getParent updated-node) right-parent new-value property nil)]
    [delete insert]))

(defmulti normalize-change :operation)

(defmethod normalize-change :delete [delete]
  [delete])

(defmethod normalize-change :insert [insert]
  [insert])

(defmethod normalize-change :move [move]
  (move->delete-insert move))

(defmethod normalize-change :update [update]
  (update->delete-insert update))

(defn normalize-changes [changes]
  (mapcat normalize-change changes))