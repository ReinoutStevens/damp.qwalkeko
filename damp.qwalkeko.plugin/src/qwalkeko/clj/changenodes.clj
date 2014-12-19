(ns qwalkeko.clj.changenodes
  (:import [changenodes.Differencer])
  (:import [changenodes.operations
            Delete Insert Move Update] )
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt.astnode :as astnode])
  (:require [qwalkeko.clj.ast :as ast])
  (:require [damp.ekeko.jdt
             [ast :as jdt]]))




;;Changes
(defrecord CDelete  [operation original copy index property dependent-on])
(defrecord CMove [operation original copy left-parent right-parent property index dependencies dependent-on])
(defrecord CUpdate [operation original copy left-parent right-parent property dependent-on])
(defrecord CInsert [operation original copy left-parent right-parent property index dependencies dependent-on])

(defrecord CListInsert [operation original copy left-parent right-parent property index dependencies dependent-on child])
(defrecord CListMove [operation original copy left-parent right-parent property index dependencies dependent-on child])
(defrecord CListDelete  [operation original copy index property dependent-on child])


(defmethod print-method CInsert [x ^java.io.Writer writer]
  (print-method (dissoc x :dependencies :dependent-on) writer))

(defmethod print-method CMove [x ^java.io.Writer writer]
  (print-method (dissoc x :dependencies :dependent-on) writer))

(defmethod print-method CDelete [x ^java.io.Writer writer]
  (print-method (dissoc x :dependent-on) writer))

(defmethod print-method CUpdate [x ^java.io.Writer writer]
  (print-method (dissoc x :dependent-on) writer))

(defmethod print-method CListMove [x ^java.io.Writer writer]
  (print-method (dissoc x :dependencies :dependent-on :child) writer))

(defmethod print-method CListInsert [x ^java.io.Writer writer]
  (print-method (dissoc x :dependencies :dependent-on :child) writer))

(defmethod print-method CListDelete [x ^java.io.Writer writer]
  (print-method (dissoc x :dependent-on :child) writer))

(defn convert-index [idx]
  (if (< idx 0)
    nil
    (atom idx)))

(defn convert-property [property]
  (astnode/ekeko-keyword-for-property-descriptor property))



(defn get-node-idx [node property idx]
  (if (nil? idx)
    (ast/has-clj property node)
    (nth (seq (ast/has-clj-unwrap property node)) @idx)))

(defmulti convert-operation class)

(defmethod convert-operation Delete [operation]
  (let [idx (convert-index (.getIndex operation))
        m {:operation :delete
           :original (.getOriginal operation)
           :copy (.getAffectedNode operation)
           :index idx
           :property (convert-property (.getLocationInParent (.getOriginal operation)))
           :dependent-on (atom nil)}]
    (if idx
      (map->CDelete (assoc m :child (atom nil)))
      (map->CDelete m))))

(defmethod convert-operation Insert [operation]
  (let [prop (convert-property (.getProperty operation))
        idx (convert-index (.getIndex operation))
        original (.getOriginal operation)
        m {:operation :insert
           :original original
           :copy (get-node-idx (.getAffectedNode operation) prop idx) ;newly inserted node in left
           :left-parent (.getAffectedNode operation)
           :right-parent  (.getRightParent operation)
           :property prop
           :index idx
           :dependencies (atom {})
           :dependent-on (atom nil)}]
    (if idx
      (map->CListInsert (assoc m :child (atom nil)))
      (map->CInsert m))))

(defmethod convert-operation Move [operation]
  (let [prop (convert-property (.getProperty operation))
        idx (convert-index (.getIndex operation))
        m {:operation :move
           :original (.getOriginal operation)
           :copy (get-node-idx (.getLeftNode operation) prop idx)
           :left-parent (.getParent (.getLeftNode operation))
           :right-parent (.getNewParent operation)
           :property prop
           :index idx
           :dependencies (atom {})
           :dependent-on (atom nil)}]
    (if idx
      (map->CListMove (assoc m :child (atom nil)))
      (map->CMove m))))

(defmethod convert-operation Update [operation]
  (let [prop (convert-property (.getProperty operation))]
    (->CUpdate :update
      (ast/has-clj-unwrap prop (.getOriginal operation))
      (ast/has-clj-unwrap prop (.getAffectedNode operation))
      (.getLeftParent operation)
      (.getRightParent operation)
      prop
      (atom nil))))



(defn insert? [x]
  (= :insert (:operation x)))

(defn move? [x]
  (= :move (:operation x)))

(defn update? [x]
  (= :update (:operation x)))

(defn delete? [x]
  (= :delete (:operation x)))

(defn list-operation? [x]
  (not= nil (:child x)))

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


(defn insert-dependent-on [insert]
  @(:dependent-on insert))

(defn insert-dependencies [insert]
  @(:dependencies insert))

(defmulti change-index-if-removed (fn [operation idx] (class operation)))
(defmethod change-index-if-removed CInsert [operation idx]
  (dec idx))

(defmethod change-index-if-removed CMove [operation idx]
  (dec idx))

(defmethod change-index-if-removed CUpdate [operation idx]
  idx)

(defmethod change-index-if-removed CDelete [operation idx]
  (inc idx))

(defn insert-get-independent-index [insert]
  (defn find-parent-and-update-idx [idx parentnode current]
    (if (or (not= parentnode (:original current)) (nil? current))
      idx
      (recur (change-index-if-removed current idx) parentnode @(:dependent-on current))))
  (let [original (:original insert)]
    (if-let [idx (:index insert)]
      (find-parent-and-update-idx @idx original @(:dependent-on insert))
      nil)))


(defn insert-add-dependency [insert dependee]
  "dependee depends on insert"
  (let [prop (:property dependee)]
    (swap! (:dependencies insert)
      (fn [h]
        (assoc h prop dependee)))
    (reset! (:dependent-on dependee) insert)))
    


(defmulti insert-change-dependent? 
  "dependent depdends on insert. Only used during the creation of the dependency graph." 
  (fn [insert dependent] (class dependent)))

(defmethod insert-change-dependent? :default [insert change] 
  (= (:left-parent change) (:copy insert))) 
    
(defmethod insert-change-dependent? CDelete [insert delete]
 false) ;;is done later

(defmethod insert-change-dependent? CInsert [insert depends]
  (and 
    (not= insert depends)
    (= (:left-parent depends) (:copy insert))))

(defmethod insert-change-dependent? CListInsert [insert listinsert]
  (and  
    (= (:left-parent listinsert) (:copy insert))
    (nil? ((:property listinsert) @(:dependencies insert)))))

(defmethod insert-change-dependent? CListMove [insert listmove]
  (and  
    (= (:left-parent listmove) (:copy insert))
    (nil? ((:property listmove) @(:dependencies insert)))))


(defmethod insert-change-dependent? CListDelete [insert listdelete]
  false)


(defn insert-remove-dependency [insert dependency]
  (let [prop (:property dependency)
        all-deps (insert-dependent-on insert)
        deps (prop (insert-dependencies insert))
        idx (.indexOf deps dependency)
        [t d] (split-at idx deps)]
      (if-not (nil? (:index dependency))
        (swap! (:dependencies insert) (fn [h val] (assoc h prop (concat t (map #(swap! (:index %) dec) (rest d))))))
        (swap! (:dependencies insert) (fn [h val] (assoc h prop (concat t (rest d))))))))




(defn insert-delete-redundant? [insert delete]
  (if-let [delete-original (:original delete)]
    (if-let [insert-parent (:original insert)]
      (let [delete-parent (.getParent delete-original)
            insert-left (get-node-idx (:right-parent insert) (:property insert) (:index insert))]
        (if (= delete-parent insert-parent)
          (if (ast/match? delete-original insert-left) ;;nodes look the same
            ;;lets verify that they are added at the same position
            (if (and (:index insert) (:index delete))
              (= (insert-get-independent-index insert) @(:index delete))
              false)
            false)
          false))
      false)
    false))
              

(defn insert-link-direct-dependencies [changes]
  "links ChildProperty to an insert"
  (let [inserts (filter insert? changes)]
    (doall
      (map 
        (fn [i]
          (doall
            (map 
              (fn [c]
                (when (insert-change-dependent? i c)
                  (insert-add-dependency i c)))
              changes)))
        inserts))))


(defn link-list-dependencies [changes]
  "links operations in the same ChildListProperty in a linked list (without deletes)"
  (defn link-group [group]
    (reduce
      (fn [parent child]
        (reset! (:dependent-on child) parent)
        (reset! (:child parent) child)
        child)
      group))
  (let [listchanges (remove delete? (filter list-operation? changes))
        grouped (group-by :left-parent listchanges)]
    (doall
      (map link-group
        (mapcat #(vals (group-by :property %)) (vals grouped))))))


(defn link-delete-dependencies [changes]
  "adds deletes to groups of linked operations"
  (defn link-delete-group [group]
    (defn loop-and-add [prev current group]
      (when-not (empty? group)
        (if (nil? current) ;;end of the list
          (let [gcurr (first group)] ;;probably better to write with reduce
            (when-not (nil? prev)
              (reset! (:child prev) gcurr)
              (reset! (:dependent-on gcurr) prev))
            (recur gcurr nil (rest group)))
          (let [gcurr (first group)
                gidx @(:index gcurr)
                idx @(:index current)]
            (if (< gidx idx)
              (do
                (when-not (nil? prev)
                  (reset! (:child prev) gcurr)
                  (reset! (:dependent-on gcurr) prev))
                (reset! (:dependent-on current) gcurr)
                (reset! (:child gcurr) current)
                (recur gcurr current (rest group)))
              (recur current @(:child current) group))))))
    (let [group-root (.getParent (:original (first group)))
          group-property (:property (first group))
          list-root (first (filter #(and 
                                      (= (:original %) group-root)
                                      (= group-property (:property %)))
                             (filter list-operation? changes)))]
      (loop-and-add nil list-root group)))
          
  (let [listdeletes (filter delete? (filter list-operation? changes))
        grouped (group-by #(.getParent (:original %)) listdeletes)]
    (doall
      (map link-delete-group
        (mapcat #(vals (group-by :property %)) (vals grouped))))))





(defn link-group-roots [changes]
  (defn find-and-set-root [operation roots]
    (let [root (some #(= (:copy %) (:left-parent operation)) roots)]
      (when-not (nil? root)
        (reset! (:dependent-on operation) root)
        (swap! (:dependencies root)
          (fn [h]
            (assoc h (:property operation) operation))))))
  (let [possible-roots (remove list-operation? (filter insert? changes))
        ;;deletes should never be inside an insert as the node shouldnt get inserted in the first place
        list-roots (remove delete? (filter #(nil? (:dependent-on %)) (filter list-operation? changes)))] 
    (doall
      (map #(find-and-set-root % possible-roots) list-roots))))


(defn create-dependency-graph [changes]
  (defn graphify []
    (let [roots (filter #(nil? @(:dependent-on %)) changes)]
      {:roots roots :changes changes}))
  (insert-link-direct-dependencies changes)
  (link-list-dependencies changes)
  (link-delete-dependencies changes)
  (link-group-roots changes)
  (graphify))
        
    
          