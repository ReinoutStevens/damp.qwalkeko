(ns qwalkeko.clj.changenodes
  (:import [changenodes.Differencer])
  (:import [changenodes.operations
            Delete Insert Move Update] )
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt.astnode :as astnode])
  (:require [qwalkeko.clj.ast :as ast])
  (:require [damp.ekeko.jdt
             [ast :as jdt]]))

;;Converting Java Changes to Clojure Changes (classes prefixed with a C)
(defrecord CDelete  [operation original copy property index dependency]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CDelete. :delete nil nil nil (atom nil) (atom nil))))

(defrecord CMove [operation original copy left-parent right-parent property index dependents dependency]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CMove. :move nil nil nil nil nil (atom nil) (atom nil) (atom nil))))

(defrecord CUpdate [operation original copy left-parent right-parent property dependency]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CUpdate. :update nil nil nil nil nil (atom nil))))

(defrecord CInsert [operation original copy left-parent right-parent property index dependents dependency]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CInsert. :insert nil nil nil nil nil (atom nil) (atom nil) (atom nil))))

(defrecord CListInsert [operation original copy left-parent right-parent property index dependents dependency child]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CListInsert. :insert nil nil nil nil nil (atom nil) (atom nil) (atom nil) (atom nil))))

(defrecord CListMove [operation original copy left-parent right-parent property index dependents dependency child]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CListMove. :move nil nil nil nil nil (atom nil) (atom nil) (atom nil) (atom nil))))

(defrecord CListDelete  [operation original copy property index dependency child]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CListDelete. :delete nil nil nil (atom nil) (atom nil) (atom nil))))


;;Custom Printing so we do not get stuck in printing cycles
(defmethod print-method CInsert [x ^java.io.Writer writer]
  (print-method (dissoc x :dependents :dependency) writer))

(defmethod print-method CMove [x ^java.io.Writer writer]
  (print-method (dissoc x :dependents :dependency) writer))

(defmethod print-method CDelete [x ^java.io.Writer writer]
  (print-method (dissoc x :dependency) writer))

(defmethod print-method CUpdate [x ^java.io.Writer writer]
  (print-method (dissoc x :dependency) writer))

(defmethod print-method CListMove [x ^java.io.Writer writer]
  (print-method (dissoc x :dependents :dependency :child) writer))

(defmethod print-method CListInsert [x ^java.io.Writer writer]
  (print-method (dissoc x :dependents :dependency :child) writer))

(defmethod print-method CListDelete [x ^java.io.Writer writer]
  (print-method (dissoc x :dependency :child) writer))

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
           :dependency (atom nil)}]
    (if idx
      (map->CListDelete (assoc m :child (atom nil)))
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
           :dependents (atom {})
           :dependency (atom nil)}]
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
           :right-parent (.getParent (.getRightNode operation))
           :property prop
           :index idx
           :dependents (atom {})
           :dependency (atom nil)}]
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

(defn updateo [x]
  (logic/project [x]
    (logic/== true (update? x))))

(defn inserto [x]
  (logic/project [x]
    (logic/== true (insert? x))))

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


(defn insert-add-dependency [insert dependee]
  "dependee depends on insert"
  (let [prop (:property dependee)]
    (swap! (:dependents insert)
      (fn [h]
        (assoc h prop dependee)))
    (reset! (:dependency dependee) insert)))
    


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
    (nil? ((:property listinsert) @(:dependents insert)))))

(defmethod insert-change-dependent? CListMove [insert listmove]
  (and  
    (= (:left-parent listmove) (:copy insert))
    (nil? ((:property listmove) @(:dependents insert)))))


(defmethod insert-change-dependent? CListDelete [insert listdelete]
  false)

              

(defn insert-link-direct-dependents [changes]
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


(defn link-list-dependents [changes]
  "links operations in the same ChildListProperty in a linked list (without deletes)"
  (defn link-group [group]
    (reduce
      (fn [parent child]
        (reset! (:dependency child) parent)
        (reset! (:child parent) child)
        child)
      group))
  (let [listchanges (remove delete? (filter list-operation? changes))
        grouped (group-by :left-parent listchanges)]
    (doall
      (map link-group
        (mapcat #(vals (group-by :property %)) (vals grouped))))))


(defn link-delete-dependents [changes]
  "adds deletes to groups of linked operations"
  (defn link-delete-group [group]
    (defn loop-and-add [prev current group]
      (when-not (empty? group)
        (if (nil? current) ;;end of the list
          (let [gcurr (first group)] ;;probably better to write with reduce
            (when-not (nil? prev)
              (reset! (:child prev) gcurr)
              (reset! (:dependency gcurr) prev))
            (recur gcurr nil (rest group)))
          (let [gcurr (first group)
                gidx @(:index gcurr)
                idx @(:index current)]
            (if (< gidx idx)
              (do
                (when-not (nil? prev)
                  (reset! (:child prev) gcurr)
                  (reset! (:dependency gcurr) prev))
                (reset! (:dependency current) gcurr)
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
        (reset! (:dependency operation) root)
        (swap! (:dependents root)
          (fn [h]
            (assoc h (:property operation) operation))))))
  (let [possible-roots (remove list-operation? (filter insert? changes))
        ;;deletes should never be inside an insert as the node shouldnt get inserted in the first place
        list-roots (remove delete? (filter #(nil? (:dependency %)) (filter list-operation? changes)))] 
    (doall
      (map #(find-and-set-root % possible-roots) list-roots))))


(defn create-dependency-graph [changes]
  (defn graphify []
    (let [roots (filter #(nil? @(:dependency %)) changes)]
      {:roots roots :changes changes}))
  (insert-link-direct-dependents changes)
  (link-list-dependents changes)
  (link-delete-dependents changes)
  (link-group-roots changes)
  (graphify))
        

(defn ast-ast-graph [left right]
  (let [changes (get-ast-changes left right)]
    (create-dependency-graph changes)))

;;Graph helpers
(defn change-graph-roots [graph]
  (:roots graph))

(defn change-graph-dependencies [graph]
  (remove #(nil? @(:dependency %)) (:changes graph)))


;;qwal integration to navigate changes a bit better
(defn change-root [?root graph]
  (logic/project [graph]
    (damp.ekeko.logic/contains (:roots graph) ?root)))

(defn change [?change graph]
  (logic/project [graph]
    (damp.ekeko.logic/contains (:changes graph) ?change)))


(defn change=>> []
  (fn [graph change ?dependent]
    (logic/fresh [?prop]
      (logic/project [change]
        (logic/fresh [?map]
          (logic/== false (update? change))
          (logic/== ?map (:dependents change))
          (logic/project [?map]
            (damp.ekeko.logic/contains (seq (keys @?map)) ?prop)
            (logic/project [?prop]
              (logic/== ?dependent (?prop @(:dependents change))))))))))

(defn change=> [?prop]
  (fn [graph change ?dependent]
    (logic/project [change]
      (logic/fresh [?map]
        (logic/== false (update? change))
        (logic/== ?map (:dependents change))
        (logic/project [?map]
	          (damp.ekeko.logic/contains (seq (keys @?map)) ?prop)
           (logic/project [?prop]
             (logic/== ?dependent (?prop @(:dependents change)))))))))


(defn change<= []
  (fn [graph change ?dependency]
    (logic/project [change]
      (logic/fresh [?map]
        (logic/== ?map (:dependency change))
        (logic/project [?map]
          (logic/== ?dependency @?map)
          (logic/!= nil ?dependency))))))

(defn change=c> []
  (fn [graph listchange ?dependent]
    (logic/project [listchange]
      (logic/fresh [?c]
        (logic/== true (list-operation? listchange))
        (logic/== ?c (:child listchange))
        (logic/project [?c]
          (logic/== ?dependent @?c))))))

(defn change=c< [] 
  (fn [graph listchange ?dependency]
    (logic/project [listchange]
      (logic/== true (list-operation? listchange))
      (logic/fresh [?c]
        (logic/== ?c (:dependency listchange))
        (logic/project [?c]
          (logic/== ?dependency @?c))))))
    


    
;;Operations that work on changes that are already in a graph
(defn list-operation-parent-operation [x]
  (when (list-operation? x)
    (cons x (lazy-seq (list-operation-parent-operation @(:dependency x))))))

(defn list-operation-children [x]
  (when (list-operation? x)
    (cons x (lazy-seq (list-operation-children @(:child x))))))


(defn change-dependents-recursive [change]
  (let [deps-atom (:dependents change)]
    (when deps-atom
      (let [deps (vals @deps-atom)]
        (concat deps (mapcat change-dependents-recursive deps))))))


(defmulti change-index-if-removed (fn [operation idx] (class operation)))
(defmethod change-index-if-removed :default [operation idx]
  idx)

(defmethod change-index-if-removed CListInsert [operation idx]
  (dec idx))

(defmethod change-index-if-removed CListMove [operation idx]
  (dec idx))

(defmethod change-index-if-removed CListDelete [operation idx]
  (inc idx))

(defn change-get-independent-index [operation]
  (let [idx (:index operation)]
    (when idx
      (reduce (fn [idx op] (change-index-if-removed op idx)) @idx (rest (list-operation-parent-operation operation))))))


(defn change-remove [graph change]
   (defn change-remove-regular [change]
     (let [parent @(:dependency change)]
       (when parent
         (swap! (:dependents parent) #(dissoc % (:property change))))
       (reset! (:dependency change) nil)
       {:roots (remove #{change}  (:roots graph))
        :changes (remove (set (conj (change-dependents-recursive change) change)) (:changes graph))}))
   
  (defn change-remove-list [change]
    (let [prev @(:dependency change)
          next @(:child change)]
      (when next
        (reset! (:dependency next) prev)
        (doall (map #(swap! (:index %) (fn [idx c] (change-index-if-removed c idx)) change) (list-operation-children next))))
      (when prev
        (if (list-operation? prev)
          (reset! (:child prev) next)
          (swap! (:dependents prev) #(dissoc % (:property change)))))
      {:roots (remove #{change} (:roots graph))
       :changes (remove (set (conj (change-dependents-recursive change) change)) (:changes graph))}))
  (if (list-operation? change)
    (change-remove-list change)
    (change-remove-regular change)))




(defn insert-delete-redundant? [insert delete]
  (if-let [delete-original (:original delete)]
    (if-let [insert-parent (:original insert)]
      (let [delete-parent (.getParent delete-original)
            insert-left (get-node-idx (:right-parent insert) (:property insert) (:index insert))]
        (if (= delete-parent insert-parent)
          (if (ast/match? delete-original insert-left) ;;nodes look the same
            ;;lets verify that they are added at the same position
            (if (and (:index insert) (:index delete))
              (= (change-get-independent-index insert) @(:index delete))
              false)
            false)
          false))
      false)
    false))


(defn insert-has-redundant-delete? [insert]
  (let [child @(:child insert)]
    (and 
      (delete? child)
      (= (change-get-independent-index insert) 
        (change-get-independent-index child))
      (ast/match? (:copy insert) (:original child)))))

(defn delete-has-redundant-insert? [delete]
  (let [child @(:child delete)]
    (and
      (insert? child)
      (= (change-get-independent-index delete) 
        (change-get-independent-index child))
      (ast/match? (:copy child) (:original delete)))))


(defn remove-redundant-operations [graph]
  (defn remove-redundant-operation [graph change]
    (let [next @(:child change)]
      (change-remove (change-remove graph next) change)))
  (let [changes (:changes graph)
        list-operations (filter list-operation? changes)
        list-inserts (filter insert? list-operations)
        list-delete (filter delete? list-operations)
        redundant-inserts (filter insert-has-redundant-delete? list-inserts)
        redundant-deletes (filter delete-has-redundant-insert? list-delete)]
    (reduce remove-redundant-operation graph (concat redundant-deletes redundant-inserts))))
        
        



(comment
  (logic/run* [?cu]
 (logic/fresh [?typedecl ?tname ?package ?pname]
   (jdt/ast :CompilationUnit ?cu)
   (jdt/has :package ?cu ?package)
   (jdt/has :name ?package ?pname)
   (jdt/name|qualified-string ?pname "org.apache.tools.ant")
   (ast/compilationunit-typedeclaration|main ?cu ?typedecl)
   (jdt/has :name ?typedecl ?tname)
   (jdt/name|simple-string ?tname "DirectoryScanner"))))

