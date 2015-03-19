(ns qwalkeko.clj.functionalnodes
  (:import [changenodes.Differencer])
  (:import [changenodes.operations
            Delete Insert Move Update] )
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt.astnode :as astnode])
  (:require [damp.ekeko.logic :as el])
  (:require [damp.ekeko.jdt.astnode :as astnode])
  (:require [qwalkeko.clj.ast :as ast])
  (:require [qwalkeko.clj.changes :as change])
  (:require [damp.qwal :as qwal])
  (:require [damp.ekeko.jdt
             [ast :as jdt]]))

;;Converting Java Changes to Clojure Changes (classes prefixed with a C)
(defrecord CDelete  [operation original copy property index graph-idx]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CDelete. :delete nil nil nil nil nil)))

(defrecord CMove [operation original copy left-parent right-parent property index graph-idx]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CMove. :move nil nil nil nil nil nil nil)))

(defrecord CUpdate [operation original copy left-parent right-parent property graph-idx]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CUpdate. :update nil nil nil nil nil nil)))

(defrecord CInsert [operation original copy left-parent right-parent property index graph-idx]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CInsert. :insert nil nil nil nil nil nil nil)))

(defrecord CListDelete  [operation original copy property index dependency graph-idx]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CListDelete. :delete nil nil nil nil nil nil)))

(defrecord CListMove [operation original copy left-parent right-parent property index graph-idx]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CListMove. :move nil nil nil nil nil nil nil)))

(defrecord CListInsert [operation original copy left-parent right-parent property index graph-idx]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CListInsert. :insert nil nil nil nil nil nil nil)))


;;Converting Java Operations to Clojure

(defn- convert-index [idx]
  (if (< idx 0)
    nil
    idx))

(defn- convert-property [property]
  (astnode/ekeko-keyword-for-property-descriptor property))

(defn- get-node-idx [node property idx]
  (if (nil? idx)
    (ast/has-clj property node)
    (nth (seq (ast/has-clj-unwrap property node)) idx)))

(defmulti convert-operation class)

(defmethod convert-operation Delete [operation]
  (let [idx (convert-index (.getIndex operation))
        m {:operation :delete
           :original (.getOriginal operation)
           :copy (.getAffectedNode operation)
           :index idx
           :property (convert-property (.getLocationInParent (.getOriginal operation)))
           :graph-idx nil}]
    (if idx
      (map->CListDelete m)
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
           :graph-idx nil}]
    (if idx
      (map->CListInsert m)
      (map->CInsert m))))

(defmethod convert-operation Move [operation]
  (let [prop (convert-property (.getProperty operation))
        idx (convert-index (.getIndex operation))
        m {:operation :move
           :original (.getOriginal operation)
           :copy (.getLeftNode operation)
           :left-parent (.getParent (.getLeftNode operation))
           :right-parent (.getParent (.getRightNode operation))
           :property prop
           :index idx
           :graph-idx nil}]
    (if idx
      (map->CListMove m)
      (map->CMove m))))

(defmethod convert-operation Update [operation]
  (let [prop (convert-property (.getProperty operation))
        m {:operation :update
           :original (ast/has-clj-unwrap prop (.getOriginal operation))
           :copy (ast/has-clj prop (.getLeftParent operation))
           :left-parent (.getLeftParent operation)
           :right-parent (.getRightParent operation)
           :property prop
           :graph-idx nil}]
    (map->CUpdate m)))

;;Interface with Java Differencer
(defn- make-differencer [left-ast right-ast]
  (new changenodes.Differencer left-ast right-ast))

(defn- difference [differencer]
  (.difference differencer)
  differencer)

(defn- get-operations [differencer]
  (seq (.getOperations differencer)))

(defn- left-matching [differencer]
  (.getLeftMatching differencer))

(defn- right-matching [differencer]
  (.getRightMatching differencer))

(defn get-ast-changes [left-ast right-ast]
  (let [differencer (make-differencer left-ast right-ast)
        converted (seq (map convert-operation (get-operations (difference differencer))))]
    {:changes converted 
     :differencer differencer}))

;;Some useful functions
(defn insert? [x]
  (= :insert (:operation x)))

(defn move? [x]
  (= :move (:operation x)))

(defn update? [x]
  (= :update (:operation x)))

(defn delete? [x]
  (= :delete (:operation x)))

(defmulti list-operation? class)
(defmethod list-operation? :default [x]
  false)
(defmethod list-operation? CListInsert [x]
  true)
(defmethod list-operation? CListMove [x]
  true)
(defmethod list-operation? CListDelete [x]
  true)

;;Make Graph of Change Dependencies

(defn changes->graph [changes]
  (let [size (count (:changes changes))]
    {:changes (vec (map-indexed (fn [idx c] (assoc c :graph-idx idx)) (:changes changes)))
     :dependents (vec (take size (repeat {})))
     :dependency (vec (take size (repeat nil)))
     :child (vec (take size (repeat nil)))
     :differencer (:differencer changes)}))

(defn graph-node-idx [graph idx]
  (when-not (nil? idx)
    (nth (:changes graph) idx)))
  

(defn change-dependents-idx [graph change]
  (nth (:dependents graph) (:graph-idx change)))

(defn change-dependents [graph change]
  (map #(graph-node-idx graph %)
    (change-dependents-idx graph change)))
                         
(defn change-dependency-idx [graph change]
  (nth (:dependency graph) (:graph-idx change)))

(defn change-dependency [graph change]
 (graph-node-idx graph
   (change-dependency-idx graph change)))


(defn change-child-idx [graph change]
  (nth (:child graph) (:graph-idx change)))

(defn change-child [graph change]
  (graph-node-idx graph 
    (change-child-idx graph change)))
    
(defn change-add-dependency [graph change dependent]
  (let [idx (:graph-idx change)
        property (:property dependent)
        dependents (:dependents graph)
        dependent-map (nth dependents idx)
        copy (:copy change)
        all-parents (iterate #(.getParent %) (:left-parent dependent))
        parents (take-while (fn [x] (not= x copy)) all-parents)
        properties (cons property (map #(astnode/ekeko-keyword-for-property-descriptor 
                                         (.getLocationInParent %)) parents))
        new-dependents (assoc dependents idx (assoc-in dependent-map properties (:graph-idx dependent)))
        new-dependency (assoc (:dependency graph) (:graph-idx dependent) idx)]
    (assoc graph 
      :dependents new-dependents
      :dependency new-dependency)))


(defn change-add-list-dependency [graph change dependent]
  (let [idx (:graph-idx change)
        property (:property dependent)
        dependents (:dependents graph)
        dependent-map (nth dependents idx)
        copy (:copy change)
        new-dependents (assoc dependents idx (assoc dependent-map property (:graph-idx dependent)))
        new-dependency (assoc (:dependency graph) (:graph-idx dependent) idx)]
    (assoc graph 
      :dependents new-dependents
      :dependency new-dependency)))


(defn insert-change-already-linked? [graph insert change]
    (let [property (:property change)
          all-parents (iterate #(.getParent %) (:left-parent change))
          copy (:copy insert)
          parents (take-while (fn [x] (not= x copy)) all-parents)
          properties (cons property (map #(astnode/ekeko-keyword-for-property-descriptor 
                                            (.getLocationInParent %)) parents))]
      (get-in (change-dependents graph insert) properties)))
      

(defn change-dependents-recursive [graph change]
  (let [changes (change-dependents graph change)]
    (mapcat #(change-dependents-recursive graph %) changes)))


(defn change-dependents-idx-recursive [graph change]
  (map :graph-idx (change-dependents-recursive graph change)))

(defn change-add-child [graph change child]
  (let [children (:child graph)]
    (assoc graph
      :child
      (assoc children (:graph-idx change) (:graph-idx child)))))

(defn change-remove-dependency [graph change dependent]
   (let [prop (:property dependent)
         idx (:graph-idx change)
         property (:property dependent)
         dependents (nth idx (:dependents graph))
         new-dependents (assoc (:dependents graph) idx (dissoc dependents property))
         new-dependency (assoc (:dependency graph) (:graph-idx dependent) nil)]
     (assoc graph 
       :dependents new-dependents
       :dependency new-dependency)))

;;Methods used to set up the graph
(defn insert-change-dependent-default? [graph insert change]
   (let [left-parent (:left-parent change)
        location (.getLocationInParent left-parent)
        mandatory (and (not (.isChildListProperty location))(.isMandatory location))]
    (or
      (= (:left-parent change) (:copy insert))
      (and mandatory (= (:copy insert)
                       (.getParent left-parent))))))

    
(defmulti insert-change-dependent? 
  "dependent depdends on insert. Only used during the creation of the dependency graph." 
  (fn [graph insert dependent] (class dependent)))

(defmethod insert-change-dependent? :default [graph insert change] 
  (insert-change-dependent-default? graph insert change))

(defmethod insert-change-dependent? CDelete [graph insert delete]
 false) ;;is done later

(defmethod insert-change-dependent? CInsert [graph insert depends]
  (and 
    (not= insert depends)
    (= (:left-parent depends) (:copy insert))))

(defmethod insert-change-dependent? CListInsert [graph insert listinsert]
  (and
    (insert-change-dependent-default? graph insert listinsert)
    (not (insert-change-already-linked? graph insert listinsert))))

(defmethod insert-change-dependent? CListMove [graph insert listmove]
  (and  
    (insert-change-dependent-default? graph insert listmove)
    (not (insert-change-already-linked? graph insert listmove))))

(defmethod insert-change-dependent? CListDelete [graph insert listdelete]
  false)

(defn- insert-link-direct-dependents [graph]
  "links ChildProperty to an insert"
  (let [changes (:changes graph)
        inserts (filter insert? changes)]
    (reduce
      (fn [graph i]
        (reduce
          (fn [graph c]
            (if (insert-change-dependent? graph i c)
              (change-add-dependency graph i c)
              graph))
          graph
          changes))
      graph
      inserts)))

(defn- link-list-dependents [graph]
  "links operations in the same ChildListProperty in a linked list (without deletes)"
  (defn link-group [graph group]
    ;;group is a list of nodes, at index i we find the parent of i+1 etc
    (let [parent-child (partition 2 1 group)] ;;we group together a parent and its child
      (reduce
        (fn [graph pc]
          (let [parent (first pc)
                child (second pc)]
            (change-add-child (change-add-list-dependency graph child parent) parent child)))
        graph
        parent-child)))
  (let [changes (:changes graph)
        listchanges (remove delete? (filter list-operation? changes))
        grouped (group-by :left-parent listchanges)]
    (reduce
      (fn [graph group]
        (link-group graph group))
      graph
      (mapcat #(vals (group-by :property %)) (vals grouped)))))

(defn- link-delete-dependents [graph]
  "adds deletes to groups of linked operations"
  (defn link-delete-group [graph group]
    ;;adds a group of deletes to a sequence of linked operations
    ;;prev is the previous operation in the sequence
    ;;current is the current operation
    ;;whenever the idx of the first elmt of group is < than the one of current operation
    ;;we insert it between prev and current
    (defn loop-and-add [graph prev current group]
      (if-not (empty? group)
        (if (nil? current) ;;end of the list
          (let [gcurr (first group)
                new-graph (if-not (nil? prev)
                            (-> graph
                              (change-add-child prev gcurr)
                              (change-add-list-dependency gcurr prev))
                            graph)]
            (recur new-graph gcurr nil (rest group)))
          (let [gcurr (first group)
                gidx (:index gcurr)
                idx (:index current)]
            (if (< gidx idx)
              ;;the first of our group must be inserted here
              (let [current-added-graph
                    (->
                      graph
                      (change-add-list-dependency current gcurr) 
                      (change-add-child gcurr current))
                    prev-added-graph
                    (if-not (nil? prev)
                      (-> current-added-graph
                        (change-add-child prev gcurr)
                        (change-add-list-dependency gcurr prev))
                      current-added-graph)]
                (recur prev-added-graph gcurr current (rest group)))
              (recur graph current (change-child graph current) group))))
        graph))  
    (let [changes (:changes graph)
          group-root (.getParent (:original (first group)))
          group-property (:property (first group))
          list-root (first (filter #(and 
                                      (= (:original %) group-root)
                                      (= group-property (:property %)))
                             (filter list-operation? changes)))]
      (loop-and-add graph nil list-root group)))
  (let [changes (:changes graph)
        listdeletes (filter delete? (filter list-operation? changes))
        grouped (group-by #(.getParent (:original %)) listdeletes)]
    (reduce
      link-delete-group
      graph
      (mapcat #(vals (group-by :property %)) (vals grouped)))))



(defn- link-group-roots [graph]
  (defn find-and-set-root [graph operation roots]
    (let [root (some #(= (:copy %) (:left-parent operation)) roots)]
      (if-not (nil? root)
        (change-add-dependency graph operation root)
        graph)))
  (let [changes (:changes graph)
        possible-roots (remove list-operation? (filter insert? changes))
        ;;deletes should never be inside an insert as the node shouldnt get inserted in the first place
        list-roots (remove delete? (filter #(nil? (change-dependency graph %)) (filter list-operation? changes)))] 
    (reduce
      (fn [g change]
        (find-and-set-root g change possible-roots))
      graph
      list-roots)))

(defn- add-roots-to-graph [graph]
  (let [dependency (:dependency graph)
        roots (filter #(nil? (nth dependency %))
                (seq (range (count (:changes graph)))))]
    (assoc graph :roots roots)))

(defn create-dependency-graph [changes]
  (let [graph (changes->graph changes)]
    (->
      graph
      insert-link-direct-dependents
      link-list-dependents
      link-delete-dependents
      link-group-roots
      add-roots-to-graph)))

(defn ast-ast-graph [left right]
  (let [changes (get-ast-changes left right)]
    (assoc (create-dependency-graph changes) :differencer (:differencer changes))))

;;Graph helpers
(defn change-graph-roots [graph]
  (map #(graph-node-idx graph %) (:roots graph)))

(defn change-graph-dependencies-idx [graph]
  (remove #(nil? (nth (:dependency graph) %)) (range (count (:changes graph)))))

(defn change-graph-dependencies [graph]
  (map #(graph-node-idx graph %) (change-graph-dependencies-idx graph)))

;;qwal integration to navigate changes a bit better
(defn change-root [?root graph]
  (logic/project [graph]
    (damp.ekeko.logic/contains (change-graph-roots graph) ?root)))

(defn change [?change graph]
  (logic/project [graph]
    (damp.ekeko.logic/contains (:changes graph) ?change)))

(defn change=>> []
  (fn [graph change ?dependent]
    (logic/fresh [?prop]
      (logic/project [change]
        (logic/fresh [?map]
          (logic/== false (update? change))
          (logic/== ?map (change-dependents graph change))
          (logic/project [?map]
            (damp.ekeko.logic/contains (seq (keys @?map)) ?prop)
            (logic/project [?prop]
              (logic/== ?dependent (?prop (change-dependents graph change))))))))))

(defn change=> [?prop]
  (fn [graph change ?dependent]
    (logic/project [change]
      (logic/fresh [?map]
        (logic/== false (update? change))
        (logic/== ?map (change-dependents graph change))
        (logic/project [?map]
	          (damp.ekeko.logic/contains (seq (keys @?map)) ?prop)
           (logic/project [?prop]
             (logic/== ?dependent (?prop (change-dependents graph change)))))))))

(defn change<= []
  (fn [graph change ?dependency]
    (logic/project [change]
      (logic/fresh [?map]
        (logic/== ?map (change-dependency graph change))
        (logic/project [?map]
          (logic/== ?dependency @?map)
          (logic/!= nil ?dependency))))))

(defn change=c> []
  (fn [graph listchange ?dependent]
    (logic/project [listchange]
      (logic/== true (list-operation? listchange))
      (logic/== ?dependent (change-child graph listchange)))))
     
(defn change=c< [] 
  (fn [graph listchange ?dependency]
    (logic/project [listchange]
      (logic/== true (list-operation? listchange))
      (logic/== ?dependency (change-dependency graph listchange)))))
    
;;Operations that work on changes that are already in a graph
(defn list-operation-parent-operation [graph x]
  (when (list-operation? x)
    (cons x (lazy-seq (list-operation-parent-operation (change-dependency graph x))))))

(defn list-operation-children [graph x]
  (when (list-operation? x)
    (cons x (lazy-seq (list-operation-children (change-child graph x))))))

(defn change-dependents-recursive [graph change]
  (let [deps (vals (change-dependents graph change))]
    (concat deps (mapcat change-dependents-recursive deps))))

(defmulti change-index-if-removed (fn [operation idx] (class operation)))
(defmethod change-index-if-removed :default [operation idx]
  idx)

(defmethod change-index-if-removed CListInsert [operation idx]
  (dec idx))

(defmethod change-index-if-removed CListMove [operation idx]
  (dec idx))

(defmethod change-index-if-removed CListDelete [operation idx]
  (inc idx))

(defn change-get-independent-index [graph operation]
  (let [idx (:index operation)]
    (when idx
      (reduce 
        (fn [idx op] (change-index-if-removed op idx))
        idx 
        (rest (list-operation-parent-operation graph operation))))))

;;Change Normalization
(defn change-remove [graph change]
  (defn change-remove-single-change [graph change]
    (let [parent (change-dependency graph change)
          new-graph (if parent
                     (change-remove-dependency graph parent change)
                     graph)
          new-graph' (assoc new-graph 
                       :changes (assoc (:changes new-graph) (:change-idx change) nil)
                       :dependency (assoc (:dependency new-graph) (:change-idx change) nil)
                       :dependents (assoc (:dependents new-graph) (:change-idx change) {}))]
      new-graph'))
  
  (defn change-remove-regular [graph change]
    (let [dependents (change-dependents-recursive graph change)
          new-graph (reduce (fn [g c]
                              (change-remove-single-change g c))
                      graph
                      (cons change dependents))
          new-graph' (assoc new-graph :roots (remove #{(:change-idx change)} (:roots new-graph)))]
      new-graph))
  
  (defn change-remove-list [graph change]
    (let [prev (change-dependency graph change)
          next (change-child graph change)
          new-graph (if next
                      (let [new-graph (change-add-dependency graph next prev)
                            new-graph' (reduce (fn [g c]
                                                 (let [new-change
                                                       (assoc c :index (change-index-if-removed change (:index c)))] ;;update index of c
                                                   (assoc g :changes (assoc (:changes g) (:graph-idx c) new-change))))
                                         new-graph
                                         (list-operation-children new-graph next))]
                        new-graph')
                      graph)
          new-graph' (if prev
                         (if (list-operation? prev)
                           (change-add-child graph prev next)
                           (change-add-dependency graph prev next)));;overwrites dependency between prev and change
          new-graph'' (assoc new-graph' :roots (remove #{(:change-idx change)} (:roots new-graph')))]
      (change-remove-regular new-graph'' (nth (:changes new-graph'') (:graph-idx change)))))
  (if (list-operation? change)
    (change-remove-list graph change)
    (change-remove-regular graph change)))


(defn insert-delete-redundant? [graph insert delete]
  (if-let [delete-original (:original delete)]
    (if-let [insert-parent (:original insert)]
      (let [delete-parent (.getParent delete-original)
            insert-left (get-node-idx (:right-parent insert) (:property insert) (:index insert))]
        (if (= delete-parent insert-parent)
          (if (ast/match? delete-original insert-left) ;;nodes look the same
            ;;lets verify that they are added at the same position
            (if (and (:index insert) (:index delete))
              (= (change-get-independent-index insert) (:index delete))
              false)
            false)
          false))
      false)
    false))


(defn insert-has-redundant-delete? [graph insert]
  (let [child (change-child graph insert)]
    (and 
      (delete? child)
      (= (change-get-independent-index insert) 
        (change-get-independent-index child))
      (ast/match? (:copy insert) (:original child)))))

(defn delete-has-redundant-insert? [graph delete]
  (let [child (change-child graph delete)]
    (and
      (insert? child)
      (= (change-get-independent-index delete) 
        (change-get-independent-index child))
      (ast/match? (:copy child) (:original delete)))))


(defn remove-redundant-operations [graph]
  (defn remove-redundant-operation [graph change]
    (let [next (change-child graph change)]
      (-> graph
        (change-remove next)
        (change-remove change))))
  (let [changes (:changes graph)
        list-operations (filter list-operation? changes)
        list-inserts (filter insert? list-operations)
        list-delete (filter delete? list-operations)
        redundant-inserts (filter insert-has-redundant-delete? list-inserts)
        redundant-deletes (filter delete-has-redundant-insert? list-delete)]
    (reduce remove-redundant-operation graph (concat redundant-deletes redundant-inserts))))



;;declarative graph generation or smthg like that
(defn graph-change|insert [graph ?insert]
  (el/contains (filter insert? (:changes graph)) ?insert))

(defn graph-change|move [graph ?move]
    (el/contains (filter move? (:changes graph)) ?move))

(defn graph-change|delete [graph ?delete]
  (el/contains (filter delete? (:changes graph)) ?delete))

(defn graph-change|update [graph ?update]
  (el/contains (filter update? (:changes graph)) ?update))


(defn change-type [?change ?type]
  (logic/project [?change]
    (logic/featurec ?change {:operation ?type})))

(defn change|move [change]
  (logic/all
    (change-type change :move)))

(defn change|update [change]
  (logic/all
    (change-type change :update)))

(defn change|insert [change]
  (logic/all
    (change-type change :insert)))

(defn change|delete [change]
  (logic/all
    (change-type change :delete)))

(defn change|list-operation [change]
  (logic/project [change]
    (logic/== true (list-operation? change))))


(defn change-original [change ?original]
  (logic/project [change]
    (logic/featurec change {:original ?original})))  

(defn change-copy [change ?copy]
  (logic/project [change]
    (logic/featurec change {:copy ?copy})))  


(defn change-leftparent [change ?leftparent]
  (logic/project [change]
    (logic/featurec change {:left-parent ?leftparent}))) 

(defn change-rightparent [change ?rightparent]
  (logic/project [change]
    (logic/featurec change {:right-parent ?rightparent}))) 

(defn change-property [change ?property]
  (logic/project [change]
    (logic/featurec change {:property ?property}))) 

(defn change-index [change ?idx]
  (logic/project [change]
    (logic/featurec change {:index ?idx})))

(defn graph-change-index|independent [graph change ?idx]
  (logic/project [graph change]
    (logic/== (change-get-independent-index graph change) ?idx)))

;;Matching
(defn graph-leftmatching [graph ?matching]
  (logic/project [graph]
    (logic/== ?matching (.getLeftMatching (:differencer graph)))))

(defn graph-rightmatching [graph ?matching]
  (logic/project [graph]
    (logic/== ?matching (.getRightMatching (:differencer graph)))))

(defn graph-leftmatchingprime [graph ?matching]
  (logic/project [graph]
    (logic/== ?matching (.getLeftMatchingPrime (:differencer graph)))))

(defn graph-rightmatchingprime [graph ?matching]
  (logic/project [graph]
    (logic/== ?matching (.getRightMatchingPrime (:differencer graph)))))

(defn ast-ast|matching [graph ?left ?right]
  (logic/project [graph]
    (logic/fresh [?matching]
      (logic/conda
        [(logic/lvaro ?left)
         (logic/conda
           [(logic/lvaro ?right)
            (graph-leftmatching graph ?matching)
            (logic/project [?matching]
              (el/contains (seq (keys ?matching)) ?left)
              (logic/project [?left]
                (logic/== ?right (get ?matching ?left))))]
           [;;right is bound
            (graph-rightmatching graph ?matching)
            (logic/project [?matching]
             (el/contains (seq (keys ?matching)) ?right)
             (logic/project [?right]
               (logic/== ?left (get ?matching ?right))))])]
        [;;left is bound
         (graph-leftmatching graph ?matching)
         (logic/project [?matching]
           (logic/project [?left]
             (logic/== ?right (get ?matching ?left))))]))))

(defn ast-ast|matching-prime [graph ?left ?right]
    (logic/project [graph]
    (logic/fresh [?matching]
      (logic/conda
        [(logic/lvaro ?left)
         (logic/conda
           [(logic/lvaro ?right)
            (graph-leftmatchingprime graph ?matching)
            (logic/project [?matching]
              (el/contains (seq (keys ?matching)) ?left)
              (logic/project [?left]
                (logic/== ?right (get ?matching ?left))))]
           [;;right is bound
            (graph-rightmatchingprime graph ?matching)
            (logic/project [?matching]
             (el/contains (seq (keys ?matching)) ?right)
             (logic/project [?right]
               (logic/== ?left (get ?matching ?right))))])]
        [;;left is bound
         (graph-leftmatchingprime graph ?matching)
         (logic/project [?matching]
           (logic/project [?left]
             (logic/== ?right (get ?matching ?left))))]))))


;;



      