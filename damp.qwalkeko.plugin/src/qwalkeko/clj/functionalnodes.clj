(ns qwalkeko.clj.functionalnodes
  (:import [changenodes.Differencer])
  (:import [changenodes.operations
            Delete Insert Move Update] )
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt.astnode :as astnode])
  (:require [damp.ekeko.logic :as el])
  (:require [qwalkeko.clj.ast :as ast])
  (:require [damp.qwal :as qwal])
  (:require [damp.ekeko.jdt
             [ast :as jdt]]))

;;;; This file converts Java Changes provided by ChangeNodes into a more structured representation.
;;;; This representation structures changes as a tree (even though it's called a graph everywhere)
;;;;  in which dependent changes are connected.
;;;; A dependency between changes can exist because 
;;;;  - a change is a child of another change (eg: Insert method and Insert expression in body of that method)
;;;;  - two changes modify the same ChildListProperty (eg: insert two expressions in a body at certain index i and j)
;;;; The latter changes can be applied independently from eachother, but indexes of 'later' changes must be changed
;;;;  if earlier changes have not been applied yet.

;;;; The algorithm works in different steps. First of all dependencies of the first kind are detected.
;;;; Second we link together changes that apply on the same list.
;;;; Finally we link together the head of that list with the corresponding change

;;;; The implementation is completely functional, and every operation returns a new graph object.
;;;; Changes are represented internally as numbers (accessed via :graph-idx). This also means that
;;;; indexes should always be converted to a concrete change against the current graph as old concrete changes
;;;; may no longer be correct. Indexes remain unique throughout the lifetime of the graph.

;;;; Most changes have a :original, :copy, :left-parent and :right-parent 

;;Clojure Representation of a Change
(defrecord CDelete  [operation original copy property left-parent index graph-idx]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CDelete. :delete nil nil nil nil nil nil)))

(defrecord CMove [operation original copy left-parent right-parent property index graph-idx prime-parent left-removed]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CMove. :move nil nil nil nil nil nil nil nil nil)))

(defrecord CUpdate [operation original copy left-parent right-parent property graph-idx]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CUpdate. :update nil nil nil nil nil nil)))

(defrecord CInsert [operation original copy left-parent right-parent property index graph-idx left-removed]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CInsert. :insert nil nil nil nil nil nil nil nil)))

(defrecord CListDelete  [operation original copy property left-parent index  dependency graph-idx]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CListDelete. :delete nil nil nil nil nil nil nil)))

(defrecord CListMove [operation original copy left-parent right-parent property index graph-idx prime-parent prime-idx prime-property]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (CListMove. :move nil nil nil nil nil nil nil nil nil nil)))

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
    (nth (seq (ast/has-clj-unwrapped property node)) idx)))

(defmulti convert-operation class)

(defmethod convert-operation Delete [operation]
  (let [idx (convert-index (.getIndex operation))
        m {:operation :delete
           :original (.getOriginal operation)
           :copy (.getAffectedNode operation)
           :left-parent (.getLeftParent operation)
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
           :copy (.getCopy operation)
           :left-parent (.getAffectedNode operation)
           :right-parent  (.getRightParent operation)
           :property prop
           :index idx
           :graph-idx nil
           :left-removed (.getLeftRemoved operation)}]
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
           :left-removed (.getLeftRemoved operation)
           :property prop
           :index idx
           :graph-idx nil
           :prime-parent (.getLeftPrimeParent operation)
           :prime-property (.getPrimeProperty operation)
           :prime-idx (convert-index (.getPrimeIndex operation))}]
    (if idx
      (map->CListMove m)
      (map->CMove m))))

(defmethod convert-operation Update [operation]
  (let [prop (convert-property (.getProperty operation))
        m {:operation :update
           :original (ast/has-clj prop (.getOriginal operation))
           :copy (ast/has-clj prop (.getLeftParent operation))
           :left-parent (.getLeftParent operation)
           :right-parent (.getRightParent operation)
           :property prop
           :graph-idx nil}]
    (map->CUpdate m)))




;;Moves of mandatory properties need to have a delete as well
(declare move? change-dependencies-idx-recursive)

;;Interface with Java Differencer
(defn make-differencer [left-ast right-ast]
  (new changenodes.Differencer left-ast right-ast))

(defn difference [differencer]
  (.difference differencer)
  differencer)

(defn get-operations [differencer]
  (seq (.getOperations differencer)))

(defn left-matching [differencer]
  (.getLeftMatching differencer))

(defn right-matching [differencer]
  (.getRightMatching differencer))

(defn get-java-changes [left right]
  (let [differencer (make-differencer left right)]
    (get-operations (difference differencer))))

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
(declare graph-order)
(defn changes->graph [changes]
  (let [size (count (:changes changes))]
    {:changes (vec (map-indexed (fn [idx c] (assoc c :graph-idx idx)) (:changes changes)))
     :dependencies (vec (take size (repeat (sorted-set))))
     :child (vec (take size (repeat nil)))
     :parent (vec (take size (repeat nil)))
     :differencer (:differencer changes)}))

(defn graph-change-idx [graph idx]
  "Retrieves correspending change"
  (when-not (nil? idx)
    (nth (:changes graph) idx)))
 
(defn change-dependencies-idx-recursive 
  ([graph change-idx]
    (change-dependencies-idx-recursive graph change-idx #{}))
  ([graph change-idx visited]
    (if (visited change-idx)
      #{}
      (let [deps (set (nth (:dependencies graph) change-idx))]
        (into deps
          (mapcat #(change-dependencies-idx-recursive graph % (conj visited change-idx)) deps))))))

(defn change-dependents-idx-recursive 
  ([graph change-idx]
    (change-dependents-idx-recursive graph change-idx #{}))
  ([graph change-idx visited]
    (if (visited change-idx)
      #{}
      (let [deps (set (nth (:dependents graph) change-idx))]
        (into deps
          (mapcat #(change-dependents-idx-recursive graph % (conj visited change-idx)) deps))))))
 
(defn change-child-idx [graph change]
  (nth (:child graph) (:graph-idx change)))

(defn change-child [graph change]
  (graph-change-idx graph 
    (change-child-idx graph change)))
    
(defn change-add-dependency [graph change dependent]
  (let [idx (:graph-idx change)
        dependencies (:dependencies graph)
        new-dependencies (update-in dependencies [(:graph-idx dependent)] conj idx)]
    (assoc graph 
      :dependencies new-dependencies)))

(defn change-remove-dependency [graph change old-dependent]
    (let [idx (:graph-idx change)
        dependencies (:dependencies graph)
        new-dependencies (update-in dependencies [(:graph-idx old-dependent)] disj idx)]
    (assoc graph 
      :dependencies new-dependencies)))

(defn change-add-parent-dependency [graph change parent]
  (let [parents (:parent graph)]
    (assoc graph
      :parent
      (assoc parents (:graph-idx change) (:graph-idx parent)))))
     
(defn change-add-child-dependency [graph change child]
  (let [children (:child graph)]
    (assoc graph
      :child
      (assoc children (:graph-idx change) (:graph-idx child)))))

(defn change-add-list-dependency [graph change parent]
  (-> graph
    (change-add-parent-dependency change parent)
    (change-add-child-dependency parent change)))

;;Methods used to set up the graph
(defmulti move-change-dependent? (fn [graph move change] (class change)))
(defmulti insert-change-dependent? (fn [graph insert change] (class change)))
(defmulti change-change-dependent?
   "Dependent depends on change. Only used during the creation of the dependency graph." 
   (fn [graph change dependent] (class change)))

(defn change-change-dependent-default? [graph change dep]
   (let [left-parent (:left-parent dep)
         mandatory-parents 
         (take-while #(not (nil? %))
           (iterate (fn [node]
                      (let [location (.getLocationInParent node)]
                       (if (and 
                             (not (nil? location))
                             (not (.isChildListProperty location))
                             (.isMandatory location))
                         (.getParent node)
                         nil)))
             (:left-parent dep)))]
    (or
      (= (:left-parent dep) (:copy change))
      (some #(= (:copy change) %) mandatory-parents))))

(defn insert-change-dependent-default? [graph insert change]
   (change-change-dependent-default? graph insert change))

(defmethod change-change-dependent? :default [graph change dep]
  false)

(defmethod change-change-dependent? CInsert [graph insert dep]
  (insert-change-dependent? graph insert dep))

(defmethod change-change-dependent? CListInsert [graph insert dep]
  (insert-change-dependent? graph insert dep))

(defmethod change-change-dependent? CMove [graph move dep]
  (move-change-dependent? graph move dep))

(defmethod change-change-dependent? CListMove [graph move dep]
  (move-change-dependent? graph move dep))

(defmethod insert-change-dependent? :default [graph insert change] 
  (insert-change-dependent-default? graph insert change))

(defmethod insert-change-dependent? CDelete [graph insert delete]
  false)

(defmethod insert-change-dependent? CInsert [graph insert depends]
  (and 
    (not= insert depends)
    (change-change-dependent-default? graph insert depends)))

(defmethod insert-change-dependent? CMove [graph insert move]
  (insert-change-dependent-default? graph insert move))

(defmethod insert-change-dependent? CListInsert [graph insert listinsert]
  (insert-change-dependent-default? graph insert listinsert))

(defmethod insert-change-dependent? CListMove [graph insert listmove]
  (insert-change-dependent-default? graph insert listmove))

(defmethod insert-change-dependent? CListDelete [graph insert listdelete]
  false)

;;Moves
(defn move-change-dependent-default? [graph move change]
  (change-change-dependent-default? graph move change))

(defmethod move-change-dependent? :default [graph move change]
  (move-change-dependent-default? graph move change))

;;deleting a node we later need is not recommended
(defmethod move-change-dependent? CDelete [graph move delete]
 (let [parents (take-while #(not (nil? %)) (iterate #(.getParent %) (:original move)))]
   (some #(= % (:original delete)) parents)))

(defmethod move-change-dependent? CMove [graph move depends]
  (and 
    (not= move depends)
    (or
      (move-change-dependent-default? graph move depends)
      (some #(= (:left-removed depends) %) ;;moves overwrites the part in which move resides
        (take-while #(not (nil? %)) 
          (iterate #(.getParent %) (:copy move))))
      (some #(= (:left-removed depends) %) ;;move removes the part in which move resides
        (take-while #(not (nil? %)) 
          (iterate #(.getParent %) (:prime-parent move))))
      (and 
        (nil? (:prime-parent move))
        (some #(= (:left-removed depends) %) 
          (take-while #(not (nil? %)) 
            (iterate #(.getParent %) (:copy move)))))
      (and 
        (= (:prime-parent move) (:left-parent depends)))))) ;;move is overwritten by depends, so move must be done first
        ;(= (:property move) (:property depends)))))) ;;probably not 100% correct
    
;;an insert can overwrite part of the AST that still needs to be used by a move
;;if that insert operates somewhere above the move
(defmethod move-change-dependent? CInsert [graph move insert]
 (let [insert-parent (when-let [original (:original insert)] (.getParent original))
        parents (take-while #(not (nil? %))
                  (iterate #(.getParent %) (:original move)))]
    (or
      (move-change-dependent-default? graph move insert)
      (some #(= (:left-removed insert) %) ;;insert removes the part in which move resides
        (take-while #(not (nil? %)) 
          (iterate #(.getParent %) (:prime-parent move))))
      (and 
        insert-parent
        (not (instance? CListMove move))
        (some #(= insert-parent %) parents)))))

(defmethod move-change-dependent? CListInsert [graph move insert]
  (move-change-dependent-default? graph move insert))

(defmethod move-change-dependent? CListMove [graph move listmove]
    (move-change-dependent-default? graph move listmove))

(defmethod move-change-dependent? CListDelete [graph move listdelete]
   (let [parents (take-while #(not (nil? %)) (iterate #(.getParent %) (:original move)))]
     (some #(= % (:original listdelete)) parents)))

(defn- change-link-direct-dependents [graph]
  "Links changes that directly depend on each other by comparing two changes pairwise"
  (let [changes (:changes graph)]
    (reduce
      (fn [graph c]
        (reduce
          (fn [graph dep]
            (if (and (not= dep c) (change-change-dependent? graph c dep))
              (change-add-dependency graph c dep)
              graph))
          graph
          changes))
      graph
      changes)))

(defn- link-list-dependents [graph]
  "links operations in the same ChildListProperty in a linked list (without deletes)"
  (defn link-group [graph group]
    ;;group is a list of nodes, at index i we find the parent of i+1 etc
    (let [parent-child (partition 2 1 group)] ;;we group together a parent and its child
      (reduce
        (fn [graph pc]
          (let [parent (first pc)
                child (second pc)]
            (change-add-list-dependency graph child parent)))
        graph
        parent-child)))
  (let [changes (:changes graph)
        listchanges (remove delete? (filter list-operation? changes))
        grouped (group-by :left-parent listchanges)]
    (reduce
      (fn [graph group]
        (link-group graph (sort-by :index group)))
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
          (let [gcurr (first group) ;;so we just add every item of the group in a weird recursion fashion
                new-graph (if-not (nil? prev)
                            (change-add-list-dependency graph gcurr prev)
                            graph)]
            (recur new-graph gcurr nil (rest group)))
          (let [gcurr (first group)
                gidx (:index gcurr)
                idx (:index current)]
            (if (< gidx idx)
              ;;the first of our group must be inserted here
              (let [current-added-graph
                    (change-add-list-dependency graph current gcurr)
                    prev-added-graph
                    (if-not (nil? prev)
                      (change-add-list-dependency current-added-graph gcurr prev)
                      current-added-graph)]
                (recur prev-added-graph gcurr current (rest group)))
              (recur graph current (change-child graph current) group))))
        graph))  
    (let [changes (:changes graph)
          group-root (.getParent (:original (first group)))
          group-property (:property (first group))
          list-root (first (filter #(and 
                                      (cond 
                                        (move? %)
                                        (= (:left-parent %) (:left-parent (first group)))
                                        :else
                                        (= (:original %) group-root))
                                        
                                      (= group-property (:property %)))
                             (filter list-operation? changes)))]
      (loop-and-add graph nil list-root group)))
  (let [changes (:changes graph)
        listdeletes (filter delete? (filter list-operation? changes))
        grouped (group-by #(.getParent (:original %)) listdeletes)]
    (reduce
      (fn [graph group] (link-delete-group graph (sort-by :index group)))
      graph
      (mapcat #(vals (group-by :property %)) (vals grouped)))))

(comment
  (defn- link-group-roots [graph]
    "Links the head of a list to the parent of all the elements in the list."
    (defn find-and-set-root [graph operation roots]
      (let [poss-roots (filter #(change-change-dependent-default? graph % operation) 
                         (filter #(not= operation %) roots))
            root (first poss-roots)]
        (if-not (nil? root)
          (change-add-dependency graph root operation)
          graph)))
    (let [changes (:changes graph)
          possible-roots (filter #(or (insert? %) (move? %)) changes)
          ;;deletes should never be inside an insert as the node shouldnt get inserted in the first place
          list-operations (remove delete? (filter list-operation? changes))
          ;;group them by property and set the root of every property list
          grouped-operations (group-by :left-parent list-operations)]
        (reduce
          (fn [g parented]
            (let [property (group-by :property parented)]
              (reduce
                (fn [g prop]
                  (let [res (first (sort-by :index prop))]
                    (find-and-set-root g res possible-roots)))
                g
                (vals property))))
          graph
          (vals grouped-operations)))))
 

(defn- add-roots-to-graph [graph]
  (let [dependencies (:dependencies graph)
        roots (filter #(empty? (nth dependencies %))
                (seq (range (graph-order graph))))]
    (assoc graph :roots roots)))

(defn remove-insert-move-cycles [graph]
  (defn replace-move [graph move inserts]
    (let [convertor (if (:index move) map->CListInsert map->CInsert)
          new-insert (convertor {:operation :insert
                                 :original (:original move) ;;perhaps make this nil...
                                 :copy (:copy move)
                                 :left-parent (:left-parent move)
                                 :right-parent (:right-parent move)
                                 :property (:property move)
                                 :index (:index move)
                                 :graph-idx (:graph-idx move)})
          dependencies (reduce (fn [deps i]
                                 (if (change-change-dependent? graph new-insert i) ;;i may still be dependent if it is not the main cause of the cycle
                                   deps
                                   (update-in deps [(:graph-idx i)] disj (:graph-idx move))))
                         (:dependencies graph)
                         inserts)
          new-changes (assoc (:changes graph) (:graph-idx move) new-insert)]
      (assoc graph :dependencies dependencies :changes new-changes)))
  ;;could be done by topological sorting but who cares about performance...
  (let [bad-moves (filter (fn [move]
                            ((change-dependencies-idx-recursive graph (:graph-idx move))
                              (:graph-idx move)))
                    (filter move? (:changes graph)))
        bad-inserts (filter (fn [insert]
                              ((change-dependencies-idx-recursive graph (:graph-idx insert))
                                (:graph-idx insert)))
                      (filter insert? (:changes graph)))]
    (reduce
      (fn [graph move]
        (let [move-deps (change-dependencies-idx-recursive graph (:graph-idx move))
              deps (filter (fn [insert]
                             (and
                               ((change-dependencies-idx-recursive graph (:graph-idx insert))
                                 (:graph-idx move))
                               (move-deps (:graph-idx insert))))
                     bad-inserts)]
        (if (empty? deps) ;;shouldnt be the case though
          graph
          (replace-move graph move deps))))
      graph
      bad-moves)))

(defn- remove-move-cycles [graph]
  (letfn [(move->insert [move]
          (let [m {:operation :insert
                   :graph-idx (:graph-idx move)
                   :original (:original move)
                   :copy (:copy move)
                   :left-parent (:left-parent move)
                   :right-parent (:right-parent move)
                   :property (:property move)
                   :index (:index move)
                   :left-removed (:left-removed move)}]
            (if (:index move)
              (map->CListInsert m)
              (map->CInsert m))))
          (fix-dependencies [graph new-insert]
            (let [old-dependencies (nth (:dependencies graph) (:graph-idx new-insert))]
              (reduce        
                (fn [graph dep]
                  (if (change-change-dependent? graph dep new-insert)
                    graph
                    (change-remove-dependency graph dep new-insert)))
                graph
                (map #(nth (:changes graph) %) old-dependencies))))
          (install-new-insert [graph new-insert]
            (let [changes (:changes graph)
                  new-changes (assoc (:changes graph) (:graph-idx new-insert) new-insert)]
              (assoc graph :changes new-changes)))
          (bad-move? [graph move]
            ((change-dependencies-idx-recursive graph (:graph-idx move))
              (:graph-idx move)))
          (fix-bad-move [graph move]
            (let [deps (change-dependencies-idx-recursive graph (:graph-idx move))
                  dep-moves (filter move? (map #(nth (:changes graph) %) deps))
                  dep-inserts (map move->insert dep-moves)
                  installed (reduce
                              (fn [graph insert]
                                (install-new-insert graph insert))
                              graph
                              dep-inserts)]
              (reduce
                (fn [graph insert]
                  (fix-dependencies graph insert))
                installed
                dep-inserts)))]
  (let [moves (filter move? (:changes graph))]
    (reduce
      (fn [graph move]
        (if (bad-move? graph move)
          (fix-bad-move graph move)
          graph))
      graph
      moves))))


(defn remove-cycles [graph]
  "Removes dependency cycles from the dependency graph, turning it into a DAG."
  ;There are several possibilities to have cycles:
  ;First, it is possible to have cycles by having an insert that overwrites a move
  ;while that move moves the node back into the insert. We can fix it by replacing that move with an insert.
  ;Second, a 'swap' may occur, in which move A overwrites the source of move B, and move B needs to go to move A.
  ;This may also happen in larger groups, in which A moves to B, B moves to C, and C moves to A.
  ;We can solve this by ordering these moves and replacing 1 of them by an Insert
    (-> graph
      remove-insert-move-cycles
      remove-move-cycles))

(defn fix-list-indices [graph]
  ;;listmoves result in unoutputted deletes
  ;;as a result some operations have an incorrect index
  ;;lets try to solve that
  (letfn [(fix-delete-index [graph delete moves]
          (let [lparent (:left-parent delete)
                candidate-moves (filter #(not (nil? (:prime-idx %))) 
                                  (filter #(= (:prime-property %) (:property delete))
                                    (filter #(= (:prime-parent %) lparent) moves))) ;;check property type
                sorted-moves (sort-by :prime-idx candidate-moves)
          ;      (map-indexed (fn [i el] (- (:prime-idx 
                dependent-moves (filter #(< (:prime-idx %) (:index delete)) candidate-moves)]
            (update-in graph [:changes]
              (fn [changes]
                (assoc changes (:graph-idx delete)
                  (assoc delete :index (- (:index delete) (count dependent-moves))))))))]
    (let [listmoves  (filter move? (:changes graph))
          deletes (filter list-operation? (:changes graph))]
      (reduce  #(fix-delete-index %1 %2 listmoves) graph deletes))))
    


(defn create-dependency-graph [changes]
  (let [graph (changes->graph changes)]
    (->
      graph
      ;fix-delete-indices
      change-link-direct-dependents
      link-list-dependents
      link-delete-dependents
      remove-cycles
      add-roots-to-graph)))

(defn ast-ast-graph [left right]
  (let [changes (get-ast-changes left right)]
    (assoc (create-dependency-graph changes) :differencer (:differencer changes))))

;;Graph helpers

(defn graph-order [graph]
  (count (:changes graph)))

(defn graph-change-parent [graph change]
  (let [parent (nth (:parent graph) (:graph-idx change))]
    (when parent
      (nth (:changes graph) parent))))

(defn graph-change-child [graph change]
  (let [child (nth (:child graph) (:graph-idx change))]
    (when child
      (nth (:changes graph) child))))

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


(defn ast-ast-changes [?left ?right ?changes]
  (logic/project [?left ?right]
    (logic/== ?changes (seq (get-ast-changes ?left ?right)))))