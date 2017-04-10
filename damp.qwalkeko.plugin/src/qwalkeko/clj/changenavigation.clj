(ns qwalkeko.clj.changenavigation
   (:import [changenodes.operations
            Delete Insert Move Update] )
   (:import [changenodes.comparing
             DepthFirstNodeIterator])
   (:import [org.eclipse.jdt.core.dom
             AST ASTNode])
   (:require [damp.ekeko.logic :as el])
   (:require [clojure.core.logic :as logic])
   (:require [qwalkeko.clj.functionalnodes :as changes])
   (:require [damp.ekeko.jdt.astnode :as astnode])
   (:require [damp.ekeko.jdt
              [ast :as jdt]]))

;;File implementing the intermediate AST graph using a dependency graph
;;Nodes are constructed on the fly when required
;;The implementation is completely functional, so new objects are always returned

;;Currently, the notion of graphs and nodes are mixed
;;A node is represented by the complete graph: it contains the path of applied changes and the ASTs along that path
;;This way we retrieve the AST corresponding with that node

;Easy improvements
;; When two permutations of the same subsequence (A->B-C, C->B->A) are applied they should end up in the intermediate state
;; currently we construct the same AST twice

;Terminology
;; IAG -> Intermediate AST Graph, equivalent to ES in the paper

;Graph information
;; We have a functional implementation, meaning whenever we apply a change we create a new graph
;; This graph has an additional ES that is the result of applying that change
;; A graph may not be the best name as it is more a single path through the conceptual ESG

;Change Application
;;Changes are represented using nodes from either source, source' or target
;;We need a way to know to what nodes of the current AST the source' nodes used in changes correspond with
;;Initially we have a hashmap that starts with the original mapping going from source' to source, provided by changenodes
;;Whenever we apply a change we create a copy of the current AST and its mapping, and update the mapping based on the effect
;;of the change. This update is performed by changenodes itself as changes take this mapping as an argument

; Variables (written 1year of writing the code, not 100% sure :p )
;; asts -> a list of ASTs that contain the different intermediate ASTs of the path. The first element is the latest AST
;; applied -> vector with booleans stating which changes have been applied (I guess a list with the order may be better)
;; prime-to-int-map -> a sequence of maps of source' to intermediate ASTs. Same ordering as asts field
;; dependencies -> vector that models the dependencies of change i 
;;    at index i we find all the changes that must be applied before we can apply change i
;; dependents -> inverse of dependencies
;;    at index i we find all the changes that cannot be applied before change i is applied
;; parents -> vector containing the list dependencies
;; roots -> changes without any dependencies

;;applying changes
(defn- link-sequences [left-seq copy-seq]
  (apply assoc {} (interleave left-seq copy-seq)))

(defn- link-asts [left right]
  "Creates a map that maps nodes in left AST to right AST.
   Assumes both ASTs are equal."
  (let [left-iter (new DepthFirstNodeIterator left)
        right-iter (new DepthFirstNodeIterator right)
        left-seq (iterator-seq left-iter)
        right-seq (iterator-seq right-iter)]
    (link-sequences left-seq right-seq)))

(defn graph-prepare-for-change [graph]
  "Prepares an IAG node for a new change by copying the latest AST
   and extending the current maps. Returns a new graph."
   (let [current-ast (first (:asts graph))
         new-ast (AST/newAST AST/JLS8)
         left-copy (ASTNode/copySubtree new-ast current-ast)
         mapped (link-asts current-ast left-copy)
         current-map (get (:prime-to-int-map graph) (.getAST current-ast))
         new-map (apply hash-map 
                   (mapcat 
                     (fn [l] [l (get mapped (get current-map l))]) (keys current-map)))
         new-list {:ast (.getAST current-ast) :map mapped}]
     (-> graph
       (update-in [:asts] conj left-copy)
       (update-in [:int-to-int-list] conj new-list)
       (update-in [:prime-to-int-map] assoc new-ast new-map))))

(defn graph-corresponding-node [graph ast node]
  (let [ast-map (:prime-to-int-map graph)
        node-map (get ast-map ast)]
    (get node-map node)))

(defn graph-corresponding-node-latest-ast [graph node]
  "Returns the current representation of ASTnode from source' in the latest ast of graph.
   This is used to apply changes on an intermediate AST"
  (let [ast (.getAST (first (:asts graph)))]
    (graph-corresponding-node graph ast node)))

;;independent indexes
(defmulti graph-change-current-index  
  "Computes the index of a listchange depending whether previous listchanges have been applied"
  (fn [graph change idx] (class change)))

(defmethod graph-change-current-index :default [graph change idx]
  idx)

(defmethod graph-change-current-index qwalkeko.clj.functionalnodes.CListInsert [graph change idx]
  (let [parent (changes/graph-change-parent graph change)
        new-idx (if (nth (:applied graph) (:graph-idx change)) idx (- idx 1))]
    (graph-change-current-index graph parent new-idx)))

(defmethod graph-change-current-index qwalkeko.clj.functionalnodes.CListMove [graph change idx]
  (let [parent (changes/graph-change-parent graph change)
        new-idx (if (nth (:applied graph) (:graph-idx change)) idx (- idx 1))]
    (graph-change-current-index graph parent new-idx)))


(defmethod graph-change-current-index qwalkeko.clj.functionalnodes.CListDelete [graph change idx]
  (let [parent (changes/graph-change-parent graph change)
        new-idx (if (nth (:applied graph) (:graph-idx change)) idx (+ idx 1))]
    (graph-change-current-index graph parent new-idx)))


(defn java-change-apply [graph jchange idx]
  "applies a java change on graph. Idx is the index of the corresponding clojure change"
  (let [new-ast (first (:asts graph))
        ast-map (get (:prime-to-int-map graph) (.getAST new-ast))
        int-list (:map (first (:int-to-int-list graph)))
        hash-map (java.util.HashMap. ast-map)]
    (.apply jchange (java.util.HashMap.) hash-map)
    (-> graph 
      (assoc :prime-to-int-map (assoc (:prime-to-int-map graph) (.getAST new-ast) (into {} hash-map)))
      (assoc :current idx)
      (update-in [:applied] (fn [app] (assoc app idx true))))))

(defmulti change-apply 
  "applies a clojure change to the latest ast of graph"
  (fn [graph change] (class change)))

(defmethod change-apply qwalkeko.clj.functionalnodes.CInsert [graph change]
  (let [new-graph (graph-prepare-for-change graph)
        prop (:property change)
        lparent (:left-parent change)
        lnode (:copy change)
        mlparent (graph-corresponding-node-latest-ast new-graph lparent)
        new-insert (new Insert nil mlparent lparent 
                     lnode
                     (astnode/node-property-descriptor-for-ekeko-keyword lparent prop)
                     -1)]
    (java-change-apply new-graph new-insert (:graph-idx change))))

(defmethod change-apply qwalkeko.clj.functionalnodes.CListInsert [graph change]
  (let [new-graph (graph-prepare-for-change graph)
        prop (:property change)
        lparent (:left-parent change)
        lnode (:copy change)
        mlparent (graph-corresponding-node-latest-ast new-graph lparent)
        index (graph-change-current-index graph (changes/graph-change-parent graph change) (:index change))
        new-insert (new Insert nil mlparent lparent 
                     lnode
                     (astnode/node-property-descriptor-for-ekeko-keyword lparent prop)
                     index)]
    (java-change-apply new-graph new-insert (:graph-idx change))))

(defmethod change-apply qwalkeko.clj.functionalnodes.CMove [graph change]
  (let [new-graph (graph-prepare-for-change graph)
        prop (:property change)
        lparent (:left-parent change)
        lnode (:copy change)
        rnode (qwalkeko.clj.ast/has-clj-unwrapped prop lparent)
        mnode (graph-corresponding-node-latest-ast new-graph lnode)
        mlparent (graph-corresponding-node-latest-ast new-graph lparent)
        new-move (new Move nil mnode mlparent rnode 
                   (astnode/node-property-descriptor-for-ekeko-keyword lparent prop)
                   -1)]
    (java-change-apply new-graph new-move (:graph-idx change))))

(defmethod change-apply qwalkeko.clj.functionalnodes.CListMove [graph change]
  (defn number-of-deletes []
    (let [parents (take-while #(not (nil? %)) (iterate #(nth (:parent graph) %) 
                                                (nth (:parent graph) (:graph-idx change))))
          changes (map #(nth (:changes graph) %) parents)
          deletes (filter #(= (:operation %) :delete) changes)]
      (count deletes)))
  (let [new-graph (graph-prepare-for-change graph)
        prop (:property change)
        lparent (:left-parent change)
        lnode (:copy change)
        mnode (graph-corresponding-node-latest-ast new-graph lnode)
        ;rnode (nth (seq (qwalkeko.clj.ast/has-clj-unwrapped prop lparent)) (- (:index change) (number-of-deletes)))
        rnode (:copy change) ;(nth (.indexOf (seq (qwalkeko.clj.ast/has-clj-unwrapped prop lparent)) (:copy change)))
        mlparent (graph-corresponding-node-latest-ast new-graph lparent)
        index  (graph-change-current-index graph (changes/graph-change-parent graph change) (:index change))
        new-move (new Move nil mnode mlparent rnode 
                   (astnode/node-property-descriptor-for-ekeko-keyword lparent prop)
                   index)]
    (java-change-apply new-graph new-move (:graph-idx change))))


(defmethod change-apply qwalkeko.clj.functionalnodes.CDelete [graph change]
    (let [new-graph (graph-prepare-for-change graph)
          prop (:property change)
          lnode (:copy change)
          mlnode (graph-corresponding-node-latest-ast new-graph lnode)
          new-delete (new Delete nil mlnode)]
      (java-change-apply new-graph new-delete (:graph-idx change))))

(defmethod change-apply qwalkeko.clj.functionalnodes.CListDelete [graph change]
    (let [new-graph (graph-prepare-for-change graph)
          prop (:property change)
          lnode (:copy change)
          mlnode (graph-corresponding-node-latest-ast new-graph lnode)
          new-delete (new Delete nil mlnode)]
      (java-change-apply new-graph new-delete (:graph-idx change))))

(defmethod change-apply qwalkeko.clj.functionalnodes.CUpdate [graph change]
  (let [new-graph (graph-prepare-for-change graph)
        prop (:property change)
        lparent (:left-parent change)
        mlparent (graph-corresponding-node-latest-ast new-graph lparent)
        new-update (new Update nil mlparent lparent
                     (astnode/node-property-descriptor-for-ekeko-keyword lparent prop))]
    (java-change-apply new-graph new-update (:graph-idx change))))



(defmulti graph-change-convert-to-ast 
  "Converts a change so it can be applied on a given AST.
   Assumes all of the change dependencies are resolved."
  (fn [graph change ast] (class change)))

(defmethod graph-change-convert-to-ast :default [graph change ast]
  nil)
(defmethod graph-change-convert-to-ast qwalkeko.clj.functionalnodes.CDelete [graph change ast]
  (update-in change [:copy] #(graph-corresponding-node graph ast %)))

(defmethod graph-change-convert-to-ast qwalkeko.clj.functionalnodes.CMove [graph change ast]
  (-> change
    (update-in [:copy] #(graph-corresponding-node graph ast %))
    (update-in [:left-parent] #(graph-corresponding-node graph ast %))))

(defmethod graph-change-convert-to-ast qwalkeko.clj.functionalnodes.CUpdate [graph change ast]
  (-> change
    (update-in [:left-parent] #(graph-corresponding-node graph ast %))))

(defmethod graph-change-convert-to-ast qwalkeko.clj.functionalnodes.CInsert [graph change ast]
  (-> change
    (update-in [:copy] #(graph-corresponding-node graph ast %))
    (update-in [:left-parent] #(graph-corresponding-node graph ast %))))

(defmethod graph-change-convert-to-ast qwalkeko.clj.functionalnodes.CListDelete [graph change ast]
  (update-in change [:copy] #(graph-corresponding-node graph ast %)))

(defmethod graph-change-convert-to-ast qwalkeko.clj.functionalnodes.CListMove [graph change ast]
  (-> change
    (update-in [:copy] #(graph-corresponding-node graph ast %))
    (update-in [:left-parent] #(graph-corresponding-node graph ast %))))

(defmethod graph-change-convert-to-ast qwalkeko.clj.functionalnodes.CListInsert [graph change ast]
  (-> change
    (update-in [:copy] #(graph-corresponding-node graph ast %))
    (update-in [:left-parent] #(graph-corresponding-node graph ast %))))

;;Navigatable Graph
(defrecord NavigatableChangeGraph 
  [left right differencer changes roots dependents dependencies child int-to-int-list prime-to-int-map asts applied current]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (NavigatableChangeGraph. nil nil nil nil nil nil nil nil nil nil nil nil nil)))

(defmethod print-method NavigatableChangeGraph [graph ^java.io.Writer w]
  (print-method (dissoc graph :int-to-int-list :prime-to-int-map :asts :changes) w))

(defn graph-to-navigatable-graph [graph left right]
  (let [diff (:differencer graph)
        copy-original (into {} (.getCopyToOriginal diff))
        original-copy (clojure.set/map-invert copy-original)
        prime-int-map {(.getAST left)  copy-original}
        int-prime-list '()
        dependents (reduce 
                     (fn [res i] 
                       (let [deps (nth (:dependencies graph) i)]
                         (reduce
                           (fn [res d]
                             (update-in res [d]
                               conj i))
                           res deps)))                       
                     (vec (repeat (changes/graph-order graph) #{}))
                     (range (changes/graph-order graph)))]
    (map->NavigatableChangeGraph 
      (assoc graph 
        :dependencies (map seq (:dependencies graph)) ;;core.logic does not like sets
        :prime-to-int-map prime-int-map
        :int-to-int-list int-prime-list
        :asts (list left) 
        :dependents (map seq dependents) ;;core.logic still does not like sets
        :left left :right right
        :applied (vec (repeat (count (:changes graph)) false))
        :current nil))))
        

(defn ast-ast-navigatable-graph [left right]
  (let [changes (changes/ast-ast-graph left right)
        navigatable (graph-to-navigatable-graph changes left right)]
    navigatable))


(defn graph-change-dependencies-recursive [graph idx]
  (letfn [(with-visited [visited idx]
            (if (some #{idx} visited)
              visited
              (let [deps (nth (:dependencies graph) idx)
                    new-visited (conj visited idx)]
                (reduce
                  with-visited
                  new-visited
                  deps))))]
    (disj (with-visited #{} idx) idx)))


(defn graph-change-dependents-recursive [graph idx]
  (letfn [(with-visited [visited idx]
            (if (some #{idx} visited)
              visited
              (let [deps (nth (:dependents graph) idx)
                    new-visited (conj visited idx)]
                (reduce
                  with-visited
                  new-visited
                  deps))))]
    (disj (with-visited #{} idx) idx)))

;;Qwal that shit
(defn graph-change-applied? [graph idx]
  (nth (:applied graph) idx))

(defn graph-next-roots [graph]
  (let [roots (:roots graph)
        unapplied (remove #(graph-change-applied? graph %) roots)]
    unapplied))


(defn graph-change-dependencies-applied? [graph idx]
  (every?
    (fn [c]
      (graph-change-applied? graph c))
    (nth (:dependencies graph) idx)))

(defn graph-next-changes [graph]
  (let [unapplied (remove #(graph-change-applied? graph %) (range (changes/graph-order graph)))]
    (seq
      (filter
        (fn [i]
          (graph-change-dependencies-applied? graph i))
        unapplied))))

(defn change-dependencies-apply-alt [graph change]
  (defn apply-dependents [g deps]
    (if (empty? deps)
      g
      (recur (change-apply g (first deps)) (rest deps))))
  (let [new-graph (if (graph-change-applied? graph (:graph-idx change))
                    graph
                    (change-apply graph change))
        deps (map #(nth (:changes new-graph) %)
               (filter #(graph-change-dependencies-applied? new-graph %)
                 (remove #(graph-change-applied? new-graph %)
                   (nth (:dependents new-graph) (:graph-idx change)))))
        new-new-graph (apply-dependents new-graph deps)]
    (reduce
      change-dependencies-apply-alt
      new-new-graph
      deps)))


(defn change-dependencies-apply [graph change]
  (let [new-graph (if (graph-change-applied? graph (:graph-idx change))
                    graph
                    (change-apply graph change))
        dependents (filter #(graph-change-dependencies-applied? new-graph %)
                     (nth (:dependents new-graph) (:graph-idx change)))]
    (reduce
     change-dependencies-apply 
     new-graph
     (map #(nth (:changes new-graph) %) dependents))))

(defn change-> 
  "applies a single change on current"
  [_ current ?next]
  (logic/project [current]
    (logic/fresh [?changes ?change-idx ?change]
      (logic/== ?changes (graph-next-changes current))
      (logic/!= ?changes nil)
      (el/contains ?changes ?change-idx)
      (logic/project [?change-idx]
        (logic/== ?change (changes/graph-change-idx current ?change-idx))
        (logic/project [?change]
          (logic/== ?next (change-apply current ?change)))))))

(defn change->? [_ current ?next]
  "applies zero or 1 changes on current"
  (logic/conde
    [(logic/== current ?next)]
    [(change-> _ current ?next)]))

(defn change->* [g current ?next]
  "applies an arbitrary, including zero, changes on current"
  (logic/conde
    [(logic/== current ?next)]
    [(logic/fresh [?neext]
       (change-> g current ?neext)
       (change->* g ?neext ?next))]))

(defn change->+ [_ current ?next]
  "applies an arbitrary, non-zero, changes on current"
  (logic/fresh [?neext]
    (change-> _ current ?neext)
    (change->* _ ?neext ?next)))


;;some more coarse-grained operators
(defn change==>
  "applies a change and all its dependents on current"
  [_ current ?next]
  (logic/project [current]
    (logic/fresh [?changes ?change-idx ?change]
      (logic/== ?changes (graph-next-changes current))
      (logic/!= ?changes nil)
      (el/contains ?changes ?change-idx)
      (logic/project [?change-idx]
            (logic/== ?change (changes/graph-change-idx current ?change-idx))
            (logic/project [?change]
              (logic/== ?next (change-dependencies-apply-alt current ?change)))))))


(defn change==>* [g current ?next]
  "applies a change and all of its dependents an arbitry number of times"
  (logic/conde
    [(logic/== current ?next)]
    [(logic/fresh [?neext]
       (change==> g current ?neext)
       (change==>* g ?neext ?next))]))

(defn change==>+ [g current ?next]
  "applies a change and all of its dependents an arbitry number of times"
  (logic/fresh [?neext]
    (change==> g current ?neext)
    (change==>* g ?neext ?next)))

(defn change!=>
  "applies a change and all its dependents on current"
  [_ current ?next]
  (logic/project [current]
    (logic/fresh [?changes ?change-idx ?change]
      (logic/== ?changes (graph-next-changes current))
      (logic/!= ?changes nil)
      (logic/onceo (el/contains ?changes ?change-idx))
      (logic/project [?change-idx]
            (logic/== ?change (changes/graph-change-idx current ?change-idx))
            (logic/project [?change]
              (logic/== ?next (change-dependencies-apply-alt current ?change)))))))


(defn change!=>* [g current ?next]
  "applies a change and all of its dependents an arbitry number of times"
  (logic/conde
    [(logic/== current ?next)]
    [(logic/fresh [?neext]
       (change!=> g current ?neext)
       (change!=>* g ?neext ?next))]))

(defn change!=>+ [g current ?next]
  "applies a change and all of its dependents an arbitry number of times"
  (logic/fresh [?neext]
    (change!=> g current ?neext)
    (change!=>* g ?neext ?next)))

(defn change-limit=> [limit]
  (letfn [(limit-inner [g current ?next]
            (logic/conda
              [(logic/== true (< limit (count (filter true? (:applied current)))))
               logic/fail]
              [logic/succeed
               (logic/conde
                 [(logic/fresh [?neext]
                    (change==> g current ?neext)
                    (limit-inner g ?neext ?next))]
                 [(logic/== current ?next)])]))]
    limit-inner))
      
(defn change-limit-> [limit]
  (letfn [(limit-inner [g current ?next]
            (logic/conda
              [(logic/== true (< limit (count (filter true? (:applied current)))))
               logic/fail]
              [logic/succeed
               (logic/conde
                 [(logic/fresh [?neext]
                    (change-> g current ?neext)
                    (limit-inner g ?neext ?next))]
                 [(logic/== current ?next)])]))]
  limit-inner))
      
(defn change-sol-> [solution]
  (letfn [(do-solution-magic [graph changes]
            (let [real-changes (map #(nth (:changes graph) %) changes)]
              (reduce
                change-apply
                graph
                real-changes)))]
    (fn [graph current next]
      (logic/project [solution]
        (logic/== next (do-solution-magic current solution))))))

(defn graph-node-tracked [graph node]
  "finds the corresponding node of node (from a previous AST) in the current AST of graph"
  (let [ast (.getAST node)
        map (take-while #(not= (:ast %) ast) (:int-to-int-list graph))
        reversed (rest (reverse map))]
    (reduce
      (fn [n l]
        (let [head (first l)]
          (get (:map head) n)))
      node
      reversed)))


(defn graph-node-node|tracked [graph node ?corresponding]
  "Finds the representation of a node retrieved in a previous ias in the current ast of graph"
  (logic/project [graph node]
    (logic/== ?corresponding (graph-node-tracked graph node))))

(defmacro with-last-change [[current ast change] & goals]
  "Binds current to current node, ast to the ast of that node and change to the last applied change.
   Evaluates goals."
  `(fn [graph# ~current next#]
     (logic/fresh [~ast ~change]
       (logic/== ~ast (first (:asts ~current)))
       (logic/== ~change 
         (graph-change-convert-to-ast ~current
           (.getAST (first (:asts ~current))) 
           (changes/graph-change-idx ~current (:current ~current))))
       ~@goals
       (logic/== next# ~current))))

(defmacro in-current-change-state [[current ast] & goals]
  "Same as with-last-change, except change is not exposed to the user."
  `(with-last-change [~current ~ast change#] 
     ~@goals))

(defmacro step-changes [navigatable ?end [& bindings ] & goals]
  "Launches a Qwal Query over an IAG."
  `(logic/fresh [graph#]
     (logic/== graph# ~navigatable)
     (damp.qwal/qwal graph# graph# ~?end [~@bindings]
                    	       ~@goals)))


(defn sorted-solutions [solutions]
  (sort-by #(count (:applied %)) < solutions))