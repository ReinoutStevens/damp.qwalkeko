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


;;applying changes
(defn- link-sequences [left-seq copy-seq]
  (apply assoc {} (interleave left-seq copy-seq)))

(defn- link-asts [left right]
  (let [left-iter (new DepthFirstNodeIterator left)
        right-iter (new DepthFirstNodeIterator right)
        left-seq (iterator-seq left-iter)
        right-seq (iterator-seq right-iter)]
    (link-sequences left-seq right-seq)))

(defn graph-prepare-for-change [graph]
   (let [current-ast (first (:asts graph))
         new-ast (AST/newAST AST/JLS8)
         left-copy (ASTNode/copySubtree new-ast current-ast)
         mapped (link-asts current-ast left-copy)
         current-map (get (:ast-map graph) (.getAST current-ast))
         new-map (apply hash-map 
                   (mapcat 
                     (fn [l] [l (get mapped (get current-map l))]) (keys current-map)))]
     (-> graph
       (update-in [:asts] conj left-copy)
       (update-in [:ast-map] assoc new-ast new-map))))

(defn graph-corresponding-node [graph ast node]
  (let [ast-map (:ast-map graph)
        node-map (get ast-map ast)]
    (get node-map node)))

(defn graph-corresponding-node-latest-ast [graph node]
  (let [ast (.getAST (first (:asts graph)))]
    (graph-corresponding-node graph ast node)))

;;independent indexes
(defmulti graph-change-current-index (fn [graph change idx] (class change)))

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
  (let [new-ast (first (:asts graph))
        ast-map (get (:ast-map graph) (.getAST new-ast))
        hash-map (java.util.HashMap. ast-map)]
    (.apply jchange (java.util.HashMap.) hash-map)
    (-> graph 
      (assoc :ast-map (assoc (:ast-map graph) (.getAST new-ast) (into {} hash-map)))
      (assoc :current idx)
      (update-in [:applied] (fn [app] (assoc app idx true))))))

(defmulti change-apply (fn [graph change] (class change)))

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
        index (graph-change-current-index (changes/graph-change-parent graph change) (:index change))
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
        mnode (graph-corresponding-node-latest-ast new-graph lnode)
        mlparent (graph-corresponding-node-latest-ast new-graph lparent)
        new-move (new Move nil mnode mlparent lparent 
                   (astnode/node-property-descriptor-for-ekeko-keyword lparent prop)
                   -1)]
    (java-change-apply new-graph new-move (:graph-idx change))))

(defmethod change-apply qwalkeko.clj.functionalnodes.CListMove [graph change]
  (let [new-graph (graph-prepare-for-change graph)
        prop (:property change)
        lparent (:left-parent change)
        lnode (:copy change)
        mnode (graph-corresponding-node-latest-ast new-graph lnode)
        mlparent (graph-corresponding-node-latest-ast new-graph lparent)
        index  (graph-change-current-index (changes/graph-change-parent graph change) (:index change))
        new-move (new Move nil mnode mlparent lparent 
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


(defmulti graph-change-convert-to-ast (fn [graph change ast] (class change)))

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
  [left right differencer changes roots dependents dependency child ast-map asts applied current]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (NavigatableChangeGraph. nil nil nil nil nil nil nil nil nil nil nil nil)))

(defmethod print-method NavigatableChangeGraph [graph ^java.io.Writer w]
  (print-method (dissoc graph :ast-map :asts :changes) w))

(defn graph-to-navigatable-graph [graph left right]
  (let [diff (:differencer graph)
        copy-original (into {} (.getCopyToOriginal diff))
        ast-map {(.getAST left)  copy-original}
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
        :ast-map ast-map 
        :asts (list left) 
        :dependents (map seq dependents) ;;core.logic still does not like sets
        :left left :right right
        :applied (vec (repeat (count (:changes graph)) false))
        :current nil))))
        

(defn ast-ast-navigatable-graph [left right]
  (let [changes (changes/ast-ast-graph left right)
        navigatable (graph-to-navigatable-graph changes left right)]
    navigatable))


;;Qwal that shit
(defn graph-change-applied? [graph idx]
  (nth (:applied graph) idx))

(defn graph-next-roots [graph]
  (let [roots (:roots graph)
        unapplied (remove #(graph-change-applied? graph %) roots)]
    unapplied))

(defn graph-next-changes [graph]
  (let [unapplied (remove #(graph-change-applied? graph %) (range (changes/graph-order graph)))]
    (filter
      (fn [i]
        (every?
          (fn [c]
            (graph-change-applied? graph c))
          (nth (:dependencies graph) i)))
      unapplied)))

(defn change-> [_ current ?next]
  (logic/fresh [?changes ?change-idx ?change]
    (logic/== ?changes (graph-next-changes current))
    (logic/!= ?changes nil)
    (el/contains ?changes ?change-idx)
    (logic/project [?change-idx]
      (logic/== ?change (changes/graph-change-idx current ?change-idx))
      (logic/project [?change]
        (logic/== ?next (change-apply current ?change))))))

(defn change->* [_ current ?next]
  (logic/conde
    [(logic/== current ?next)]
    [(logic/fresh [?neext]
       (change-> _ current ?neext)
       (change->* _ ?neext ?next))]))

(defn change->+ [_ current ?next]
  (logic/fresh [?neext]
    (change-> _ current ?neext)
    (change->* _ ?neext ?next)))


(defn graph-node-node-ast-corresponding [graph original ?node ast]
  (logic/project [graph original ast]
    (logic/== ?node (graph-change-convert-to-ast graph original ast))
    (logic/!= ?node nil)))

(defmacro with-last-change [[current ast change] & goals]
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
  `(with-last-change [~current ~ast change# ] 
     ~@goals))

(defmacro step-changes [navigatable ?end [& bindings ] & goals]
  `(logic/fresh [graph#]
     (logic/== graph# ~navigatable)
     (damp.qwal/qwal graph# graph# ~?end [~@bindings]
                    	       ~@goals)))