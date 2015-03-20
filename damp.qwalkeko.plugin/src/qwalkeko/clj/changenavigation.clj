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
(defn link-sequences [left-seq copy-seq]
  (apply assoc {} (interleave left-seq copy-seq)))

(defn link-asts [left right]
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

(defn java-change-apply [graph jchange idx]
  (let [new-ast (first (:asts graph))
        ast-map (get (:ast-map graph) (.getAST new-ast))
        hash-map (java.util.HashMap. ast-map)]
    (.apply jchange (java.util.HashMap.) hash-map)
    (-> graph 
      (assoc :ast-map (assoc (:ast-map graph) new-ast (into {} hash-map)))
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
        index (:index change)
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
        new-move (new Move nil mnode mlparent lparent 
                   (astnode/node-property-descriptor-for-ekeko-keyword lparent prop)
                   (:index change))]
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


(defrecord NavigatableChangeGraph 
  [left right differencer changes roots dependents dependency child ast-map asts applied current]
  clojure.core.logic.protocols/IUninitialized
  (-uninitialized [_] (NavigatableChangeGraph. nil nil nil nil nil nil nil nil nil nil nil nil)))

(defmethod print-method NavigatableChangeGraph [graph ^java.io.Writer w]
  (print-method (dissoc graph :ast-map :asts) w))


(defn graph-to-navigatable-graph [graph left right]
  (let [diff (:differencer graph)
        copy-original (into {} (.getCopyToOriginal diff))
        ast-map {(.getAST left)  copy-original}]
    (map->NavigatableChangeGraph 
      (assoc graph :ast-map ast-map :asts (list left) :left left :right right
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

(defn vals* [m]
  (defn vals-recur [a m]
    (if (map? m)
      (reduce into a (map (fn [v] (vals-recur a v)) (vals m)))
      (conj a m)))
  (vals-recur '() m))


(defn graph-next-changes [graph]
  (let [curr (:current graph)]
    (if (nil? curr)
      (graph-next-roots graph)
      (let [next (nth (:dependents graph) curr)]
        (if-not (empty? next)
          (vals* next)
          (let [nextchild (nth (:child graph) curr)]
            (if nextchild
              (list nextchild)
              (graph-next-roots graph))))))))

(defn graph-hop-over-change [graph change]
  ;;change is the next change that needs to be applied
  ;;we will apply it and all of its dependent changes
  (let [dependents (vals (nth (:dependents graph) change))
        new-graph (change-apply graph (changes/graph-node-idx graph change))]
    (if (empty? dependents)
      new-graph
      (reduce (fn [g idx]
                (graph-hop-over-change g idx))
        new-graph
        dependents))))

(defn change-> [_ current ?next]
  (logic/fresh [?changes ?change-idx ?change]
    (logic/== ?changes (graph-next-changes current))
    (logic/!= ?changes nil)
    (el/contains ?changes ?change-idx)
    (logic/project [?change-idx]
      (logic/== ?change (changes/graph-node-idx current ?change-idx))
      (logic/project [?change]
        (logic/== ?next (change-apply current ?change))))))

(defn change!> [_ current ?next]
  (logic/fresh [?changes ?change-idx]
    (logic/== ?changes (graph-next-changes current))
    (logic/!= ?changes nil)
    (el/contains ?changes ?change-idx)
    (logic/project [?change-idx]
      (logic/== ?next (graph-hop-over-change current ?change-idx)))))


(defmacro in-current-change-state [[current ast] & goals]
  `(fn [graph# ~current next#]
     (logic/fresh [~ast]
       (logic/== ~ast (first (:asts ~current)))
       ~@goals
       (logic/== next# ~current))))

(defmacro step-changes [navigatable ?end [& bindings ] & goals]
  `(logic/fresh [graph#]
     (logic/== graph# ~navigatable)
     (damp.qwal/qwal graph# graph# ~?end [~@bindings]
                    	       ~@goals)))