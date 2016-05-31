(ns qwalkeko.clj.graph-algo
  (:use clojure.set)
  (:require [qwalkeko.clj.functionalnodes :as changes]))

;;Adapted from http://rosettacode.org/wiki/Topological_sort#Clojure

(defn dep
  "Constructs a single-key dependence, represented as a map from
   item to a set of items, ensuring that item is not in the set."
  [item items]
  {item (difference (set items) (list item))})
 
(defn empty-dep
  "Constructs a single-key dependence from item to an empty set."
  [item]
  (dep item '()))
 
(defn pair-dep
  "Invokes dep after destructuring item and items from the argument."
  [[item items]]
  (dep item items))
 
(defn default-deps
  "Constructs a default dependence map taking every item
   in the argument to an empty set"
  [items]
  (apply merge-with union (map empty-dep (flatten items))))
 
(defn declared-deps
  "Constructs a dependence map from a list containaining
   alternating items and list of their predecessor items."
  [items]
  (apply merge-with union (map pair-dep (partition 2 items))))
 
(defn deps
  "Constructs a full dependence map containing both explicitly
   represented dependences and default empty dependences for
   items without explicit predecessors."
  [items]
  (merge (default-deps items) (declared-deps items)))
 
(defn no-dep-items
  "Returns all keys from the argument which have no (i.e. empty) dependences."
  [deps]
  (filter #(empty? (deps %)) (keys deps)))
 
(defn remove-items
  "Returns a dependence map with the specified items removed from keys
   and from all dependence sets of remaining keys."
  [deps items]
  (let [items-to-remove (set items)
        remaining-keys  (difference (set (keys deps)) items-to-remove)
        remaining-deps  (fn [x] (dep x (difference (deps x) items-to-remove)))]
    (apply merge (map remaining-deps remaining-keys))))
 
(defn topo-sort-deps
  "Given a dependence map, returns either a list of items in which each item
   follows all of its predecessors, or a string showing the items among which
   there is a cyclic dependence preventing a linear order."
  [deps]
  (loop [remaining-deps deps
         result         '()]
    (if (empty? remaining-deps)
        (reverse result)
        (let [ready-items (no-dep-items remaining-deps)]
          (if (empty? ready-items)
              (str "ERROR: cycles remain among " (keys remaining-deps))
              (recur (remove-items remaining-deps ready-items)
                     (concat ready-items result)))))))
 
(defn topo-sort-items
  "Given a list of alternating items and predecessor lists, constructs a
   full dependence map and then applies topo-sort-deps to that map."
  [items]
  (topo-sort-deps (deps items)))


(defn topo-sort-graph [graph]
  (let [keys (range (changes/graph-order graph))]
    (topo-sort-items
      (interleave
        keys
        (map
          (fn [key]
            (let [r (seq (nth (:dependencies graph) key))]
              (if (nil? r) '() r)))
          keys)))))


(defn paths [dag]
  (defn value-for [idx v]
    (let [incoming (nth (:dependencies dag) idx)
          vs (conj (map (fn [i] (nth v i)) incoming) 0)]
      (assoc v idx (inc (apply max vs)))))
  (let [topo (topo-sort-graph dag)
        v (vec (take (changes/graph-order dag) (repeat 0)))]
  (reduce 
    (fn [v i]
      (value-for i v))
    v
    topo)))

(defn longest-path [dag]
  (apply max (paths dag)))
 

(defn extend-solution [graph solution]
  (let [not-applied-changes (remove (fn [x] (some #{x} solution)) (range (count (:changes graph))))
        no-dependencies (filter (fn [c]
                                  (clojure.set/subset?
                                    (set (nth (:dependencies graph) c))
                                    solution))
                          not-applied-changes)]
    (map #(conj solution %) no-dependencies)))

(defn comb [k added graph solutions]
  (cond
    (= k 0) solutions
    (= added 0) (recur (dec k) (inc added) graph (map set (map vector (:roots graph))))
    :else 
    (let
      [new-solutions (mapcat #(extend-solution graph %) solutions)
       unique (distinct new-solutions)]
      (recur (dec k) (inc added) graph unique))))


(defn all-subset-changes [nav-graph]
  (apply concat
         (for [x (range 1 (inc (count (:changes nav-graph))))]
           (map #(into #{} %) (comb x 0 nav-graph nil)))))


(defn solution-ordered [graph solution]
  (let [top-sort (topo-sort-graph graph)
        changes (filter (fn [x] (some #{x} solution)) top-sort)]
    changes))
