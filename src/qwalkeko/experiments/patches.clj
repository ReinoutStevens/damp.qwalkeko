(ns qwalkeko.experiments.patches
  (:refer-clojure :exclude [== type])
  (:require [clojure.core.logic :as logic])
  (:require [damp.qwal :as q])
  (:require [qwalkeko.clj.logic :as l])
  (:require [qwalkeko.clj.reification :as r]))


(defn all-successors [graph a-version]
  (logic/run* [result]
              (q/qwal graph a-version result []
                      (q/q=>+))))


(defn find-branching-versions [graph a-root]
  (filter r/was-branched (all-successors graph a-root)))


(defn find-corresponding-merges [graph branching-version]
  (let [succ (r/successors branching-version)
        left (first succ)
        right (second succ)
        merging-left (filter r/was-merged (all-successors graph left))
        merging-right (filter r/was-merged (all-successors graph right))]
    (clojure.set/intersection (set merging-left) (set merging-right))))


(defn find-corresponding-merge [graph branching-version]
  (let [succ (r/successors branching-version)
        left (first succ)
        right (second succ)]
    (logic/run 1 [result]
                (q/qwal graph left result []
                        (q/q=>*)
                        (q/qcurrent [curr]
                                    (l/was-mergedo curr)))
                (q/qwal graph right result []
                        (q/q=>*)))))


(defn find-cochanged-file [graph branching-version]
  (let [merges (find-corresponding-merge graph branching-version)]
    (when (not (empty? merges))
      (let [first-merge (first merges)
            succ (r/successors branching-version)
            left (first succ)
            right (second succ)
            left-limited-graph (l/make-graph-between graph left first-merge)
            right-limited-graph (l/make-graph-between graph right first-merge)
            changed-left
            (logic/run* [changed-file changed-version]
                        (q/qwal left-limited-graph left first-merge []
                                (q/q=>*)
                                (q/qcurrent [curr]
                                            (logic/membero changed-file 
                                                           (r/changed-files curr))
                                            (logic/== changed-version curr))
                                (q/q=>+)))] ;;a + here as we do not want to include the merging version
        (logic/run* [cochanged changed-version-left changed-version-right]
                   (q/qwal right-limited-graph right first-merge []
                          (q/q=>*)
                          (q/qcurrent [curr]
                                      (logic/membero cochanged (r/changed-files curr))
                                      (logic/membero [cochanged changed-version-left] changed-left)
                                      (logic/changed-version-right curr))
                          (q/q=>+)))))))