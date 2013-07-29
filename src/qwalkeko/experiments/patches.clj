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


(defn find-corresponding-merges-of-versions [graph version-left version-right]
  (let [merging-left (filter r/was-merged (conj (all-successors graph version-left) version-left))
        merging-right (filter r/was-merged (conj (all-successors graph version-right) version-right))]
    (clojure.set/intersection (set merging-left) (set merging-right))))

(defn find-corresponding-merges [graph branching-version]
  (let [succ (r/successors branching-version)
        left (first succ)
        right (second succ)]
    (find-corresponding-merges-of-versions graph left right)))


(defn find-corresponding-merge [graph branching-version]
  (first (sort-by #(.getTime %1)
                  (find-corresponding-merges graph branching-version))))


(defn find-corresponding-merge-of-versions [graph version-left version-right]
  (first (sort-by #(.getTime %1)
                  (find-corresponding-merges-of-versions graph version-left version-right))))


(defn find-cochanged-file [graph branching-version]
  (let [first-merge (find-corresponding-merge graph branching-version)]
    (when (not (nil? first-merge))
      (let [succ (r/successors branching-version)
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
                                                           (r/edited-files curr))
                                            (logic/== changed-version curr))
                                (q/q=>+)))] ;;a + here as we do not want to include the merging version
        (logic/run* [cochanged left-version right-version]
                   (q/qwal right-limited-graph right first-merge []
                          (q/q=>*)
                          (q/qcurrent [curr]
                                      (logic/membero cochanged (r/edited-files curr))
                                      (logic/membero [cochanged left-version] changed-left)
                                      (logic/== right-version curr))
                          (q/q=>+)))))))