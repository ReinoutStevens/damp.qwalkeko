(ns qwalkeko.clj.graph
  (:require [qwalkeko.clj.reification :as r])
  (:require [clojure.core.logic :as logic]))


(defrecord Metaversion [jmetaversion successors predecessors]
  clojure.core.logic.protocols.IUninitialized
  (-uninitialized [_] (Metaversion. nil (atom '()) (atom '()))))

(defn metaversion [metaversion]
  (Metaversion. metaversion (atom '#{})  (atom #{})))

(defmethod clojure.core/print-method Metaversion [x writer]
  (.write writer (str "#<Metaversion-" (.getRevisionNumber (:jmetaversion x)) ">")))


(defn successors [version]
  @(:successors version))

(defn predecessors [version]
  @(:predecessors version))


(defn predecessors! [metaversion predecessors]
  (reset! (:predecessors metaversion) (set predecessors)))

(defn successors! [metaversion successors]
  (reset! (:successors metaversion) (set successors)))

(defn convert-to-graph [versions]
  (let [converted (zipmap versions (map #(metaversion %) versions))]
    (doall
      (map (fn [version]
             (let [c (get converted version)]
               (do
                 (successors! c
                              (map (fn [succ]
                                     (get converted succ))
                                   (r/successors version)))
                 (predecessors! c
                                (map (fn [pred]
                                       (get converted pred))
                                     (r/predecessors version))))))
           versions))
    converted))

(defn successoro [version succs]
  (logic/project [version]
                 (logic/== succs (seq (successors version)))))

(defn predecessoro [version preds]
  (logic/project [version]
                 (logic/== preds (seq (predecessors version)))))


(defrecord Graph [roots project successors predecessors versions]
  clojure.core.logic.protocols.IUninitialized
  (-uninitialized [_] (Graph. '() nil '() '() '())))

(defmethod clojure.core/print-method Graph [x writer]
  (.write writer (str "#<Graph-" (.getName (:project x)) ">")))

(defn convert-project-to-graph [meta-project]
  (let [roots (.getRoots meta-project)
        versions (.getVersions meta-project)
        converted (convert-to-graph versions)]
    (Graph. 
      (map #(get converted %) roots)
      meta-project
      successoro
      predecessoro
      (vals converted))))

(defn convert-model-to-graph [history-project-model]
  (convert-project-to-graph (first (.getMetaProjects (.getMetaProduct history-project-model)))))


(defn graph-project-name [a-graph]
  (.getName (:project a-graph)))

;;Retrieve information out of versions / graphs

(defn versions [graph]
  (:versions graph))

(defn roots [graph]
  (:roots graph))

(defn ensure-checkout [metaversion]
  (r/ensure-checkout (:jmetaversion metaversion)))

(defn ensure-delete [metaversion]
  (r/ensure-delete (:jmetaversion metaversion)))

(defn eclipse-project [metaversion]
  (r/eclipse-project (:jmetaversion metaversion)))

(defn file-infos [version]
  (r/file-infos (:jmetaversion version)))

(defn revision-number [version]
  (r/revision-number (:jmetaversion version)))

(defn date [version]
  (r/date (:jmetaversion version)))

(defn commit-message [version]
  (r/commit-message (:jmetaversion version)))

;;Graph filtering
(defn filter-graph [graph f]
  (defn add-predecessor! [v pred]
    (swap! (:predecessors v) conj pred))
  
  (defn filter-versions [versions]
    (let [filtered (filter f versions)]
      (zipmap filtered (map metaversion (map :jmetaversion filtered)))))
  
  (defn collect-successors [version filtered] ;;version is from the original graph
    (let [mapped (get filtered version) ;;get new metaversion
          succs (successors version) ;;get successors in old one
          non-filtered (remove #(contains? filtered %) succs) ;;contains successors that are filtered as well
          filtered-succs (map #(get filtered %) (filter #(contains? filtered %) succs))
          recursive-succs (mapcat #(collect-successors % filtered) non-filtered)
          result (set (concat filtered-succs recursive-succs))]
      (do
        (when mapped
          (doall
            (map #(add-predecessor! % mapped) result)))
        result)))
  
  (defn link-version [version filtered]
    (let [successors (collect-successors version filtered)
          mapped (get filtered version)]
      (do
        (successors! mapped successors))))
  
  (let [v (:versions graph)
        filtered (filter-versions v)
        vls (vals filtered)]
    (do
      (doall
        (map #(link-version % filtered) (keys filtered)))
      (assoc graph 
        :roots (filter #(empty? (predecessors %)) vls)
        :versions vls))))
      
  
             