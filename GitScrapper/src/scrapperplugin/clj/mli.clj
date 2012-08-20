(ns scrapperplugin.clj.mli
  (:import [scrapperplugin HistoryProjectModel])
  (:use [damp.ekeko ekekomodel]))


(defn history-project-models []
  (filter (fn [project-model]
            (instance? HistoryProjectModel project-model))
       (queried-project-models)))

