(ns qwalkeko.experiments.refactorings-japan
  (:require [clojure.core.logic :as logic])
  (:require [qwalkeko.clj.logic :as l])
  (:require [qwalkeko.clj.reification :as r])
  (:require [qwalkeko.clj.graph :as graph])
  (:require [qwalkeko.clj.ast :as ast])
  (:require [qwalkeko.clj.changenodes :as change])
  (:require [damp.ekeko.jdt
             [ast :as jdt]
             [convenience :as conv]])
  (:require [damp.qwal :as qwal]))





(def a-model (first (damp.ekeko.ekekomodel/all-project-models)))
(def a-graph (graph/convert-model-to-graph a-model))
(def a-root (first (:roots a-graph)))

(def refactored-version
  (first
    (filter #(= "refactoring DirectoryScanner to reduce duplicated code, tests all pass  git-svn-id: https://svn.apache.org/repos/asf/ant/core/trunk@436724 13f79535-47bb-0310-9956-ffa450edef68"
               (graph/commit-message %))
         (:versions a-graph))))