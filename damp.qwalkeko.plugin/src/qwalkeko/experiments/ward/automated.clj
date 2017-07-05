(ns qwalkeko.experiments.ward.automated
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt.astnode :as astnode])
  (:require [damp.ekeko.logic :as el])
  (:require [qwalkeko.clj.logic :as l])
  (:require [qwalkeko.clj.ast :as ast])
  (:require [damp.qwal :as qwal])
  (:require [qwalkeko.clj.graph :as graph])
  (:require [qwalkeko.clj.functionalnodes :as changes])
  (:require [qwalkeko.clj.changenavigation :as nav])
  (:require [damp.ekeko.jdt
             [ast :as jdt]])
  (:require [clojure.repl :as repl])
  (:require [clojure.string :as string])
  (:require [qwalkeko.experiments.ward.projectmanagement :as projectmanagement]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finding changes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-commit-in-graph
  "Given a project graph and a commit SHA, finds the commit object"
  [graph id]
  (first (filter #(= (graph/revision-number %) id) (:versions graph))))

(defn fuckyou
  "Because having qwal/=>+ in find-changes caused too much backtracking apparently."
  [versie]
  (letfn [(fuckyou-intern [g curr ?end]
            (logic/condu
              [(logic/== curr versie)
               (logic/== ?end versie)]
              [(logic/fresh [?next]
                 (qwal/trans g curr ?next)
                 (fuckyou-intern g ?next ?end))]))]
    fuckyou-intern))

(defn find-changes
  "Given a graph and the breaking and fixing commits, returns a list of changed
   files and the changes. For the changes, qwalkeko is used. File needs to have
   been changed in both breaking and fixing commit (confirm?)."
  [graph breaking fixing]
  (let [changed (filter #(= (:status %) :edit) (graph/file-infos fixing)) ; consider only edits of files of the fixing commit (not new/deleted files)
        changes (doall ; force evaluation
                  (map ; For every changed file
                    (fn [changed]
                      (l/qwalkeko 1 [?changes ?end]
                        (qwal/qwal graph breaking ?end [?left ?right]
                          (l/in-source-code [curr]
                            (l/fileinfo|compilationunit changed ?left curr))
                          (fuckyou fixing)
                          (l/in-source-code [curr]
                            (l/fileinfo|compilationunit changed ?right curr)
                            (changes/ast-ast-changes ?left ?right ?changes)))))
                    changed))]
    ; Qwalkeko creates folders of the versions it analyses. Clean them up.
    (graph/ensure-delete breaking)
    (graph/ensure-delete fixing)
    (list changed changes)))

(defn read-breaker-fixer-csv
  "Takes the path to a CSV dump of the form: \"project\",\"breaker\",\"fixer\",\"breaker_tr\",\"fixer_tr\".
   The CSV dump has a header row, which we drop. Returns list of lists"
  [filelocation]
  (let [file (slurp filelocation)
        lines (drop 1 (string/split file #"\n"))
        cleaned-lines (map #(string/replace % #"\"" "") lines)
        entries (map #(string/split % #",") cleaned-lines)]
    entries))

(defn create-project-list
  "Gets a list of unique projects (strings) given a parsed CSV dump"
  [parsed-csv]
  (let [projects (map first parsed-csv)]
    (distinct projects)))


(defn handle-change
  "Helper function (define it in find-all-changes?). Given the graph of a
   project and a change entry (a list of project, breaker sha, fixer sha,
   breaker tr id, fixer tr id), finds the actual changes and returns an ad-hoc
   'object' containing the inputted information + the found changes."
  [graph change]
  (let [breaking (find-commit-in-graph graph (nth change 1))
        fixing (find-commit-in-graph graph (nth change 2))
        changes (find-changes graph breaking fixing)]
    {
     :project (nth change 0),
     :breaking (nth change 1),
     :fixing (nth change 2),
     :breaking_tr (nth change 3),
     :fixing_tr (nth change 4),
     :changed (first changes),
     :changes (second changes)
    }))

(defn handle-project
  "Helper function (define it in find-all-changes?). Given a project name and
   a list of change entries (see handle-change), it returns the Qwalkeko changes
   for the entries."
  [entry]
  (let [name (first entry)
        project (projectmanagement/name-to-eclipse-project name)
        changes (second entry)]
    (.open project nil)
    (damp.util.Natures/removeNature project damp.ekeko.EkekoNature/NATURE_ID)
    (damp.util.Natures/addNature project damp.ekeko.EkekoNature/NATURE_ID)
    ; Poor man's "refresh after Ekeko population is done"
    (Thread/sleep 10000)
    (let [g (projectmanagement/name-to-graph name)
          results (map #(handle-change g %) changes)]
      (damp.util.Natures/removeNature project damp.ekeko.EkekoNature/NATURE_ID)
      (.close project nil)
      results)))

(defn find-all-changes
  "Given a list of changes, each a list of project, breaker sha, fixer sha,
   breaker tr id, fixer tr id, finds the AST changes of each and returns it
   all."
  [changelist-ungrouped]
  (let [changelist (group-by first changelist-ungrouped)]
    (flatten (map handle-project changelist))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Actual action ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Folder in which to clone the different projects
(def PROJECT_FOLDER "/Users/ward/Documents/phd-docs/paper-mergeerrorpatterns/projects")
; File containing the CSV dump
(def COMMITS_DUMP "/Users/ward/Documents/phd-docs/paper-mergeerrorpatterns/test_fixers.csv")

(def BREAKERFIXER (read-breaker-fixer-csv COMMITS_DUMP))
(def PROJECTS (create-project-list BREAKERFIXER))

;(projectmanagement/clone-projects PROJECT_FOLDER PROJECTS)
;(projectmanagement/import-projects PROJECT_FOLDER)
;(find-all-changes BREAKERFIXER)
