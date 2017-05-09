(ns qwalkeko.experiments.automated
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt.astnode :as astnode])
  (:require [damp.ekeko.logic :as el])
  (:require [qwalkeko.clj.logic :as l])
  (:require [qwalkeko.clj.ast :as ast])
  (:require [clojure.java.io :as io])
  (:require [damp.qwal :as qwal])
  (:require [qwalkeko.clj.graph :as graph])
  (:require [qwalkeko.clj.functionalnodes :as changes])
  (:require [qwalkeko.clj.changenavigation :as nav])
  (:require [damp.ekeko.jdt
             [ast :as jdt]])
  (:require [clojure.repl :as repl])
  (:require [clojure.string :as string])
  (:require [clojure.java.shell :as shell]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setting up the projects locally ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn clone-project
  "Given a location to clone projects in and one GitHub project, clone the
   project into the folder. Both are strings."
  [project-folder userproject]
  (let [user (first (string/split userproject #"/"))
        project (second (string/split userproject #"/"))
        ghlink (str "https://github.com/" user "/" project)
        clone-folder (str project-folder "/" user "-" project)]
    (shell/sh "git" "clone" "-q" ghlink clone-folder)))

(defn clone-projects
  "Given a location to clone projects in (string) and a list of GitHub projects (user/repo),
   clone every project into the folder."
  [dll-folder projects]
    (map #(clone-project dll-folder %) projects))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Importing projects in Eclipse ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-git-folder
  "Check whether a given File object is a .git folder"
  [folder]
  (and
    (.isDirectory folder)
    (= ".git" (.getName folder))))

(defn get-projects-git
  "Get File objects for the .git/ folder of every project in project-folder string."
  [project-folder]
  (let [folder (io/file project-folder)
        projects (filter #(.isDirectory %) (.listFiles folder))
        projects-subfolders (map #(.listFiles %) projects)
        projects-git (flatten (map #(filter is-git-folder %) projects-subfolders))]
    projects-git))

(defn import-project-location
  "Given a File object of a .git folder, imports the project into the Eclipse
   workspace in which Qwalkeko is active. The imported project will have only
   a project.xml file in it which is the meta information that Qwalkeko needs
   to reason about it. The imported project is given the correct Eclipse nature
   for Qwalkeko. The imported project is also given the correct Eclipse nature
   for Ekeko such that Ekeko queries can be cast over it. Note that Qwalkeko
   overrides the usual Ekeko building with its own (HistoryProjectModel.buildMetaProduct),
   which creates a resources/ folder within the project.

   The code here is a clojure translation of the code in ImportRepositoryHandler.java"
  [location]
  (let [project-name (.getName (.getParentFile location))
        root (.getRoot (org.eclipse.core.resources.ResourcesPlugin/getWorkspace))
        project (.getProject root project-name)]
    (if-not (.exists project)
      (.create project nil))
    (let [bundle (org.eclipse.core.runtime.Platform/getBundle qwalkeko.Activator/PLUGIN_ID)
          jarPath (new org.eclipse.core.runtime.Path "lib/git-scrapper-1.0.1.jar")
          jarUrl (org.eclipse.core.runtime.FileLocator/find bundle jarPath nil)
          jarString (str (.getAbsolutePath (org.eclipse.core.runtime.FileLocator/getBundleFile bundle)) (.getPath (.toURI jarUrl)))
          target (.toString (.getLocation (.getFile project "project.xml")))
          pb (new ProcessBuilder (into-array String (list "java" "-jar" jarString (str location) target)))
          proc (.start pb)]
      (.waitFor proc)
      (.open project nil)
      (damp.util.Natures/addNature project qwalkeko.HistoryNature/NATURE_ID)
      (damp.util.Natures/addNature project damp.ekeko.EkekoNature/NATURE_ID)
      ; Poor man's "refresh after Nature analysis is done"
      (Thread/sleep 10000)
      (.refreshLocal project org.eclipse.core.resources.IProject/DEPTH_INFINITE nil)
      project)))

(defn import-projects
  "Given a string representing a folder with projects, import each of them in
   Eclipse. Set up the correct Natures (Qwalkeko and Ekeko). Returns a list of
   the Eclipse projects."
  [projects-folder-string]
  (let [projects-git (get-projects-git projects-folder-string)]
    (map import-project-location projects-git)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finding changes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn convert-project-to-graph
  "Given an eclipse project (not ekeko project), creates the graph after
   finding the right Ekeko project."
  [project]
  (let [projects (damp.ekeko.ekekomodel/all-project-models)
        history (first (filter #(= (.getProject %) project) projects))]
    (graph/convert-model-to-graph history)))


(defn is-qwalkeko-project-named?
  "Predicate to check whether a Qwalkeko project has a certain name. The /
   separating GitHub user and project is changed to a -."
  [project name]
  (= (string/replace name #"/" "-") (.getName (.getProject project))))

(defn convert-project-name-to-graph
  "Given the name of a project ('user/repo'), find the project and create a graph"
  [projectname]
  (let [projects (damp.ekeko.ekekomodel/all-project-models)
        history (first (filter #(is-qwalkeko-project-named? % projectname) projects))]
    (graph/convert-model-to-graph history)))

(defn find-commit-in-graph
  "Given a project graph and a commit SHA, finds the commit object"
  [graph id]
  (first (filter #(= (graph/revision-number %) id) (:versions graph))))

(defn fuckyou [versie]
  "Because having qwal/=>+ in find-changes caused too much backtracking apparently."
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
   files and the changes. For the changes, qwalkeko is used."
  [graph breaking fixing]
  (let [changed (filter #(= (:status %) :edit) (graph/file-infos fixing)) ; take only edits of files of the fixing commit (not new/deleted files)
        changes (doall ; force evaluation
                  (map
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
   a list of change entries (see handle-change), creates a graph, finds the
   changes for the entries and returns it all."
  [entry]
  (let [name (first entry)
        changes (second entry)
        g (convert-project-name-to-graph name)]
    (map #(handle-change g %) changes)))

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

;(clone-projects PROJECT_FOLDER PROJECTS)
;(import-projects PROJECT_FOLDER)
;(find-all-changes BREAKERFIXER)
