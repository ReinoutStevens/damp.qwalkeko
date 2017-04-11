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
  "Given a location to clone projects in and a file with one GitHub project per
   line, clone every project into the folder. Both arguments are strings."
  [dll-folder info-file]
  (let [projects (string/split-lines (slurp info-file))]
    (map #(clone-project dll-folder %) projects)))


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


(defn find-commit-in-graph
  "Given a project graph and a commit SHA, finds the commit object"
  [graph id]
  (first (filter #(= (graph/revision-number %) id) (:versions graph))))


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
                          (qwal/q=>+)
                          (l/in-git-info [curr]
                            (logic/== curr fixing))
                          (l/in-source-code [curr]
                            (l/fileinfo|compilationunit changed ?right curr)
                            (changes/ast-ast-changes ?left ?right ?changes)))))
                    changed))]
    ; Qwalkeko creates folders of the versions it analyses. Clean them up.
    (graph/ensure-delete breaking)
    (graph/ensure-delete fixing)
    (list changed changes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Actual action ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Folder in which to clone the different projects
(def PROJECT_FOLDER "/Users/ward/Documents/phd/paper-mergeerrorpatterns/projects")
; File containing GitHub projects (line per line)
(def PROJECT_FILE "/Users/ward/Documents/phd/paper-mergeerrorpatterns/github_projects.txt")

;(clone-projects PROJECT_FOLDER PROJECT_FILE)
;(import-projects PROJECT_FOLDER)