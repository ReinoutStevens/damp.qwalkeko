(ns qwalkeko.experiments.ward.projectmanagement
  (:require [clojure.string :as string])
  (:require [clojure.java.shell :as shell])
  (:require [clojure.java.io :as io])
  (:require [qwalkeko.clj.graph :as graph]))


(defn sanitize-project-name
  "GitHub projects are commonly referred to as 'user/project'. This may lead to
   problems and/or confusion in file hierarchy. Instead replace that / by a -."
  [name]
  (string/replace name #"/" "-"))

; TODO: This can use sanitize-project-name
(defn clone-project
  "Given a location to clone projects in and one GitHub project, clone the
   project into the folder. Both are strings. userproject is of the form
   user/project"
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

; TODO: Find out how the Nature thing works exactly. That is, does it need to be
; readded etc everytime. Does it matter?
; addNature seems to do nothing if the Nature is already present
; Does it need the sleep still though?
; Some stuff seems to be present in damp.ekeko.EkekoModel class.
; TODO: Use this at some point. Currently not sure enough of inner workings.
(defn open-project
  "A project in Eclipse can be closed (and ignored by everything) or open.
   Changing between the two is a bit more involved than you would think since
   we also want the NATUREs to be correctly present. Our knowledge of the Ekeko
   and Qwalkeko natures is not sufficient to know if this is done smoothly or
   not. We use the horrible workaround of pausing 10s."
  [eclipseproject]
  (.open eclipseproject nil)
  (damp.util.Natures/addNature eclipseproject qwalkeko.HistoryNature/NATURE_ID)
  (damp.util.Natures/addNature eclipseproject damp.ekeko.EkekoNature/NATURE_ID)
  ; Assuming only need this the first time (i.e., when we call this function after
  ; importing a project.
  ;(Thread/sleep 10000)
  )

(defn import-project-location
  "Given a File object of a .git folder, imports the project into the Eclipse
   workspace in which Qwalkeko is active. The imported project will have only
   a project.xml file in it which is the meta information that Qwalkeko needs
   to reason about it. The imported project is given the correct Eclipse nature
   for Qwalkeko. The imported project is also given the correct Eclipse nature
   for Ekeko such that Ekeko queries can be cast over it. Note that Qwalkeko
   overrides the usual Ekeko building with its own (HistoryProjectModel.buildMetaProduct),
   which creates a resources/ folder within the project.

   To (hopefully) save memory and avoid GC crashes, the projects are closed
   after the nature has been added. Opening should be done before looking for
   changes.

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
      ;(damp.util.Natures/addNature project damp.ekeko.EkekoNature/NATURE_ID)
      ; Poor man's "refresh after Nature analysis is done"
      (Thread/sleep 10000)
      (.close project nil)
      (.refreshLocal project org.eclipse.core.resources.IProject/DEPTH_INFINITE nil)
      project)))

(defn import-projects
  "Given a string representing a folder with projects, import each of them in
   Eclipse. Set up the correct Natures (Qwalkeko and Ekeko). Returns a list of
   the Eclipse projects."
  [projects-folder-string]
  (let [projects-git (get-projects-git projects-folder-string)]
    (map import-project-location projects-git)))


;; GRAPHS

(defn name-to-eclipse-project
  [projectname]
  (let [eclipseroot (.getRoot (org.eclipse.core.resources.ResourcesPlugin/getWorkspace))
        name (sanitize-project-name projectname)
        project (.getProject eclipseroot name)]
    project))

(defn eclipse-project-to-graph
  "Given an eclipse project (not ekeko project), creates the graph after
   finding the right Ekeko project."
  [eclipseproject]
  (let [projects (damp.ekeko.ekekomodel/all-project-models)
        history (first (filter #(= (.getProject %) eclipseproject) projects))]
    (graph/convert-model-to-graph history)))

(defn- qwalkeko-project-is-named?
  "Predicate to check whether a Qwalkeko project has a certain name. The /
   separating GitHub user and project is changed to a -."
  [qwalkekoproject name]
  (= (string/replace name #"/" "-") (.getName (.getProject qwalkekoproject))))

(defn name-to-graph
  "Given the name of a project ('user/repo'), find the project and create a graph"
  [projectname]
  (let [projects (damp.ekeko.ekekomodel/all-project-models)
        history (first (filter #(qwalkeko-project-is-named? % projectname) projects))]
    (graph/convert-model-to-graph history)))