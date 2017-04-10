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
  ;(:require [qwalkeko.clj.graph-algo :as algo])
  (:require [qwalkeko.clj.changenavigation :as nav])
  (:require [damp.ekeko.jdt
             [ast :as jdt]]))


;;Folder met al de projecten


;;file met project commit bullshit


;;importeer project


     
     
(defn import-project-location [location] ;;location is file object naar .git
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
      (damp.util.Natures/addNature project  qwalkeko.HistoryNature/NATURE_ID)
      (damp.util.Natures/addNature project  damp.ekeko.EkekoNature/NATURE_ID)
      ;;add sleep
      (.refreshLocal project org.eclipse.core.resources.IProject/DEPTH_INFINITE nil)
      project)))
    
(defn convert-project-to-graph [project]
  (let [projects (damp.ekeko.ekekomodel/all-project-models)
        history (first (filter #(= (.getProject %) project) projects))]
    (graph/convert-model-to-graph history)))


(defn find-commit-in-graph [graph id]
  (first (filter #(= (graph/revision-number %) id) (:versions graph))))


(defn do-magic [graph breaking fixing]
  (let [changed (filter #(= (:status %) :edit) (graph/file-infos fixing))
        changes (doall
                  (map 
                    (fn [changed]
                      (l/qwalkeko 1 [?changes]
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
    (graph/ensure-delete breaking)
    (graph/ensure-delete fixing)
    (list changed changes)))
        
              