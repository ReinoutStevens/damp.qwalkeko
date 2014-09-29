(ns qwalkeko.clj.logic
  (:refer-clojure :exclude [== type])
  (:require [clojure.core.logic :as logic])
  (:require [qwalkeko.clj.reification :as reification])
  (:require [qwalkeko.clj.graph :as graph])
  (:require [qwalkeko.clj.ast :as ast])
  (:require [qwalkeko.clj.sessions :as sessions]) 
  (:require [damp.ekeko.workspace.reification :as workspace])
  (:require [damp.ekeko.jdt.ast :as jdt])
  (:require [damp.ekeko :as ekeko])
  (:import [qwalkeko HistoryProjectModel])
  (:require [damp.qwal :as qwal])
  (:require [clojure.java.io :as io]))




;;Rules over FileInfo

(defn fileinfo [?fileinfo version]
  (logic/conda
    [(logic/lvaro ?fileinfo)
     (logic/membero ?fileinfo (graph/file-infos version))]
    [(logic/nonlvaro ?fileinfo)])) ;;assume this is a fileinfo in current version, membero is too slow

(defn fileinfos [?fileinfos version]
  (logic/all
    (logic/== ?fileinfos (graph/file-infos version))))

(defn fileinfo|status [?fileinfo ?status version]
  (logic/all
    (fileinfo ?fileinfo version)
    (logic/project [?fileinfo]
                   (logic/featurec ?fileinfo {:status ?status}))))

(defn fileinfo|add [?fileinfo version]
  (logic/all
    (fileinfo|status ?fileinfo :add version)))

(defn fileinfo|edit [?fileinfo version]
  (logic/all
    (fileinfo|status ?fileinfo :edit version)))

(defn fileinfo|delete [?fileinfo version]
  (logic/all
    (fileinfo|status ?fileinfo :delete version)))


(defn fileinfo|file [?fileinfo ?file version]
  (logic/all
    (fileinfo ?fileinfo version)
    (logic/project [?fileinfo]
                   (logic/featurec ?fileinfo {:file ?file}))))

(defn fileinfo|java [?fileinfo version]
  (logic/fresh [?file]
    (fileinfo|file ?fileinfo ?file version)
    (logic/project [?file]
      (logic/== true (.endsWith ?file ".java")))))


(defn fileinfo|maintypename [?fileinfo ?name version]
  (logic/fresh [?file]
    (fileinfo|file ?fileinfo ?file version)
    (logic/project [?file]
      (logic/== ?name
        (apply str (take-while #(not (= % \.)) (.getName (io/file ?file))))))))


(defn fileinfo-package|in [?fileinfo ?package version]
  (logic/fresh [?fname ?pname ?pstrname]
    (jdt/ast :PackageDeclaration ?package)
    (jdt/has :name ?package ?pname)
    (jdt/name|qualified-string ?pname ?pstrname)
    (fileinfo|file ?fileinfo ?fname version)
    (logic/project [?pstrname ?fname]
      ;;we drop the Class part of the filename and replace / with .
      ;;we then verify that this string ends with packagename 
      (logic/== 
        true
        (.endsWith
          (clojure.string/replace
            (apply str (take (.lastIndexOf ?fname "/") ?fname))
            \/ \.)
          ?pstrname)))))

(defn fileinfo|compilationunit [?fileinfo ?compilationunit version]
  (logic/fresh [?typedeclaration ?name ?filename ?package]
    (fileinfo ?fileinfo version)
    (fileinfo|maintypename ?fileinfo ?filename version)
    (jdt/ast :CompilationUnit ?compilationunit)
    (jdt/has :package ?compilationunit ?package)
    (fileinfo-package|in ?fileinfo ?package version)
    (ast/compilationunit-typedeclaration|main ?compilationunit ?typedeclaration)
    (jdt/has :name ?typedeclaration ?name)
    (jdt/name-string|qualified ?name ?filename)))

;;general version information
(defn revisionnumber [?number version]
  (logic/all
    (logic/== ?number (graph/revision-number version))))



;;Helper macros
(defmacro qwalkeko* [ [ & vars] & goals]
  `(binding [damp.ekeko.ekekomodel/*queried-project-models*  (atom '())]
     (doall
       (logic/run* [~@vars] ~@goals))))


(defmacro qwalkeko-ui* [ [ & vars ] & goals]
  `(binding [damp.ekeko.ekekomodel/*queried-project-models*  (atom '())]
     (ekeko/ekeko* [~@vars]
       ~@goals)))

(defmacro qwalkeko [results [ & vars] & goals]
    `(binding [damp.ekeko.ekekomodel/*queried-project-models*  (atom '())]
       (doall
         (logic/run ~results [~@vars] ~@goals))))

(defmacro qwalkeko-ui [results [& vars] & goals]
  `(binding [damp.ekeko.ekekomodel/*queried-project-models*  (atom '())]
     (ekeko/ekeko-n* ~results 
       [~@vars]
       ~@goals)))

(defmacro qwalkeko-ui [n [& vars] & goals]
  `(let [querystr# (damp.ekeko.util.text/pprint-query-str '(qwalkeko-ui [~@vars] ~@goals))
         start# (System/nanoTime)
         resultsqc# (binding [damp.ekeko.ekekomodel/*queried-project-models*  (atom '())]
                      (doall (logic/run ~n [resultvar#] 
                               (logic/fresh [~@vars]
                                      (logic/== resultvar# [~@vars])
                                      ~@goals))))
         elapsed#  (/ (double (- (System/nanoTime) start#)) 1000000.0)
         cnt# (count resultsqc#)]
     (damp.ekeko.gui/eclipse-uithread-return
       (fn [] (damp.ekeko.gui/open-barista-results-viewer* querystr# '(~@vars) resultsqc# elapsed# cnt#)))))

(defmacro in-current-meta [[meta] & goals]
  `(qwal/qcurrent [~meta] ~@goals))

(defmacro in-current [[curr] & goals ]
  `(sessions/vcurrent [~curr] ~@goals))

(defmacro in-git-info [[meta] & goals]
   `(qwal/qcurrent [~meta] ~@goals))

(defmacro in-source-code [[curr] & goals]
  `(sessions/vcurrent [~curr] ~@goals))


