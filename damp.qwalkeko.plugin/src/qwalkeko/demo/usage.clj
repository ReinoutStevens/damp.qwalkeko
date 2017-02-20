(ns qwalkeko.demo.usage
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
             [ast :as jdt]]))


;;Creating a VCS Graph
;ensure that there is a project with the history nature enabled
;double check that 'a-model' is bound to a HistoryProjectModel
;projects can be imported using Ekeko->QwalKeko->Import git repo and selecting the .git dir

(def a-model (first (damp.ekeko.ekekomodel/all-project-models)))
(def a-graph (graph/convert-model-to-graph a-model))
(def a-root (first (:roots a-graph)))

;;example queries
;get modified file of revision
(l/qwalkeko 1 [?end ?file]
  (qwal/qwal a-graph a-root ?end []
    (l/in-git-info [curr]
      (l/fileinfos ?file curr))))


(l/qwalkeko 1 [?end ?file]
  (qwal/qwal a-graph a-root ?end []
    qwal/q=> ;skip a revision
    (l/in-git-info [curr]
      (l/fileinfos ?file curr))))

(l/qwalkeko 1 [?end ?file]
  (qwal/qwal a-graph a-root ?end []
    (qwal/q=>*) ;skip arbitrary number of revisions
    (l/in-git-info [curr]
      (l/fileinfos ?file curr))))

;source code stuff
(l/qwalkeko 1 [?end ?ast]
  (qwal/qwal a-graph a-root ?end []
    (qwal/q=>*) ;skip arbitrary number of revisions
    (l/in-source-code [curr]
      (jdt/ast :MethodDeclaration ?ast))))

;get two revisions of the same file
(l/qwalkeko 1 [?end ?left ?right]
  (qwal/qwal a-graph a-root ?end []
    (qwal/q=>*) ;skip arbitrary number of revisions
    (l/in-source-code [curr]
      (jdt/ast :CompilationUnit ?left))
    q=>
    (l/in-source-code [curr]
      (ast/compilationunit-compilationunit|corresponding ?left ?right))))

;matching compilation unit of modified file
(def results
  (l/qwalkeko 1 [?left ?right ?end]
    (qwal/qwal a-graph a-root ?end [?file]
      (qwal/q=>*) ;skip arbitrary number of revisions
      (l/in-source-code [curr]
        (l/fileinfo|edit ?file curr)
        (l/fileinfo|compilationunit ?file ?right))
      q<=
      (l/in-source-code [curr]
        (ast/compilationunit-compilationunit|corresponding ?left ?right)))))
(def left (first (first results)))
(def right (second (first results)))


;changes
(def nav-graph (nav/ast-ast-navigatable-graph left right))
(l/qwalkeko 1 [?state]
  (nav/step-changes nav-graph ?state [?method]
    (nav/change->*)
    (nav/in-current-change-state [curr ast]
      (jdt/ast :MethodDeclaration ?method))))

