(ns qwalkeko.experiments.ward.midclassification
  (:require [qwalkeko.clj.functionalnodes :as changes])
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt [ast :as jdt]])
  (:require [damp.qwal :as qwal])
  (:require [qwalkeko.clj.logic :as l])
  (:require [qwalkeko.clj.graph :as graph])
  (:require [qwalkeko.experiments.ward.projectmanagement :as projectmanagement])
  (:require [qwalkeko.experiments.ward.automated :as automated]))

; Helper via Jolien's code. Given a change, it can be used to consider
; each node in a change at a time.
(defn change-node|affects [change ?node]
  "Get the node affected by a change"
  (logic/condu
    [(changes/change|insert change)
     (changes/change-rightparent change ?node)]
    [(changes/change|update change)
     (changes/change-rightparent change ?node)]
    [(changes/change|delete change)
     (changes/change-original change ?node)]
    [(changes/change|move change)
     (changes/change-rightparent change ?node)]))

(defn lowest-revision-number
  "Metaversions are the input"
  [commit1 commit2]
  (first (sort-by graph/revision-number (list commit1 commit2))))

(defn commit|split
  "Check if a commit in a graph branches off into children. The children are
   distinct. The first child is the one with the 'lowest' revision number.
   Graph and commit need to be grounded.
   (We only match two children, but of course there may be any number)"
  [graph commit ?firstchild ?secondchild]
  
  (qwal/qwal graph commit ?secondchild []
    (qwal/qcurrent [curr]
      (logic/== curr commit))
    qwal/q=>
    (qwal/qcurrent [curr]
      (logic/== curr ?firstchild))
    qwal/q<=
    (qwal/qcurrent [curr]
      (logic/== curr commit))
    qwal/q=>
    (qwal/qcurrent [curr]
      (logic/== curr ?secondchild)
      (logic/!= ?firstchild ?secondchild)
      ; Hacky to force an order and avoid getting a match on children A-B and
      ; children B-A order.
      (logic/project [?firstchild ?secondchild]
        (logic/== ?firstchild (lowest-revision-number ?firstchild ?secondchild))))))

(defn commit|merge
  "Check if a commit in a graph is the result of a merge.
   Graph and commit need to be grounded.
   (We only match two parents, but of course there may be any number - octopus merge)."
  [graph commit ?firstparent ?secondparent]
  (qwal/qwal graph commit ?secondparent []
    (qwal/qcurrent [curr]
      (logic/== curr commit))
    qwal/q<=
    (qwal/qcurrent [curr]
      (logic/== curr ?firstparent))
    qwal/q=>
    (qwal/qcurrent [curr]
      (logic/== curr commit))
    qwal/q<=
    (qwal/qcurrent [curr]
      (logic/== curr ?secondparent)
      (logic/!= ?firstparent ?secondparent)
      (logic/project [?firstparent ?secondparent]
        (logic/== ?firstparent (lowest-revision-number ?firstparent ?secondparent))))))

(defn path-between-commits
  [graph start ?end]
  (qwal/qwal graph start ?end []
    (qwal/q=>*)))

(defn commit|child
  [graph commit ?child]
  (qwal/qwal graph commit ?child []
    qwal/q=>))


; The idea:
; Need to analyse the change for various "tells" and categorise it in midlevel
; categories that make sense. Midlevel being something in the hierarchy between
; toplevel (src, test, build, ...) and lowlevel (AST changes). Probably will
; not end up being a real hierarchy, sadly.

; Based on an example in ray2013 work.
; One branch removes the inner of a nested for loop
; Another branch adds a continue statement in that inner for loop
; (This could of course happen with other looping structures too
;  as well as other keywords like break)
; ISSUE from original idea: Can we do anything here by just looking at
;                           breaker->fixer?
;                           Seeing a continue disappear.. maybe, but that
;                           implies that is even the way they fix it.
;                           Instead look at rest I guess. Will need to rethink
;                           the glue code for this though...

(defn node-in-other-node [ast1 ast2]
  (jdt/ast-parent+ ast1 ast2))

(defn ast|continue
  "Check whether a given AST node is a continue statement"
  [ast]
  (jdt/ast :ContinueStatement ast))

(defn ast|break
  "Check whether a given AST node is a break statement"
  [ast]
  (jdt/ast :BreakStatement ast))

(defn ast|for
  "Check whether a given AST node is a for statement"
  [ast]
  (jdt/ast :ForStatement ast))

(defn change|adds-continue [change]
  (logic/fresh [?node] ; Jolien also used nested logic/all, is that needed?
    (changes/change|insert change)
    (changes/change-rightparent change ?node)
    (ast|continue ?node)))

(defn change|adds-break [change]
  (logic/fresh [?node]
    (changes/change|insert change)
    (changes/change-rightparent change ?node)
    (ast|break ?node)))

(defn change|removes-for [change]
  (logic/fresh [?node]
    (changes/change|delete change)
    (changes/change-original change ?node)
    (ast|for ?node)))


; Assume you already cloned and imported the projects of interest
; (See automated.clj for details)
(defn test-run-fakes []
  (let [FAKE_PROJECTS_FOLDER "/Users/ward/Documents/phd-docs/paper-mergeerrorpatterns-pretend-projects"
        FAKE_COMMITS_FILE "/Users/ward/Documents/phd-docs/paper-mergeerrorpatterns-pretend-projects/fake_commits.csv"
        FAKE_BREAKERFIXER (automated/read-breaker-fixer-csv FAKE_COMMITS_FILE)]
    (let [change-infos (automated/find-all-changes FAKE_BREAKERFIXER)]
      (map println change-infos)
      change-infos)))

(defn test-run-open-project [eclipseproject]
  (.open eclipseproject nil)
  (damp.util.Natures/addNature eclipseproject damp.ekeko.EkekoNature/NATURE_ID)
  ; Poor man's "refresh after Ekeko population is done"
  ; Possibly redundant...
  (Thread/sleep 5000)
  )

(defn test-run-close-project [eclipseproject]
  (.close eclipseproject nil)
  ; Do we need to remove the EkekoNature again?
  )

; Assumes you already cloned/imported
(defn test-run-qwally []
  (let [name "integration-issues-examples"
        eclipseproject (projectmanagement/name-to-eclipse-project name)]
    
    (test-run-open-project eclipseproject)
    
    (let [graph (projectmanagement/eclipse-project-to-graph eclipseproject)
          commits (:versions graph)
          successors (map
                       (fn [commit]
                         (l/qwalkeko* [?firstchild ?secondchild]
                           (commit|split graph commit ?firstchild ?secondchild)))
                       commits)
          predecessors (map
                         (fn [commit]
                           (l/qwalkeko* [?firstchild ?secondchild]
                             (commit|merge graph commit ?firstchild ?secondchild)))
                         commits)
          pathto (map
                   (fn [commit]
                     (l/qwalkeko* [?destination]
                       (path-between-commits graph commit ?destination)))
                   commits)]
      
      
      
      (test-run-close-project eclipseproject)
      (println commits)
      (println successors)
      (println predecessors)
      ;(println "pathto")
      ;(map println pathto)
      )))

(defn find-control-flow-example
  []
  (let [projectname "integration-issues-examples"
        eclipseproject (projectmanagement/name-to-eclipse-project projectname)]
    (test-run-open-project eclipseproject)
    
    (let [graph (projectmanagement/eclipse-project-to-graph eclipseproject)
          commits (graph/versions graph)
          res (map
                (fn [commit]
                  (l/qwalkeko* [?origin ?firstchild ?secondchild ?firstparent ?secondparent ?merge ?diff1 ?diff2]
                    (logic/== commit ?origin)
                    (commit|split graph commit ?firstchild ?secondchild)
                    (path-between-commits graph ?firstchild ?firstparent)
                    (path-between-commits graph ?secondchild ?secondparent)
                    (logic/!= ?firstparent ?secondparent)
                    (commit|child graph ?secondparent ?merge)
                    (commit|child graph ?firstparent ?merge)
                    ; TODO:
                    ; - Get the changed files for origin->firstparent and origin->secondparent
                    ; - Get diff for each file between origin and each parent
                    ; - Use the ast matching there
                    ))
                commits)]
      
      (test-run-close-project eclipseproject)
      res)))