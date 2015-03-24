(ns qwalkeko.clj.changes
  (:require [clojure.core.logic :as logic])
  (:require [qwalkeko.clj.functionalnodes :as nodes])
  (:require [damp.ekeko.logic :as el])
  (:require [damp.ekeko.jdt
           [ast :as jdt]]))

(defn changes [changes lroot rroot]
  (logic/all
    (logic/project [lroot rroot]
      (logic/== changes (nodes/get-ast-changes lroot rroot)))))

(defn change [?change lroot rroot]
  (logic/fresh [?changes]
    (changes ?changes lroot rroot)
    (damp.ekeko.logic/contains ?changes ?change)))


(defn change-type [?change ?type]
  (logic/project [?change]
    (logic/featurec ?change {:operation ?type})))


;;non relational predicates
(defn change|move [change]
  (logic/all
    (change-type change :move)))

(defn change|update [change]
  (logic/all
    (change-type change :update)))

(defn change|insert [change]
  (logic/all
    (change-type change :insert)))

(defn change|delete [change]
  (logic/all
    (change-type change :delete)))

(defn change-original [change ?original]
  (logic/project [change]
    (logic/featurec change {:original ?original})))     

(defn update-rightparent [update ?value]
  (logic/all
    (change|update update)
    (logic/project [update]
      (logic/featurec update {:right-parent ?value}))))

(defn update-into 
  [update ?node]
  (logic/fresh [?newnode]
    (update-rightparent update ?newnode)
    (logic/conde
      [(logic/== ?node ?newnode)]
      [(jdt/ast-parent+ ?newnode ?node)])))

(defn update-property [update ?property]
  (logic/all
    (change|update update)
    (logic/project [update]
      (logic/featurec update {:property ?property}))))

(defn insert-newnode [insert ?node]
  (logic/all
    (change|insert insert)
    (logic/project [insert]
      (logic/featurec insert {:right-node ?node}))))

(defn move-newparent [move ?node]
  (logic/all
    (change|move move)
    (logic/project [move]
      (logic/featurec move {:new-parent ?node}))))

(defn move-rightnode [move ?node]
  (logic/all
    (change|move move)
    (logic/project [move]
      (logic/featurec move {:right-node ?node}))))


(defn insert-into
  [insert ?node]
  (logic/fresh [?newnode]
    (insert-newnode insert ?newnode)
    (logic/conde
      [(logic/== ?node ?newnode)]
      [(jdt/ast-parent+ ?newnode ?node)])))

(defn move-into
  [move ?node]
  (logic/fresh [?newnode]
    (move-rightnode move ?newnode)
    (logic/conde
      [(logic/== ?node ?newnode)]
      [(jdt/ast-parent+ ?newnode ?node)])))

(defn change-affects-original-node
  [change ?node]
  (logic/fresh [?original]
    (change-original change ?original)
    (logic/conde
      [(logic/== ?original ?node)]
      [(jdt/ast-parent+ ?original ?node)])))


(defn change-affects-new-node
  [change ?node]
  (logic/conda
    [(change|insert change)
     (insert-into change ?node)]
    [(change|move change)
     (move-into change ?node)]
    [(change|update change)
     (update-into change ?node)]))

(defn change-affects-node [change ?node]
  (logic/conde
    [(change-affects-original-node change ?node)]
    [(move-into change ?node)]
    [(insert-into change ?node)]))

(defn change-contains-original-node [change ?node]
  (logic/fresh [?original]
    (change-original change ?original)
    (logic/conde
      [(logic/== ?node ?original)]
      [(jdt/child+ ?original ?node)])))

(defn change-contains-new-node [change ?node]
  (logic/fresh [?n]
    (logic/conda
      [(change|insert change)
       (insert-newnode change ?n)]
      [(change|update change)
       (update-rightparent change ?n)]
      [(change|move change)
       (move-newparent change ?n)])
    (logic/conde
      [(logic/== ?node ?n)]
      [(jdt/child+ ?n ?node)])))


(defn change-ast|inserted [changes ?c ?ast]
  (logic/all
    (el/contains changes ?c)
    (change|insert ?c)
    (insert-newnode ?c ?ast)))

(defn change-ast|moved [changes ?c ?ast]
  (logic/all
    (el/contains changes ?c)
    (change|move ?c)
    (move-rightnode ?c ?ast)))

(defn change-ast|renamed [changes ?c ?ast]
  (logic/fresh [?name]
    (el/contains changes ?c)
    (change|insert ?c)
    (insert-newnode ?c ?name)
    (jdt/ast :SimpleName ?name)
    (jdt/ast-parent ?name ?ast)
    (jdt/has :name ?ast ?name))) ;;verify it is the name property


;;higher level predicates

(defn changes-ast|introduced [changes ?c ?ast]
  "?c introduces a new ast node"
  ;;insert
  ;;move
  (logic/all
    (el/contains changes ?c)
    (logic/conde
      ([change-ast|inserted changes ?c ?ast])
      ([change-ast|moved changes ?c ?ast]))))




(defn changes-statement-method|extracted [changes ?statement ?method]
  (logic/fresh [?delete ?insert]
    (el/contains changes ?delete)
    (change|delete ?delete)
    (el/contains changes ?insert)
    (change|insert ?insert)
    ))
    
    

 
(defn changes-method-method|extracted [changes ?source ?extracted]
  "source is bound in left version, extracted in right"
  (logic/fresh [?block ?statement]
    (jdt/ast :MethodDeclaration ?source)
    (jdt/ast :MethodDeclaration ?extracted)
    (jdt/has :body ?extracted ?block)
    ;;ensure there is no single statement inside the block that does not come from source
    (el/fails 
      (jdt/child :statements ?block ?statement)
      ;;TODO
      )))

