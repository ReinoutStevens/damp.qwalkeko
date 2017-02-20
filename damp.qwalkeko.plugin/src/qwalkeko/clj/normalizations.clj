(ns qwalkeko.clj.normalizations)


(defn graph-graph-change|remove [graph ?new-graph change]
  (logic/project [graph change]
    (logic/== ?new-graph (change-remove graph change))))


(defn graph-insert-delete|redundant [graph ?insert ?delete]
  (logic/fresh [?node ?insertIdx ?rightInsert]
    (graph-change|insert graph ?insert)
    (change-copy ?insert ?node)
    (graph-change-index|independent graph ?insert ?insertIdx)
    (logic/project [?insert]
      (qwal/qwal graph ?insert ?delete []
        change=>
        (qwal/qcurrent [curr]
	         (change|delete curr)
	         (change-original curr ?delete)
           (graph-change-index|independent ?delete ?insertIdx)
	         (ast/ast-ast|same ?rightInsert ?delete))))))

(defn graph-delete-insert|redundant [graph ?delete ?insert]
  (logic/fresh [?deleteIdx ?insertNode ?node]
    (graph-change|delete graph ?delete)
    (change-original ?delete ?node)
    (graph-change-index|independent graph ?delete ?deleteIdx)
    (logic/project [?insert]
      (qwal/qwal graph ?insert ?delete []
        change=>
        (qwal/qcurrent [curr]
          (change|insert curr)
          (change-copy curr ?insertNode)
          (graph-change-index|independent graph curr ?deleteIdx)
          (ast/ast-ast|same ?insertNode ?node))))))

(defn graph-graph|remove-insert-delete [graph ?new-graph]
  (logic/fresh [?delete ?insert ?tempgraph]
    (logic/conde
      [(graph-insert-delete|redundant graph ?insert ?delete)]
      [(graph-delete-insert|redundant graph ?delete ?insert)])
    (graph-graph-change|remove graph ?tempgraph ?delete)
    (graph-graph-change|remove ?tempgraph ?new-graph ?insert)))

(defn graph-move-move|redundant [graph ?moveA ?moveB]
  (logic/project [graph]
    (logic/fresh [?nodeA ?nodeB ?originalA ?originalB
                  ?leftParentA ?leftParentB 
                  ?rightParentA ?rightParentB]
      (graph-change|move graph ?moveA)
      (graph-change|move graph ?moveB)
      (logic/!= ?moveA ?moveB)
	    (change-copy ?moveA ?nodeA)
	    (change-copy ?moveB ?nodeB)
	    (change-rightparent ?moveA ?rightParentA)
	    (change-rightparent ?moveB ?rightParentB)
	    (ast/ast-ast|same ?nodeA ?nodeB)
	    (jdt/ast-parent ?moveA ?leftParentA)
	    (jdt/ast-parent ?moveA ?leftParentB)
	    (ast-ast|matching graph ?leftParentA ?rightParentB)
	    (ast-ast|matching graph ?leftParentB ?rightParentA))))

(defn graph-graph|remove-move-move [graph ?new-graph]
  (logic/fresh [?moveA ?moveB ?tempgraph]
    (graph-move-move|redundant graph ?moveA ?moveB)
    (graph-graph-change|remove graph ?tempgraph ?moveA)
    (graph-graph-change|remove ?tempgraph ?new-graph ?moveB)))