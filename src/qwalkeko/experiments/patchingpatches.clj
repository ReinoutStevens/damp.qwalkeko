(ns qwalkeko.experiments.patchingpatches
  (:import [changenodes.Differencer])
  (:import [changenodes.matching.SubtreeMatcher]))


;;Differencing
(defn make-differencer [left-ast right-ast]
  (new changenodes.Differencer left-ast right-ast))

(defn difference [differencer]
  (.difference differencer))


(defn patch [left-ast right-ast]
  (let [differencer (make-differencer left-ast right-ast)]
    {:operations (.getOperations (difference differencer))
     :differencer differencer}))

(defn source-ast [a-patch]
  (.getOriginal (:differencer a-patch)))

(defn target-ast [a-patch]
  (.getRight (:differencer a-patch)))


;;Matching


(defn match [left right]
  (let [matcher (SubtreeMatcher.)]
    (do
      (.match matcher left right)
      matcher)))

;;Representing Fluffy ASTs

;;assume all changes are related

(defn fluffy-patch [left-patch right-patch]
  (let [left-source-ast (source-ast left-patch)
        right-source-ast (source-ast right-patch)
        matcher (match left-source-ast right-source-ast)]
    ))
    
    

(defn ast-contains-node [an-ast node]
  (logic/fresh [?type]
    ;(c/ast type node)
    (c/ground-ast node ast)))
    

(defn ast-contains-nodes [an-ast nodes]
  (logic/conde
    [(logic/== nodes '())]
    [(logic/fresh [h t]
             (logic/conso h t nodes)
             (ast-contains-node an-ast node)
             (ast-contains-nodes an-ast t))]))
      

(defn ast-resolve-constraint [an-ast constraint]
  )

(defn ast-resolve-constraints [an-ast constraints]
  (logic/conde
    [(logic/== constraints '())]
    [(logic/fresh [h t]
                  (logic/conso h t constraints)
                  (ast-resolve-constraint an-ast h)
                  (ast-resolve-constraints an-ast t))]))

(defn ast-matches [fluffy-ast an-ast]
  (logic/all
    (ast-contains-nodes an-ast (:nodes fluffy-ast))
    (ast-resolve-constraints an-ast)))