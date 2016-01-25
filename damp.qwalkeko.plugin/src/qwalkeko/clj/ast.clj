(ns qwalkeko.clj.ast
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.logic :as el])
  (:require [damp.ekeko.jdt
             [ast :as jdt]
             [astnode :as astnode]]))

;;proper clojure integration for ast/has

(defn ast-clj? [keyword node]
  "returns whether node is an instance of keyword"
  (instance? (astnode/class-for-ekeko-keyword keyword) node))

(defn has-clj [keyword node]
  (if (nil? node)
    nil
    (let [f (keyword (astnode/reifiers node))]
      (if f
        (f node)
        nil))))

(defn has-clj-unwrapped [keyword node]
  (let [res (has-clj keyword node)]
    (if (astnode/value? res)
      (astnode/value-unwrapped res)
      res)))

;;astmatching
(defn match? 
  ([left right]
    (let [matcher (new org.eclipse.jdt.core.dom.ASTMatcher)]
      (match? left right matcher)))
  ([left right matcher]
    (.subtreeMatch left matcher right)))


;;modifiers

(defmacro make-modifier [name]
  (let [actual-name (symbol (str "modifier|" name))
        accessor (symbol
                   (str ".is" 
                     (clojure.string/capitalize 
                       (apply str (butlast (str name))))))
        ?modifier '?modifier]
    `(defn ~actual-name [~?modifier]
       (logic/all
         (jdt/ast :Modifier ~?modifier)
         (logic/project [~?modifier]
           (logic/== true (~accessor ~?modifier)))))))

(make-modifier abstract?)
(make-modifier annotation?)
(make-modifier final?)
(make-modifier modifier?)
(make-modifier native?)
(make-modifier public?)
(make-modifier private?)
(make-modifier protected?)
(make-modifier static?)
(make-modifier synchronized?)
(make-modifier transient?)
(make-modifier volatile?)

;;improved performance?
(defn child+-iter [node child]
  (logic/all
    (logic/conda
      [(logic/lvaro node)
       (jdt/child+ node child)]
      [(logic/nonlvaro node)
       (logic/project [node]
         (el/contains
           (iterator-seq 
             (new changenodes.comparing.BreadthFirstNodeIterator node))
           child))])))

(defn child+-type [?node ?type ?child]
  (logic/all
    (logic/conda
      [(logic/lvaro ?node)
       (jdt/child+ ?node ?child)
       (jdt/ast ?type ?child)]
      [(logic/lvaro ?type)
       (jdt/child+ ?node ?child)
       (jdt/ast ?type ?child)]
      [(logic/nonlvaro ?node)
       (logic/project [?node ?type]
         (el/contains
           (seq
             (filter
               #(ast-clj? ?type %)
               (iterator-seq 
                 (new changenodes.comparing.BreadthFirstNodeIterator ?node))))
             ?child))])))

(defn compilationunit-packagedeclaration [?compunit ?package]
  (logic/all
    (jdt/ast :CompilationUnit ?compunit)
    (logic/project [?compunit]
      (logic/==
        (.getPackage ?compunit)
        ?package))))

(defn typedeclaration|public? [?typedeclaration]
  (logic/fresh [?modifier]
    (jdt/ast :TypeDeclaration ?typedeclaration)
    (jdt/child :modifiers ?typedeclaration ?modifier)
    (modifier|public? ?modifier)))

(defn typedeclaration-string|named [?typedeclaration ?string]
  (logic/fresh [?name]
    (jdt/ast :TypeDeclaration ?typedeclaration)
    (jdt/has :name ?typedeclaration ?name)
    (jdt/name|simple-string ?name ?string)))


(defn compilationunit-typedeclaration|main [?compunit ?typedeclaration]
  (logic/fresh [?types]
    (jdt/ast :CompilationUnit ?compunit)
    (logic/project [?compunit]
      (logic/== ?types (seq (.types ?compunit)))
      (logic/membero ?typedeclaration ?types)
      (typedeclaration|public? ?typedeclaration))))

(declare method-method|same-signature method-cu-method-cu|same)

(defn compilationunit-compilationunit|corresponding [?left ?right]
  "finds the corresponding compilationunit of the given cu in the current version"
  (logic/fresh [?path ?leftname ?leftpackage ?leftpname ?leftmain 
                ?rightname ?rightmain ?rightpackage ?rightpname]
    (logic/all
      (jdt/ast :CompilationUnit ?left)
      (jdt/has :package ?left ?leftpackage)
      (jdt/has :name ?leftpackage ?leftpname)
      (compilationunit-typedeclaration|main ?left ?leftmain)
      (jdt/has :name ?leftmain ?leftname)
      (jdt/ast :CompilationUnit ?right)
      (jdt/has :package ?right ?rightpackage)
      (jdt/has :name ?rightpackage ?rightpname)
      (jdt/name-name|same|qualified ?leftpname ?rightpname)
      (compilationunit-typedeclaration|main ?right ?rightmain)
      (jdt/has :name ?rightmain ?rightname)
      (jdt/name-name|same|qualified ?leftname ?rightname))))

(defn method-cu-method-cu|same-name [?left-method left ?right-method right]
  "finds the corresponding method declaration of left in the current version"
  ;;probably better to compute changes to detect renames as well
  (logic/fresh [?left-name ?right-name]
    (child+-type left :MethodDeclaration ?left-method )
    (child+-type right :MethodDeclaration ?right-method)
    (jdt/has :name ?right-method ?right-name)
    (jdt/has :name ?left-method ?left-name)
    (jdt/name|simple-name|simple|same ?left-name ?right-name)))

(defn method-cu-method-cu|corresponding [?left-method left ?right-method right]
  "finds the corresponding method declaration of left in the current version"
  ;;probably better to compute changes to detect renames as well
  (logic/fresh [?left-name ?right-name]
    (method-cu-method-cu|same-name ?left-method left ?right-method right)
    (method-method|same-signature ?left-method ?right-method)))


(defn method-cu-cu|introduced [?right-method right left]
  "right-method is added in right-cu and was not presented in left-cu.
   currently only verifies no method with the same name was present, even though signatures may be different"
  (logic/fresh [?left-method ?left-name ?right-name]
    (child+-type right :MethodDeclaration ?right-method)
    (damp.ekeko.logic/fails
      (method-cu-method-cu|same left ?left-method right ?right-method))))

(defn ast-ast|same [?left ?right]
  "Succeeds when both ast nodes are the same.
   For performance reasons make sure both nodes are grounded."
  (logic/all
    (jdt/ast :ASTNode ?left)
    (jdt/ast :ASTNode ?right)
    (logic/project [?left ?right]
      (logic/== true (match? ?left ?right)))))

(defn ast-ast|different [?left ?right]
  "Succeeds when both ast nodes are different.
   For performance reasons make sure both nodes are grounded."
  (logic/all
    (jdt/ast :ASTNode ?left)
    (jdt/ast :ASTNode ?right)
    (logic/project [?left ?right]
      (logic/== false (match? ?left ?right)))))

(defn method-cu-method-cu|modified [?left-method left ?right-method right]
  (logic/all
    (method-cu-method-cu|corresponding ?left-method left ?right-method right)
    (ast-ast|same ?left-method ?right-method)))


;;similarity
(defn levenshtein [left right]
  (org.apache.commons.lang3.StringUtils/getLevenshteinDistance
    (.toString left)
    (.toString right)))

(defn levenshtein-normalized [left right]
  (let [lstring (.toString left)
        rstring (.toString right)
        maxlength (max (count lstring) (count rstring))]
    (/ (org.apache.commons.lang3.StringUtils/getLevenshteinDistance
         lstring rstring)
      maxlength)))


(defn ast-ast|levenshtein [?left ?right ?levenshtein]
  (logic/project [?left ?right]
    (logic/== ?levenshtein (levenshtein ?left ?right))))


(defn ast-ast|levenshtein-normalized [?left ?right ?levenshtein]
  (logic/project [?left ?right]
    (logic/== ?levenshtein (levenshtein-normalized ?left ?right))))

(defn usim [left right]
  (defn clean-string [str]
    (clojure.string/replace str #"\s{2,}" " ")) ;;incorrect for code that contains strings that contain spaces/newlines, oh well 
  (let [lstring (.toString left)
        rstring (.toString right)
        lclean (clean-string lstring)
        rclean (clean-string rstring)
        mlen (max (count lclean) (count rclean))]
    (/ (- mlen (levenshtein lclean rclean)) mlen)))


(defn usim-similar? [left right]
  (>= (usim left right) 0.65)) ;;0.65 taken from http://sel.ist.osaka-u.ac.jp/~lab-db/betuzuri/archive/921/921.pdf
    
    
(defn ast-ast|usim-similar [left right]
  (logic/project [left right]
    (logic/== true (usim-similar? left right))))

(defn method-method|clones [?methodA ?methodB]
  (logic/fresh [?bodyA ?bodyB]
    (jdt/ast :MethodDeclaration ?methodA)
    (jdt/ast :MethodDeclaration ?methodB)
    (logic/!= ?methodA ?methodB)
    (jdt/has :body ?methodA ?bodyA)
    (jdt/has :body ?methodB ?bodyB)
    (ast-ast|usim-similar ?bodyA ?bodyB)))

(defn type-type|same [?typeA ?typeB]
  "Checks whether both types have the same name.
   Could be done smarter by using bindings, but these are not always available"
  (logic/fresh [?nameA ?nameB]
    (jdt/ast :Type ?typeA)
    (jdt/ast :Type ?typeB)
    (jdt/has :name ?typeA ?nameA)
    (jdt/has :name ?typeB ?nameB)
    (jdt/name|simple-name|simple|same ?nameA ?nameB)))


(defn vardecl-vardecl|same-type [?varA ?varB]
  (logic/fresh [?typeA ?typeB]
    (jdt/ast :SingleVariableDeclaration ?varA)
    (jdt/ast :SingleVariableDeclaration ?varB)
    (jdt/has :type ?varA ?typeA)
    (jdt/has :type ?varB ?typeB)
    (type-type|same ?typeA ?typeB)))

(comment
  (logic/fresh [?headA ?restA ?headB ?restB]
    (logic/conda 
      [(logic/emptyo ?signatureA)
       (logic/emptyo ?signatureB)]
      [(logic/conso ?headA ?restA ?signatureA)
       (logic/conso ?headB ?restB ?signatureB)
       (vardecl-vardecl|same-type ?headA ?headB)
       (signaturelist-signaturelist|same ?restA ?restB)])))

(defn signaturelist-signaturelist|same [?signatureA ?signatureB]
  "Non-relational. Both arguments are a list containing SingleVariableDeclarations.
   Checks whether both lists have the same types for each argument."
  (defn signatures-same? [sigA sigB]
    (if (not= (count sigA) (count sigB))
      false
      (every? (fn [[a b]] 
               (= 
                 (astnode/value-unwrapped (->> a
                                   (has-clj :type)
                                   (has-clj :name)
                                   (has-clj :identifier)))
                 (astnode/value-unwrapped (->> b
                           (has-clj :type)
                           (has-clj :name)
                           (has-clj :identifier)))))
        (zipmap ?signatureA ?signatureB))))
  (logic/project [?signatureA ?signatureB]
    (logic/== true (signatures-same? ?signatureA ?signatureB))))


(defn method-method|same-signature [?methodA ?methodB]
 (logic/fresh [?signatureA ?signatureB]
  (jdt/ast :MethodDeclaration ?methodA)
  (jdt/ast :MethodDeclaration ?methodB)
  (jdt/has :parameters ?methodA ?signatureA)
  (jdt/has :parameters ?methodB ?signatureB)
  (logic/project [?signatureA ?signatureB]
    (signaturelist-signaturelist|same (seq (astnode/value-unwrapped ?signatureA)) (seq (astnode/value-unwrapped ?signatureB))))))