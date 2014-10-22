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



(defn compilationunit-typedeclaration|main [?compunit ?typedeclaration]
  (logic/fresh [?types]
    (jdt/ast :CompilationUnit ?compunit)
    (logic/project [?compunit]
      (logic/== ?types (seq (.types ?compunit)))
      (logic/membero ?typedeclaration ?types)
      (typedeclaration|public? ?typedeclaration))))


(defn compilationunit|corresponding [ast ?compunit]
  "finds the corresponding compilationunit of the ast in the current version"
  (logic/fresh [?root ?path ?leftname ?leftpackage ?leftpname ?leftmain 
                ?rightname ?rightmain ?rightpackage ?rightpname]
    (logic/project [ast]
      (jdt/ast-root ast ?root)
      (jdt/has :package ast ?leftpackage)
      (jdt/has :name ?leftpackage ?leftpname)
      (compilationunit-typedeclaration|main ?root ?leftmain)
      (jdt/has :name ?leftmain ?leftname)
      (jdt/ast :CompilationUnit ?compunit)
      (jdt/has :package ?compunit ?rightpackage)
      (jdt/has :name ?rightpackage ?rightpname)
      (jdt/name-name|same|qualified ?leftpname ?rightpname)
      (compilationunit-typedeclaration|main ?compunit ?rightmain)
      (jdt/has :name ?rightmain ?rightname)
      (jdt/name-name|same|qualified ?leftname ?rightname))))


(defn methoddeclaration|corresponding [left ?right]
  "finds the corresponding method declaration of left in the current version"
  ;;probably better to compute changes to detect renames as well
  (logic/fresh [?left-comp ?right-comp ?left-name ?right-name]
    (logic/project [left]
      (jdt/ast-root left ?left-comp)
      (compilationunit|corresponding ?left-comp ?right-comp)
      (jdt/child+ ?right-comp ?right)
      (jdt/ast :MethodDeclaration ?right)
      (jdt/has :name ?right ?right-name)
      (jdt/has :name left ?left-name)
      (jdt/name|simple-name|simple|same ?left-name ?right-name))))

(defn methoddeclaration|added [left-cu ?right-method]
  "right-method is added in the corresponding compilation unit of left-cu in the current version.
   currently only verifies no method with the same name was present, even though signatures may be different"
  (logic/fresh [?left-method ?left-name ?right-name]
    (jdt/ast :MethodDeclaration ?right-method)
    (jdt/has :name ?right-method ?right-name)
    (damp.ekeko.logic/fails
      (logic/all
        (jdt/child+ left-cu ?left-method)
        (jdt/ast :MethodDeclaration ?left-method)
        (jdt/has :name ?left-method ?left-name)
        (jdt/name|simple-name|simple|same ?left-name ?right-name)))))


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
                 (:value (->> a
                           (has-clj :type)
                           (has-clj :name)
                           (has-clj :identifier)))
                 (:value (->> b
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
    (signaturelist-signaturelist|same (seq (:value ?signatureA)) (seq (:value ?signatureB))))))