(ns qwalkeko.clj.ast
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt
             [ast :as jdt]]))

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
    (jdt/ast :MethodDeclaraton ?methodB)
    (logic/!= ?methodA ?methodB)
    (jdt/has :body ?methodA ?bodyA)
    (jdt/has :body ?methodB ?bodyB)
    (ast-ast|usim-similar ?bodyA ?bodyB)))

