(ns qwalkeko.clj.ast
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt
             [ast :as jdt]]))

;;reification of Java functions
(defn compilation-unit? [astnode]
  (= (.getNodeType astnode) org.eclipse.jdt.core.dom.ASTNode/COMPILATION_UNIT))

(defn get-ast-path [ast]
  (let [root (.getRoot ast)]
    (when (compilation-unit? root)
      (let [javaelement (.getJavaElement root)] ;;should be a org.eclipse.jdt.internal.core.CompilationUnit
        (.removeFirstSegments (.getPath javaelement) 1)))))


(defn string-to-path [string]
  (new org.eclipse.core.runtime.Path string))

(defn path-to-string [path]
  (.toString path))

;;logicify them
(defn path-string [?path ?string]
  (logic/all
    (logic/conde
      [(logic/nonlvaro ?path)
       (logic/project [?path]
         (logic/== ?string (path-to-string ?path)))]
      [(logic/nonlvaro ?string)
       (logic/project [?string]
         (logic/== ?path (string-to-path ?string)))])))

(defn ast-path [?ast ?path]
  (logic/all
    (jdt/ast :ASTNode ?ast)
    (logic/project [?ast]
      (logic/== ?path (get-ast-path ?ast)))))


;;As we manually parse files we can no longer rely on paths to retrieve them
;;Instead we'll use package name + name of the class

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
