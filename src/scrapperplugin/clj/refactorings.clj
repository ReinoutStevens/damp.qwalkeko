(ns scrapperplugin.clj.refactorings
  (:require [scrapperplugin.clj.logic])
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt.reification :as jdt])
  (:require [damp.qwal :as qwal]))
 
;;http://users.ece.utexas.edu/~miryung/Publications/TR.refactoringcatalog081410.pdf




deleted method(mFullName, mShortName,
t1FullName) 
^ added method(newmFullName,
mShortName, t2FullName) 
^ similarbody(newmFullName, newmBody, mFullName,
mBody) 
^ NOT(equals(t1FullName, t2FullName))
! move method(mShortName, t1Fullname,
t2FullName)



(defn is-method-in-class [?method ?class]
  (conda
    [(v+ ?method)
     (conda [(v+ ?class)
             (is-classo? ?class)
             (jdt/child :MethodDeclaration ?class ?method)]
            [(v- ?class)
             (project [?method]
                      (== ?class (get-type-class ?method))
                      (is-classo? ?class))])] 
    [(v- ?method)
     (is-classo? ?class)
     (jdt/child :MethodDeclaration ?class ?method)]))



(defn is-class-in-package? [?name ?package]
  (fresh [?compunit ?package]
         (is-compilationunito? ?compunit)
         (logic/project [?compunit]
                  (let [package (.getPackage ?compunit)]
                    (if (nil? package)
                      (logic/== ?package-name nil)
                      (logic/== ?package-name (.getFullyQualifiedName (.getName package))))
                    (jdt/child :TypeDeclaration package ?class)
                    (is-classo? ?class)))))


(defn is-anonymous? [x]
  (instance? org.eclipse.jdt.core.dom.AnonymousClassDeclaration x))

(defn is-method? [x]
  (instance? org.eclipse.jdt.core.dom.MethodDeclaration x))


(defn is-methodo? [?x]
  (jdt/ast :MethodDeclaration ?x))



(defn is-classo? [?x]
  (all
    (jdt/ast :TypeDeclaration ?x)
    (logic/project [?x]
                   (== false (.isInterface ?x)))))


(defn is-compilationunito? [?x]
  (jdt/ast :CompilationUnit ?x))




;;GOALS  
(defn gmethod-not-introduced [?method]
  (fn [graph current next]
    (logic/fresh [?class ?othermethod]
                 (qwal/all-goals
                   (scurrent [curr]
                             (is-methodo-in-class ?method))
                   q<=
                   (scurrent [curr]
                             (is-method-in-class-in-package ?othermethod)
                             (== ?method ?othermethod)
                             (is-methodo ?method))
                   (logic/== current next)))))


(defn gmethod-introduced [?method]
  (fn [graph current next]
    (logic/fresh [?class]
                 (qwal/all-goals
                   (scurrent [curr]
                             (is-methodo ?method)
                             (get-body ?method )
                     )
                   q<=
                   (scurrent [curr]
                             (is-methodo ?method)
                             (get-body ?method )
                             )
                   (logic/== current next)
                   (get-body ?method)))))
    
    
  
