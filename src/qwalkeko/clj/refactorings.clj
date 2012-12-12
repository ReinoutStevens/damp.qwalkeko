(ns scrapperplugin.clj.refactorings
  (:require [qwalkeko.clj.logic])
  (:use [qwalkeko.clj.protocols])
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt.reification :as jdt])
  (:require [damp.qwal :as qwal]))
 
;;http://users.ece.utexas.edu/~miryung/Publications/TR.refactoringcatalog081410.pdf




(comment
  "
eleted method(mFullName, mShortName,
t1FullName) 
^ added method(newmFullName,
mShortName, t2FullName) 
^ similarbody(newmFullName, newmBody, mFullName,
mBody) 
^ NOT(equals(t1FullName, t2FullName))
! move method(mShortName, t1Fullname,
t2FullName)
   ")

                 

(defn compare-strings [str1 str2]
  (let [regexstr "\\s"
        filtered1 (.replaceAll str1 regexstr "")
        filtered2 (.replaceAll str2 regexstr "")]
    (- 1 
      (/
        (org.apache.commons.lang.StringUtils/getLevenshteinDistance filtered1 filtered2)
        (max (count filtered1) (count filtered2))))))

(defn has-similar-body 
  ([method1 method2]
   (has-similar-body method1 method2 (/ 9 10)))
  ([method1 method2 tolerance]
   (logic/project [method1 method2]
            (logic/== true
                (<= (compare-strings (.toString (.getBody method1))
                                     (.toString (.getBody method2)))
                    tolerance)))))




(defn method-moved [?moved ?to]
  (logic/all
    (is-removed ?moved)
    (jdt/ast :MethodDeclaration ?to)
    (logic/== ?moved ?to) ;;same signature
    (has-similar-body ?moved ?to)))


(defn superclass [?class ?super]
  (defn superclass-helper [class-a class-b]
    (if-not (nil? class-a)
      (or (= (.getQualifiedName class-a)
             (.getQualifiedName class-b))
          (recur (.getSuperclass class-a) class-b))
      false))
  (logic/all
     (jdt/ast :TypeDeclaration ?class)
     (jdt/ast :TypeDeclaration ?super)
     (logic/project [?class ?super]
                    (logic/== true
                              (superclass-helper ?class ?super)))))



(defn pulled-up [?method ?pulled]
  (logic/all
    (method-moved ?method ?pulled)
    (logic/project [?method ?pulled]
             (superclass (.getDeclaringClass (.resolveBinding ?method))
                         (.getDeclaringClass (.resolveBinding ?pulled))))))


(run* [?method ?pulled]
  (fresh [?end]
    (qwal graph root ?end []
      (q=>*) ;;skip arbitrary no versions
      (qcurrent [curr] ;;method in version
        (ast :MethodDeclaration ?method))
      qwal/q=> ;;go to next version
      (qcurrent [curr]
        (ast :MethodDeclaration ?pulled)
        (pulled-up ?method ?pulled)))))

(comment 
(defn working-together [?authorA ?authorB]
              (logic/fresh [ ?end ?authorA ?authorB]
                           (qwal/qwal graph root ?end []
                                      (qwal/q=>*)
                                (qwal/qtimes 4
                                               (qwal/qcurrent [curr]
                                                             (logic/== ?authorA (.getAuthor curr)))
                                               qwal/q=>
                                                (qwal/qcurrent [curr]
                                                              (logic/== ?authorB (.getAuthor curr)))
                                                qwal/q=>))
                    (logic/== ?authors (list ?authorA ?authorB)))))



(logic/run* [?methodA ?methodB]
            (fresh [?end]
                   (qwal graph root ?end []
                         (qcurrent [curr]
                                   (ast :MethodDeclaration ?methodA))
                         q=>
                         (qcurrent [curr]
                                   (ast :MethodDeclaration ?methodB)
                                   (fails
                                     (same ?methodA ?methodB))))))