(ns qwalkeko.experiments.arvidchanges
  (:require [clojure.core.logic :as logic])
  (:require [qwalkeko.clj.functionalnodes :as change]
  (:require [damp.ekeko.jdt
             [ast :as jdt]])))

(defn methodinvocation|assert [?x]
  (logic/fresh [?strname]
    (jdt/ast :MethodInvocation ?x)
    (conv/methodinvocation|named ?x ?strname)
    (logic/project [?strname]
      (logic/== true (.startsWith ?strname "assert")))))

;;By.<something>(value)

(defn methodinvocation|by [?x]
  (logic/fresh [?name]
    (jdt/ast :MethodInvocation ?x)
    (jdt/child :expression ?x ?name)
    (jdt/name|simple-string ?name "By")))

;;@FindBy(something)
(defn annotation|findBy [?x]
  (logic/fresh [?name]
    (jdt/ast :NormalAnnotation ?x)
    (jdt/has :typeName ?x ?name)
    (jdt/name|simple-string ?name "FindBy")))

;;changes to PageObject
(defn classinstancecreation|pageobject [?x]
  (logic/fresh [?t ?n ?str]
    (jdt/ast :ClassInstanceCreation ?x)
    (jdt/has :type ?x ?t)
    (jdt/has :name ?t ?n)
    (jdt/name|simple-string ?n ?str)
    (logic/project [?str]
      (logic/== true (.endsWith ?str "Page")))))

;;changes to Driver
(defn assignment|driver [?ass]
  (logic/fresh [?name] 
    (jdt/ast :Assignment ?ass)
    (jdt/has :leftHandSide ?ass ?name)
    (jdt/ast :SimpleName ?name)
    (jdt/name|simple-string ?name "driver"))) ;;should use binding information (that we dont have) but it looks like driver is a common name

(defn trystatement|timeoutrelated [?try]
  (logic/fresh [?catch ?exception ?type ?name]
    (jdt/child :catchClauses ?try ?catch)
    (jdt/has :exception ?catch ?exception)
    (jdt/has :type ?exception ?type)
    (jdt/has :name ?type ?name)
    (logic/conda
      [(jdt/name|simple-string ?name "TimeOutException")]
      [(jdt/name|simple-string ?name "StaleElementReferenceException")]))) ;;perhaps more exceptions should be added here


(defn annotation|test [?annotation]
  (logic/fresh [?name]
    (jdt/ast :MarkerAnnotation ?annotation)
    (jdt/has :typeName ?annotation ?name)
    (jdt/name|simple-string ?name "Test")))

(defn annotation|ignore [?annotation]
  (logic/fresh [?name]
    (jdt/ast :MarkerAnnotation ?annotation)
    (jdt/has :typeName ?annotation ?name)
    (jdt/name|simple-string ?name "Ignore")))

(defn annotation|beforeclass [?annotation]
  (logic/fresh [?name]
    (jdt/ast :MarkerAnnotation ?annotation)
    (jdt/has :typeName ?annotation ?name)
    (jdt/name|simple-string ?name "BeforeClass")))

(defn annotation|afterclass [?annotation]
  (logic/fresh [?name]
    (jdt/ast :MarkerAnnotation ?annotation)
    (jdt/has :typeName ?annotation ?name)
    (jdt/name|simple-string ?name "AfterClass")))

(defn annotation|selenium [?annotation]
  (logic/alll
    (logic/conda
      [(annotation|test ?annotation)]
      [(annotation|ignore ?annotation)]
      [(annotation|beforeclass ?annotation)]
      [(annotation|afterclass ?annotation)]
      [(annotation|findby ?annotation)])))

(defn change-node|affects [change ?node]
  (logic/all
    (logic/conda
      [(change/insert? change)
       (change/change-original change ?node)]
      [(change/delete? change)
       (change/change-original change ?node)]
      [(change/update? change)
       (change/change-leftparent change ?node)]
      [(change/move? change)
       (logic/conde
         [(change/change-original change ?node)]
         [(change/change-rightparent change ?node)])])))

(defn ast|selenium [?ast]
  (logic/all
    (logic/conda
      [(annotation|selenium ?ast)]
      [(trystatement|timeoutrelated ?ast)]
      [(assignment|driver ?ast)]
      [(classinstancecreation|pageobject ?ast)]
      [(methodinvocation|by ?ast)]
      [(methodinvocation|assert ?ast)])))


(defn change-ast|affects-selenium [change ?ast]
  (logic/fresh [?node]
    (change-node|affects change ?node)
    (jdt/parent+ ?node ?ast)
    (ast|selenium ?ast)))
                                