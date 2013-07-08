(ns qwalkeko.clj.graphviz
  (:require [clojure.java.io :as io])
  (:require [qwalkeko.clj.reification :as r]))



(defn output-version [identifiers a-version]
  (let [identifier (get identifiers a-version)
        rev-no (.getRevisionNumber a-version)
        label (str 
                (apply str (take 4 rev-no))
                "..."
                (apply str (drop (- (count rev-no) 4) rev-no)))]
    (apply str
           (map #(str identifier " -> " %1 " [ label = \"" label "\" ];\n")
                (map #(get identifiers %1) (r/successors a-version))))))
  

(defn output-to-graphviz [project output]
  (let [versions (.getVersions project)
        identifiers (reduce #(assoc %1 %2 (count %1))  {} versions)
        text
        (str
        "digraph " (.getName project) " {\n"
        "rankdir=LR;\n"
        "node [shape = circle]; "
        (apply str (map #(str %1 " ") (map #(get identifiers %) versions)))";\n"
        (apply str (map #(output-version identifiers %1) versions))
        "}"
        )]
    (with-open [stream output]
      (.write stream text))))


(defn output-to-file [project location]
  (let [stream (io/writer location)]
    (output-to-graphviz project stream)))


