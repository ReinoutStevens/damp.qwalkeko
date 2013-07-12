(ns qwalkeko.clj.graphviz
  (:require [clojure.java.io :as io])
  (:require [qwalkeko.clj.reification :as r]))



(defn output-version [identifiers labeler a-version ]
  (let [identifier (get identifiers a-version)
        successor-identifiers (map #(get identifiers %1) (r/successors a-version))
        label (labeler a-version)]
    (apply str
           identifier " [ label = \"" label "\"];\n"
           (map #(str identifier " -> " %1 ";\n") successor-identifiers))))
       


(defn output-to-graphviz [project output labeler]
  (let [versions (.getVersions project)
        identifiers (reduce #(assoc %1 %2 (count %1))  {} versions)
        text
        (str
        "digraph " (.getName project) " {\n"
        "rankdir=LR;\n"
        "node [shape = circle]; "
        (apply str (map #(str %1 " ") (map #(get identifiers %) versions)))";\n"
        (apply str (map #(output-version identifiers labeler %1) versions))
        "}"
        )]
    (with-open [stream output]
      (.write stream text))))


(defn output-to-file [project location labeler]
  (let [stream (io/writer location)]
    (output-to-graphviz project stream labeler)))


(defn version-labeler [a-version]
  (let [rev-no (str (apply str (take 5 (.getRevisionNumber a-version))) "...")
        changed-files (map #(.getName %1) (map io/file (r/changed-files a-version)))
        time (str (.getTime (.getTime a-version)))]
    (apply str
           rev-no
           " \\n "
           time
           " \\n "
           (map #(str %1 " \\n ") changed-files))))