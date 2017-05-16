(ns qwalkeko.experiments.tpvision
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt.astnode :as astnode])
  (:require [damp.ekeko.logic :as el])
  (:require [qwalkeko.clj.logic :as l])
  (:require [qwalkeko.clj.ast :as ast])
  (:require [damp.qwal :as qwal])
  (:require [qwalkeko.clj.graph :as graph])
  (:require [qwalkeko.clj.functionalnodes :as changes])
  (:require [qwalkeko.clj.graph-algo :as algo])
  (:require [qwalkeko.clj.changenavigation :as nav])
  (:require [damp.ekeko.jdt
             [ast :as jdt]]))


;;mheg


(def a-model (first (damp.ekeko.ekekomodel/all-project-models)))
(def a-graph (graph/convert-model-to-graph a-model))
(def a-root (first (:roots a-graph)))


(defn find-version [g id]
  (first
    (filter #(.startsWith (graph/revision-number %) id) (:versions g))))

(def bcepg-projects
  (list
    ["2b31374926c88ff688e9f3d760950f5216ecda97" "2071620650ff77c44c3a2c48cca3ad0127045424" "RecordingsManager"]))

(def installer-projects
  (list
    [""

(defn graph-project [graph [pre-ref ref fname]]
  (let [pre-refactored   
        (find-version graph pre-ref)
        refactored
        (find-version graph ref)
        left
        (first
         (l/qwalkeko 1 [?left]
           (qwal/qwal graph pre-refactored pre-refactored []
             (l/in-source-code [curr]
               (logic/fresh [?typedecl ?tname]
                 (jdt/ast :CompilationUnit ?left)
                 (ast/compilationunit-typedeclaration|main ?left ?typedecl)
                 (jdt/has :name ?typedecl ?tname)
                 (jdt/name|simple-string ?tname fname))))))
        right
        (first
          (l/qwalkeko 1 [?left]
            (qwal/qwal graph refactored refactored []
              (l/in-source-code [curr]
                (logic/fresh [?typedecl ?tname]
                  (jdt/ast :CompilationUnit ?left)
                  (ast/compilationunit-typedeclaration|main ?left ?typedecl)
                  (jdt/has :name ?typedecl ?tname)
                  (jdt/name|simple-string ?tname fname))))))
        nav-graph
        (nav/ast-ast-navigatable-graph left right)]
    nav-graph))


;;wrap optionsfocuschange(OPTIONS_SHOW_ALL)
;;in a handler
;;new Handler().post(new Runnable() {
;;					@Override
;;					public void run() {
;; }

(defn methodinvocation|optionsfocus [?node]
  (logic/fresh [?name]
    (jdt/ast :MethodInvocation ?node)
    (jdt/has :name ?node ?name)
    (jdt/name|simple-string ?name "optionsfocuschange")))

(defn modifier|public [?node]
  (logic/all
    (jdt/ast :Modifier ?node)
    (logic/project [?node]
      (logic/== true (.isPublic ?node)))))

(defn methoddeclaration|run [?node]
  (logic/fresh [?name ?modifier]
    (jdt/ast :MethodDeclaration ?node)
    (jdt/has :name ?node ?name)
    (jdt/name|simple-string ?name "run")
    (jdt/child :modifiers ?node ?modifier)
    (jdt/ast :Modifier  ?modifier)
    (modifier|public ?modifier)))

(defn classinstance|runnable [?node]
  (logic/fresh [?type ?name]
    (jdt/ast :ClassInstanceCreation ?node)
    (jdt/has :type ?node ?type)
    (jdt/has :name ?type ?name)
    (jdt/name|simple-string ?name "Runnable")))
    

(defn optionsfocus|wrapped-in-runnable [?options]
  (logic/fresh [?expr ?block ?run ?anonymous ?class]
    (methodinvocation|optionsfocus ?options)
    (jdt/ast-parent ?options ?expr)
    (jdt/ast-parent ?expr ?block)
    (jdt/ast-parent ?block ?run)
    (methoddeclaration|run ?run)
    (jdt/ast-parent ?run ?anonymous)
    (jdt/ast-parent ?anonymous ?class)
    (classinstance|runnable ?class)))
    
(defn optionsfocus|not-wrapped-in-runnable [?options]
  (logic/all
    (methodinvocation|optionsfocus ?options)
    (el/fails
      (optionsfocus|wrapped-in-runnable ?options))))

(defn count-optionfocus [ast]
  (count
    (logic/run* [?optionfocus]
      (ast/child+-iter ast ?optionfocus)
      (methodinvocation|optionsfocus ?optionfocus)
      (optionsfocus|wrapped-in-runnable ?optionfocus))))
    
(defn optionsfocus-checker [graph]
  (let [left (:left graph)
        curr (first (:asts graph))]
    (=
      (count-optionfocus (:right graph))
      (count-optionfocus curr))))
    
    
(defn minimize-solution [graph end-state f]
  (defn validate-changes [solution]
    (let [ordered (qwalkeko.clj.graph-algo/solution-ordered graph solution)
          real-changes (map #(nth (:changes graph) %) ordered)
          new-graph (reduce
                      nav/change-apply
                      graph
                      real-changes)]
      (f new-graph)))
  (defn select-changes [applied-ids solution]
    (if (empty? applied-ids)
      solution
      (let [remove-id (first applied-ids)
            dependencies (conj (nav/graph-change-dependents-recursive graph remove-id) remove-id)
            new-solution (remove (fn [x] (some #{x} dependencies)) solution)
            new-applied (remove (fn [x] (some #{x} dependencies)) applied-ids)]
        (if (validate-changes new-solution) ;;we can remove that change and its dependencies
          (recur new-applied new-solution)
          (recur (rest applied-ids) solution)))))
  (let [applied-ids (filter #(nth (:applied end-state) %) (range (count (:changes end-state))))]
    (select-changes applied-ids applied-ids)))



(defn perform-benchmark [left right checker]
  (let [nav-graph (time (nav/ast-ast-navigatable-graph left right))
        sorted (qwalkeko.clj.graph-algo/topo-sort-graph nav-graph)
        solution (time (reduce nav/change-apply nav-graph (map #(nth (:changes nav-graph) %) sorted)))]
    (time (minimize-solution nav-graph solution checker))))

(logic/run 1 [?move]
     (logic/fresh [?subj ?meh]
       (logic/membero ?move (seq (:changes nav-graph)))
       (changes/change|move ?move)
       (changes/change-copy ?move ?subj)
       (jdt/child+ ?subj ?meh)
       (methodinvocation|optionsfocus ?meh)))

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))


(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (mean [bottom-val top-val])))))


(defn max-distance [sol]
  (let [pairs (partition 2 1 sol)]
    (map (fn [[x y]] (- y x)) pairs)))

(defn do-more-graph-stuff [nav-graph]
  (let [paths (qwalkeko.clj.graph-algo/paths nav-graph)
        longest (apply max paths)
        medpaths (median paths)
        roots (count (:roots nav-graph))]
    [longest medpaths roots]))

;;Handler
"Elapsed time: 15634.742931 msecs"
"Elapsed time: 10413.594727 msecs"
"Elapsed time: 462048.167741 msecs"
(4 29 66 122 154 213 238 240 251 252 253 254 255 256 258 281 283 285 287 289 291 
  293 294 295 296 297 298 302 303 304 305 306 307 310 312 314 316 318 320 322 324 326 328 
  330 332 333 334 335 336 337 338)
(212 63 379 209 253 386 207 30 211 381 203 254 378 5 255 339 200 202 371 236 2 152 382 3 252 
  201 155 198 374 6 369 61 231 366 368 376 367 375 257 365 308 358 156 91 235 241 214 256 208 
  363 251 234 356 92 232 197 204 4 300 274 387 360 354 69 1 60 205 59 153 210 377 233 370 273
  62 299 357 0 8 67 147 114 18 10 288 16 26 14 283 112 289 9 115 11 68 19 383 35 350 23 107 12 
  286 17 148 34 261 25 285 280 28 111 93 301 348 151 29 243 384 150 347 380 22 13 33 108 117 262
  284 32 113 31 21 281 116 242 15 290 349 149 206 385 24 27 20 7 110 282 287 291 49 37 199 42 71
  372 296 52 73 38 53 45 57 76 47 373 66 276 51 295 64 44 43 41 36 109 56 294 40 75 50 48 54 293
  260 106 77 46 39 297 55 58 72 355 259 298 74 70 65 275 84 177 94 80 96 87 124 173 79 120 81 
  123 132 78 83 179 97 82 302 125 306 122 131 351 89 118 303 90 172 304 305 174 307 176 95 119
  88 85 352 86 130 121 362 318 105 194 98 126 320 321 140 315 326 138 324 145 215 127 180 142 330 
  332 312 325 103 323 100 313 310 316 328 178 181 317 129 314 193 331 309 99 327 137 141 192 144 
  329 319 102 101 322 175 154 128 311 161 336 160 169 171 163 133 333 346 337 335 158 359 228 146
  166 170 220 227 248 334 134 162 196 195 167 143 168 338 223 139 136 159 361 221 222 157 165 135
  224 213 164 190 183 185 268 265 266 344 277 230 342 340 189 184 238 187 191 237 278 341 226 182 
  267 345 239 225 269 343 229 240 186 249 246 271 270 272 279 250 258 219 216 364 353 217 188 218
  104 292 247 244 245 264 263)
635
652
(25 37 56 32 59 25 2 11 1 1 1 1 1 2 23 2 2 2 2 2 2 1 1 1 1 1 4 1 1 1 1 1 3 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1) 2
[10 3 78]
