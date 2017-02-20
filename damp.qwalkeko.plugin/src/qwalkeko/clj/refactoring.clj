(ns qwalkeko.clj.refactoring)


(defn method-method|extracted [left ?left-method right ?extracted-method]
  (logic/fresh [?right-name ?new-left]
    ;(ast/compilationunit-compilationunit|corresponding left ?new-left)
    ;(ast/method-cu-method-cu left ?left-method ?new-left ?new-left-method)
    (ast/method-cu-cu|introduced right ?right-method left)
    (ast/method-cu-method-cu|same ?left-method left ?new-left right)
    (ast/child+-type ?new-left :MethodInvocation ?extracted-call)
    (jdt/has :name ?extracted-call ?call-name)
    (jdt/has :name ?right-method ?right-name)
    (jdt/name|simple-name|simple|same ?call-name ?right-name)
    ;;verify that part of the code has been moved to the body
    
    ))


(defn statements-extracted-from-body [source target]
  (logic/fresh [source-statements target-statements]
    (jdt/has :body source source-body)
    (jdt/has :body target target-body)
    (jdt/has :statements source-body source-statements)
    ;;take subset of source-statements
    
    ))