(+ 1 2 3)
"Hello World"

(defn func [x]
  (+ x 4))

(func 3)

(assert true)

;; Protocols (ExprC?!?)
(defprofocol ExprC)

(deftype numC [n]
  ExprC)
