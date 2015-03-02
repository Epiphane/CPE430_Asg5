(declare parse)
(declare interp)

(defn testType [result expType]
  (cond
    (= (type result) expType) "GOOD"
    :else "=====BAD====="))
(defn testValue [result expected]
  (cond
    (= result expected) "GOOD"
    :else "=====BAD====="))

;;; Expression datatype
(deftype ExprC [])
(deftype numC [val])
(deftype boolC [val])
(deftype idC [id])
(deftype binC [op a b])
(deftype ifC [cond truecase falsecase])
(deftype fnC [params body])
(deftype appC [function arg])
(derive ::numC ::ExprC)
(derive ::boolC ::ExprC)
(derive ::idC ::ExprC)
(derive ::binC ::ExprC)
(derive ::ifC ::ExprC)
(derive ::fnC ::ExprC)
(derive ::appC ::ExprC)

;;; Value datatype
(deftype Value [])
(deftype numV [val])
(deftype boolV [val])
(deftype closV [params body env])
(derive ::numV ::Value)
(derive ::boolV ::Value)
(derive ::closV ::Value)

;;; Binary operation hashmap
(def binops (hash-map '+ (fn [a b] (numV. (+ a b))),
             '- (fn [a b] (numV. (- a b))),
             '* (fn [a b] (numV. (* a b))),
             '/ (fn [a b] (numV. (/ a b))),
             '<= (fn [a b] (numV. (<= a b))),
             'eq? (fn [a b] (boolV. (= a b)))))

;;; Parses a list of values and returns a list of ExprCs.
(defn parseList [l]
  (cond
    (empty? l) (empty '((numC. 1)))
    :else (cons (parse (first l)) (parseList (rest l)))))

;;; Extracts a list of parameters from a with statement.
(defn extractParams [l]
  (cond
    (empty? (rest l)) '()
    :else (cons (first (first l)) (extractParams (rest l)))))

;;; Extracts a list of arguments from a with statement.
(defn extractArgs [l]
  (cond
    (empty? (rest l)) '()
    :else (cons (parse (nth (first l) 2)) (extractArgs (rest l)))))

;;; Extracts a body from a with statement.
(defn extractBody [l]
  (cond
    (empty? (rest l)) (parse (first l))
    :else (extractBody (rest l))))

;;; Parses a form into an ExprC.
(defn parse [s]
  (cond
    (list? s)
      (cond
        (= (symbol 'if) (first s)) (ifC. (parse (nth s 1)) (parse (nth s 2)) (parse (nth s 3)))
        (= (symbol 'fn) (first s)) (fnC. (nth s 1) (parse (nth s 2)))
        (= (symbol 'with) (first s))
          (let [params (extractParams (rest s))
                args (extractArgs (rest s))
                body (extractBody s)]
            (appC. (fnC. params body) args))
        (not (nil? (get binops (first s))))
          (binC. (first s)
                 (parse (nth s 1))
                 (parse (nth s 2)))
        :else (appC. (parse (first s))
                     (parseList (rest s))))
    :else
      (cond
        (number? s) (numC. s)
        (= Boolean (type s)) (boolC. s)
        (symbol? s) (idC. s)
        :else (throw (Exception. "Unknown datatype")))))

(parseList (list '1 '2))
(extractParams '((x = 2) (y = 3) x))
(extractArgs '((x = 2) (y = 3) x))
(extractBody '(with (x = 2) (y = 3) x))

(testType (parse '1) numC)
(testValue (.val (parse '1)) 1)
(testType (parse 'true) boolC)
(testValue (.val (parse 'true)) true)
(testType (parse 'x) idC)
(testValue (.id (parse 'x)) 'x)
(testType (parse '(if true 1 2)) ifC)
(testValue (.val (.cond (parse '(if true 1 2)))) true)
(testValue (.val (.truecase (parse '(if true 1 2)))) 1)
(testValue (.val (.falsecase (parse '(if true 1 2)))) 2)

;;; Processes a list of arguments.
(defn processArgs [args env]
  (cond
    (empty? args) '()
    :else (cons (interp (first args) env) (processArgs (rest args) env))))

;;; Binds a list of parameters to a list of arguments.
(defn bind [params args env]
  (cond
    (empty? params) env
    :else (assoc (bind (rest params) (rest args) env)
                 (first params)
                 (first args))))

;;; Interprets an ExprC into a Value.
(defn interp [expr env]
  (cond
    (= numC (type expr)) (numV. (.val expr))
    (= boolC (type expr)) (boolV. (.val expr))
    (= ifC (type expr))
       (if (.val (interp (.cond expr) env))
           (interp (.truecase expr) env)
           (interp (.falsecase expr) env))
    (= idC (type expr)) (let [value (get env (.id expr))]
                             (cond
                               (nil? value) (throw (Exception. "ID not found")) 
                               :else value))
    (= binC (type expr)) (let [operation (get binops (.op expr))]
                           (cond
                             (nil? operation) (throw (Exception. "how did you do that?"))
                             :else (operation (.val (interp (.a expr) env)) (.val (interp (.b expr) env)))))
    (= fnC (type expr)) (closV. (.params expr) (.body expr) env)
    (= appC (type expr))
      (let [clos (interp (.function expr) env)]
        (cond
          (= closV (type clos))
            (interp (.body clos)
                    (bind (.params clos) (processArgs (.arg expr) env) (.env clos)))
          :else (throw (Exception. "Cannot evaluate non-function"))))
    :else (throw (Exception. "unknown expression"))))

(bind '() '() {})
(bind (list 'x 'y) (processArgs (list (numC. 1) (numC. 2)) (hash-map)) {})
(processArgs '() (hash-map))

;;; Serializes a Value into a primitive value.
(defn serialize [value]
  (cond
    (= numV (type value)) (.val value)
    (= boolV (type value)) (.val value)
    (= closV (type value)) "#<procedure>"))

(defn top-eval [s]
  (serialize (interp (parse s) (hash-map))))

(testValue (serialize (interp (idC. 'x) (assoc (hash-map) 'x (numV. 1)))) 1)

(testValue (top-eval '(if true 1 2)) 1)
(testValue (top-eval '(if false 1 2)) 2)
(testValue (top-eval '((fn (x) (+ x 1)) 2)) 3)
(testValue (top-eval '(fn (x) x)) "#<procedure>")
(testValue (top-eval '(+ 1 2)) 3)
(testValue (top-eval '(with (x = 2) ((fn (y) (+ y 1)) x))) 3)
(testValue (top-eval '((fn (x y) x) 1)) 1)
(testValue (top-eval '((fn (x y) (+ x y)) 1 2)) 3)
(testValue (top-eval '(with (x = 2) (y = 5) (+ x y))) 7)
