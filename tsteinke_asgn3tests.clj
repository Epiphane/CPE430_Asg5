;; Gucci Lang v3
;; EBNF Specification:
;; GUCI3 = num
;;       | true/false
;;       | id
;;       | (if GUCI3 GUCI3 GUCI3)
;;       | (with (id = GUCI3) ... GUCI3)
;;       | (fn (id ...) GUCI3)
;;       | (operator GUCI3 GUCI3
;;       | (GUCI3 GUCI3 ...)
;; operator = +, -, *, /, eq?, <=

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Represents an expression
(define-type ExprC
  [val (v : Value)]
  [binop (op : (number number -> number)) (l : ExprC) (r : ExprC)]
  [ifC (cond : ExprC) (success : ExprC) (fail : ExprC)]
  [appC (fun : Value) (arg : ExprC)])

;; Represents an environment
(define-type-alias Environment (hashof symbol Value))

;; the empty environment
(define empty-env (hash (list)))

;; Represents a value
(define-type Value
  [num (n : number)]
  [id (i : symbol)]
  [bool (b : boolean)]
  [lam (arg : symbol) (body : ExprC)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Test cases
(test (parse `true) (val (bool true)))
(test (parse `134) (val (num 134)))
(test (parse `abc) (val (id 'abc)))
(test (parse `(if {<= a b} 19 21)) 
      (ifC (binop <=
                 (val (id 'a))
                 (val (id 'b)))
          (val (num 19))
          (val (num 21))))
(test (parse `(with (a = (+ 4 6)) (b = 12) (* a b)))
      (appC (lam 'a
                 (appC (lam 'b
                            (binop * 'a 'b))))
            (binop + (val (num 4)) (val (num 6)))))
(test (parse `(fn {} {6})) (val (fun (val (num 6)))))

;; Exception testing
(test/exn (parse `(fn {a -} {- a -})) "Function does not match EBNF spec")
(test/exn (parse `(fn {a a} {- a a})) "Argument identifiers are not unique")
(test/exn (parse `(13)) "Syntax does not match EBNF spec")
(test/exn (parse `(13 - 10)) "Syntax does not match EBNF spec")
(test/exn (parse `(with {a = 5})) "Syntax does not match EBNF spec")
(test/exn (parse `(fn {* a a})) "Syntax does not match EBNF spec")
(test/exn (parse `(if (<= 5 10) 'a 'b 'c)) "Syntax does not match EBNF spec")
(test/exn (parse `if) "Bad if form")
(test/exn (parse `(if 1)) "Bad if form")
(test/exn (parse `(10 10 10)) "Syntax does not match EBNF spec")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Test cases
;; Environment
(define test-env (make-hash (list)))
(hash-set! test-env 'a*2b
          (interp 
           (parse `(fn {a b} {* a (* 2 b)}))))
(hash-set! test-env 'min
          (interp 
           (parse `(fn {a b} {if (<= a b) a b}))))

(define (test-interp [expr : s-expression] [res : Val])
  (test (interp (parse expr) test-env) res))
(define (test-interp/exn [expr : s-expression] [err : string])
  (test/exn (interp (parse expr) test-env) err))

;; Basic EBNF Tests
(test-interp `(if (eq? (- 10 5) 5) ab cd) (id `ab))
(test-interp `(with (fun = (fn (a b) (* a b)))
                    (val = 10)
                    (fun val 7)) (num 70))
(test-interp `(with (val = (if (<= (/ 9 3) (+ 1 1)) false true))
                    (val)) (bool false))
(test-interp `(a*2b 10 3) (num 60))

;; Exception testing
(test-interp/exn `(ab 12) "Application of a non-closure")
(test-interp/exn `(+ / 2) "Invalid binary operation")
(test-interp/exn `(+ 10 2 3) "Invalid arity")
(test-interp/exn `(if (+ 10 2) 'a 'b) "Non-boolean test in if statement")
(test-interp/exn `(bad-fn 2) "Unbound identifier")
(test-interp/exn `(+ a b) "Unbound identifier")
(test-interp/exn `(a*2b 1 2 3) "Incorrect number of arguments")
