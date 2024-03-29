(ns tests
	(:use [clojure.test]))

(load-file "asgn6.clj")

;;;;;;;;;;;;;;;;;;;
;;; PARSE TESTS ;;;
;;;;;;;;;;;;;;;;;;;

(is (= (asgn6/parse '1) (asgn6.numC. 1)))
(is (= (asgn6/parse 'true) (asgn6.boolC. true)))
(is (= (asgn6/parse 'x) (asgn6.idC. 'x)))
(is (= (asgn6/parse '(if true 1 2)) (asgn6.ifC. (asgn6.boolC. true) (asgn6.numC. 1) (asgn6.numC. 2))))

;;;;;;;;;;;;;;;;;;;;;;
;;; TOP-EVAL TESTS ;;;
;;;;;;;;;;;;;;;;;;;;;;

(is (= (asgn6/top-eval '(if true 1 2)) "1"))
(is (= (asgn6/top-eval '(if false 1 2)) "2"))
(is (= (asgn6/top-eval '((fn (x) (+ x 1)) 2)) "3"))
(is (= (asgn6/top-eval '(fn (x) x)) "#<procedure>"))
(is (= (asgn6/top-eval '(+ 1 2)) "3"))
(is (= (asgn6/top-eval '(with (x = 2) ((fn (y) (+ y 1)) x))) "3"))
(is (= (asgn6/top-eval '((fn (x y) x) 1)) "1"))
(is (= (asgn6/top-eval '((fn (x y) (+ x y)) 1 2)) "3"))
(is (= (asgn6/top-eval '(with (x = 2) (y = 5) (+ x y))) "7"))

(is (= (asgn6/top-eval '5) "5"))
(is (= (asgn6/top-eval 'true) "true"))
(is (= (asgn6/top-eval 'false) "false"))
(is (= (asgn6/top-eval '(* 4 2)) "8"))
(is (= (asgn6/top-eval '(+ 1 2)) "3"))
(is (= (asgn6/top-eval '(- 5 4)) "1"))
(is (= (asgn6/top-eval '(+ 3 7)) "10"))
(is (= (asgn6/top-eval '(/ 6 2)) "3"))
(is (= (asgn6/top-eval '(<= 3 1)) "false"))
(is (= (asgn6/top-eval '(<= -1 1)) "true"))
(is (= (asgn6/top-eval '(* (+ 1 1) (- 6 (/ 4 2)))) "8"))


(is (= (asgn6/top-eval '(if false 2 3)) "3"))
(is (= (asgn6/top-eval '(if true 1 2)) "1"))
(is (= (asgn6/top-eval '(if true (+ 5 4) 2)) "9"))
(is (= (asgn6/top-eval '(if false (+ 5 4) (/ 6 2))) "3"))

(is (= (asgn6/top-eval '(eq? 1 2)) "false"))
(is (= (asgn6/top-eval '(eq? false true)) "false"))
(is (= (asgn6/top-eval '(eq? true true)) "true"))
(is (= (asgn6/top-eval '(eq? (<= (+ 1 2) -5) (<= 2 1))) "true"))
;(is (= (asgn6/top-eval '(eq? (fn () 7) (fn () 7))) "false"))
(is (= (asgn6/top-eval '(eq? 1 (<= 1 2))) "false"))

(is (= (asgn6/top-eval '(fn () 4)) "#<procedure>"))
(is (= (asgn6/top-eval '((fn () 4))) "4"))
(is (= (asgn6/top-eval '((fn () (+ ((fn (x y) (+ x y)) 1 (+ 1 1)) 3)))) "6"))
(is (= (asgn6/top-eval '((fn (x) x) -1) ) "-1"))
(is (= (asgn6/top-eval '((fn (x) (+ x (/ 50 x))) 10)) "15"))
(is (= (asgn6/top-eval '((fn (x y) (+ x y)) 3 6)) "9"))
(is (= (asgn6/top-eval '((fn (z y) (+ z y)) (+ 9 14) 98)) "121"))
(is (= (asgn6/top-eval '((fn (x y) (if (<= x 0) (if (<= y 0) -1 y) x)) 4 1)) "4"))
(is (= (asgn6/top-eval '((fn (x y) (if (<= x 0) (if (<= y 0) -1 y) x)) 0 3)) "3"))
(is (= (asgn6/top-eval '((fn (x y) (if (<= x 0) (if (<= y 0) -1 y) x)) 0 0)) "-1"))
(is (= (asgn6/top-eval '((fn (a b c d) (+ (+ a b) (+ c d))) 1 2 3 4)) "10"))

(is (= (asgn6/top-eval '(+ 1 ((fn () 4)))) "5"))
(is (= (asgn6/top-eval '(+ ((fn () 4)) ((fn () 4)))) "8"))
(is (= (asgn6/top-eval '(+ ((fn () 4)) ((fn (x y) (+ x y)) 3 6))) "13"))
(is (= (asgn6/top-eval '((fn (g h) ((fn (g h) (+ g h)) 2 2)) 3 3)) "4"))
(is (= (asgn6/top-eval '((fn (x y) ((fn (n) (+ n x)) 1)) 2 3)) "3"))
(is (= (asgn6/top-eval '(((fn (x y) (fn (n) (+ n x))) 1 2) 3)) "4"))

(is (= (asgn6/top-eval '(with (z = 6) (+ z 10))) "16"))
(is (= (asgn6/top-eval '(with (z = 7) (y = 9) (+ z y))) "16"))
(is (= (asgn6/top-eval '(with (z = (+ 9 14)) (y = 98) (+ z y))) "121"))

(is (thrown? IllegalArgumentException (asgn6/top-eval '(+ 1 (fn () 4)))))
(is (thrown? IllegalArgumentException (asgn6/top-eval '(* 1 (fn () 5)))))
(is (thrown? IllegalArgumentException (asgn6/top-eval '(/ 1 (fn () 6)))))
(is (thrown? IllegalArgumentException (asgn6/top-eval '(- 1 (fn () 7)))))
(is (thrown? IllegalArgumentException (asgn6/top-eval '(<= 1 (fn () 7)))))
(is (thrown? Exception (asgn6/top-eval '((fn (x y) (+ x y)) 1))))
(is (thrown? Exception (asgn6/top-eval '((lambda (y) (+ x 10)) 10))))

;(is (thrown? Exception (asgn6/top-eval '(fn (x x) 3))))
;(is (thrown? Exception (asgn6/top-eval '((fn () 4) 1))))
;(is (thrown? Exception (asgn6/top-eval '((fn (x y) (if x (if y -1 y) x)) 0 3 4))))

