;; 1.1

;; Below is a sequence of expressions. What is the result printed by the
;; interpreter in response to each expression? Assume that the sequence
;; is to be evaluated in the order in which it is presented.

;; NOTE: Have written answers to all evaluations as comments...

10 ;; 10
(+ 5 3 4) ;; 12
(- 9 1) ;; 8
(/ 6 2) ;; 3
(+ (* 2 4) (- 4 6)) ;; 6
(define a 3) ;; #<void>
(define b (+ a 1)) ;; #<void>
(+ a b (* a b)) ;; 19
(= a b) ;; #f
(if (and (> b a) (< b (* a b)))
    b
    a) ;; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ;; 16
(+ 2 (if (> b a ) b a)) ;; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ;; 16

;; 1.2

;; Translate the following expression into prefix form:

;;      5 + 4 + (2 - (3 - (6 + 4/5)))
;;      -----------------------------
;;            3 (6 - 2)(2 - 7)

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))


;; 1.3

;; Define a procedure that takes three numbers as arguments and returns the
;; sum of the squares of the two larger numbers.

(define insert-g
  (lambda (x xs comparison)
    (cond
     ((null? xs) (cons x '()))
     ((comparison x (car xs)) (cons x xs))
     (else (cons (car xs) (insert-g x (cdr xs) comparison))))))

(define insertion-sort-g
  (lambda (xs comparison)
    (cond ((null? xs) '())
          (else (insert-g (car xs) (insertion-sort-g (cdr xs) comparison) comparison)))))

(define insertion-sort-asc
  (lambda (xs)
    (insertion-sort-g xs <)))

(define insertion-sort-desc
  (lambda (xs)
    (insertion-sort-g xs >)))

(define take
  (lambda (n xs)
    (cond
     ((or (null? xs) (zero? n)) '())
     (else (cons (car xs) (take (- n 1) (cdr xs)))))))

(define square
  (lambda (x)
    (* x x)))

;; Generic
(define sum-n-largest-sqaures-from-list
  (lambda (n xs)
    (apply + (map square (take n (reverse (insertion-sort-asc xs)))))))

(sum-n-largest-sqaures-from-list 2 '(1 5 281 492 3 2 1 0))

;; Specific
(define sum-largest-two-sqaures-from-three-args
  (lambda (n m z)
    (cond
     ((= n (min n m z)) (+ (square m) (square z)))
     (else (sum-largest-two-sqaures-from-args m z n)))))

(sum-largest-two-sqaures-from-three-args 3 2 1)

;; 1.4

;; Observe that our model of evaluation allows for combinations whose operators
;; are compound expressions. Use this observation to describe the behavior of
;; the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; Of note here is that our compound expression `(if (> b 0) + -)` must be
;; evaluated (or reduced) first by the interpreter before the rest of the
;; surrounding expression. The compound expression, then, is acting here
;; as a sub-expression, and would evalute to either:
;; (+ a b) or (- a b), depending on whether b > 0 or b < 0, respectively

;; 1.5

;; Ben Bitdiddle has invented a test to determine whether the interpreter he is
;; faced with is using applicative-order evaluation or normal-order evaluation.
;; He defines the following two procedures:

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;; Then he evaluates the expression

(test 0 (p))

;; What behavior will Ben observe with an interpreter that uses applicative-order
;; evaluation? What behavior will he observe with an interpreter that uses
;; normal-order evaluation? Explain your answer.

;; With an interpreter that uses applicative-order evaluation, we'll
;; end up in an infinite loop as the expression `(p)` within `(test 0 (p))` is
;; applied/resolved before `test` can see that indeed `x = 0' is `#t`.
;; In normal-order evaluation, or what we might call 'lazy evaluation',
;; we never arrive at the non-terminating application of `(p)' because the
;; interpreter would 'see' `x = 0` as `#t` first and return a value of `0`.

;; 1.6

;; Alyssa P. Hacker doesn't see why `if` needs to be provided as a special form.
;; "Why can't I just define it as an ordinary procedure in terms of `cond`?"
;; she asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and
;; she defines a new version of `if`:

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; Eva demonstrates the program for Alyssa:

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

;; Delighted, Alyssa uses `new-if` to rewrite the square-root program:

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

;; What happens when Alyssa attempts to use this to compute square roots?
;; Explain.

;; 1.7

;; The `good-enough?` test used in computing square roots will not be very
;; effective for finding the square roots of very small numbers.
;; Also, in real computers, arithmetic operations are almost always
;; performed with limited precision. This makes our test inadequate for
;; very large numbers. Explain these statements, with examples showing
;; how the test fails for small and large numbers. An alternative
;; strategy for implementing `good-enough` is to watch how guess
;; changes from one iteration to the next and to stop when the change
;; is a very small fraction of the guess. Design a square-root procedure
;; that uses this kind of end test. Does this work better for small or
;; large numbers?

;; 1.8

;; Newton's method for cube roots is based on the fact that if `y` is
;; an approximation to the cube root of `x`, then a better approximation
;; is given by the value:

;;         x/y^2 + 2y
;;       -------------
;;             3

;; Use this formula to implement a cube-root procedure analogous to the
;; square-root procedure.


