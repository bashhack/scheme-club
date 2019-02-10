;; 1.1.1

;; Expressions (s-expressions)
(+ 42 3.14)

(- 10)

(* 7 52.143)

(/ 4.75 7.54)

(+ 1 1 2 3 5)

(+ (/ (* 2 4) 2) (- 100 10) 6)

;; 1.1.2

;; Naming
(define pi 3.14159)
(define radius 10)
(define circumference (* 2 pi radius))

(* pi (* radius radius))

circumference

;; Expressions (s-expressions) Notation
(* (+ 2 (* 4 6)) (+ 3 5 7))

;;               390_______
;;              /  |       \__
;;             *   26         15
;;               / | \     / | \  \
;;             +   2  24  +  3  5  7
;;                   / | \
;;                  *  4  6

;; 1.1.4

(define square
  (lambda (x)
    (* x x)))

(square 2)

(define sum-of-squares
  (lambda (x y)
    (+ (square x) (square y))))

(sum-of-squares 3 4)

;; 1.1.6

(define abs
  (lambda (x)
    (cond ((< x 0) (- x))
          (else x))))

(define abs-with-if
  (lambda (x)
    (if (< x 0)
        (- x)
        x)))

(abs -5)
(abs-with-if -5)

;; 1.1.7

(define sqrt-iter
  (lambda (guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x))))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define sqrt-iter-infinite
  (lambda (guess x)
    (new-if (good-enough? guess x)
            guess
            (sqrt-iter-infinite (improve guess x) x))))

(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(define good-enough?
  (lambda (guess x)
    (< (abs (- (square guess) x)) 0.001)))

(define sqrt
  (lambda (x)
    (sqrt-iter 1.0 x)))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))

;; 1.1.8

(define double
  (lambda (x)
    (+ x x)))

(define square
  (lambda (x)
    (* x x)))

(define square
  ;; Euler's number raised to power `(double (log x))`
  (lambda (x)
    (exp (double (log x)))))

;; Using block structure

(define sqrt
  (lambda (x)

    (define average
      (lambda (x y)
        (/ (+ x y) 2)))

    (define good-enough?
      (lambda (guess x)
        (< (abs (- (square guess) x)) 0.001)))

    (define improve
      (lambda (guess x)
        (average guess (/ x guess))))

    (define sqrt-iter
      (lambda (guess x)
        (if (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x) x))))

    (sqrt-iter 1.0 x)))

(sqrt 4)

;; Using block structure and lexical scoping

(define sqrt
  (lambda (x)

    (define average
      (lambda (x y)
        (/ (+ x y) 2)))

    (define good-enough?
      (lambda (guess)
        (< (abs (- (square guess) x)) 0.001)))

    (define improve
      (lambda (guess)
        (average guess (/ x guess))))

    (define sqrt-iter
      (lambda (guess)
        (if (good-enough? guess)
            guess
            (sqrt-iter (improve guess)))))

    (sqrt-iter 1.0)))

(sqrt 4)

;; 1.2.1

(define factorial-linear-recur
  (lambda (n)
    (if (= n 1)
        1
        (* n (factorial-linear-recur (- n 1))))))

(define factorial-linear-iter
  (lambda (n)
    (letrec ((iter
              (lambda (product counter)
                (if (> counter n)
                    product
                    (iter (* counter product)
                          (+ counter 1))))))
      (iter 1 1))))

(define A
  (lambda (x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1) (A x (- y 1)))))))

;; 1.2.2

(define fib
  (lambda (n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2)))))))


(define fib-iter
  (lambda (a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))


(define fib
  (lambda (n)
    (fib-iter 1 0 n)))

(fib 10)

;; Example: Counting Change

;; 'Suppose we think of the types of coins available as arranged
;; in some order. Then the following relation holds:
;; The number of ways to change amount "a" using "n" kinds of coins
;; equals
;;   * the number of ways to change amount "a" using all but
;;     the first kind of coin, plus
;;   * the number of ways to change amount "a" - "d" using all "n"
;;     kinds of coins, where "d" is the denomination of the first
;;     kind of coin'

;; 'To see why this is true, observe that the ways to make change
;; can be divided into two groups: those that do not use any of the
;; first kind of coin, and those that do. Therefore, the total
;; number of ways to make change for some amount is equal to the
;; numbers of ways to make change for the amount without using
;; any of the first kind of coin, plus the number of ways to make
;; change assuming that we do not use the first kinnd of coin.
;; But the latter number is equal to the number of ways to make
;; change for the amount that remains after using a coin of the first
;; kind.
;; Thus, we can recursively reduce the problem of changing a given
;; amount to the problem of changing smaller amounts using fewer kinds
;; of coins. Consider this reduction rule carefully, and convince
;; yourself that we can use it to describe an algorithm
;; if we specify the following degenerate cases:
;;   * if "a" is exactly 0, we should count that as 1 way to make change
;;   * if "a" is less than 0, we should count that as 0 ways to make change
;;   * if "n" is 0, we should count that as 0 ways to make change
;;

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 100)
